package org.coode.owl.mngr.impl;

import org.apache.log4j.Logger;
import org.coode.owl.mngr.*;
import org.coode.owl.util.MySimpleShortFormProvider;
import org.coode.owl.util.OWLObjectComparator;
import org.semanticweb.owlapi.expression.OWLEntityChecker;
import org.semanticweb.owlapi.expression.ShortFormEntityChecker;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.reasoner.OWLReasonerFactory;
import org.semanticweb.owlapi.reasoner.structural.StructuralReasonerFactory;
import org.semanticweb.owlapi.util.*;
import org.semanticweb.owlapi.vocab.OWLRDFVocabulary;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.*;


/**
 * Author: drummond<br>
 * The University Of Manchester<br>
 * Medical Informatics Group<br>
 * Date: Jul 4, 2006<br><br>
 * <p/>
 * nick.drummond@cs.manchester.ac.uk<br>
 * www.cs.man.ac.uk/~drummond<br><br>
 */
public class OWLServerImpl implements OWLServer {

    private static final Logger logger = Logger.getLogger(OWLServerImpl.class.getName());

    private OWLOntologyManager mngr;

    private OWLOntology activeOntology;

    private OWLReasoner reasoner;
    private Set<OWLReasonerFactory> reasonerFactories = new HashSet<OWLReasonerFactory>();

    private ShortFormProvider shortFormProvider;

    private OntologyIRIShortFormProvider uriShortFormProvider;

    private OWLEntityChecker owlEntityChecker;

    private CachingBidirectionalShortFormProvider nameCache;

    private OWLEntityFinder finder;

    private OWLObjectComparator<OWLObject> comparator;

    private Map<String, OWLClassExpressionParser> parsers = new HashMap<String, OWLClassExpressionParser>();

    private HierarchyProvider<OWLOntology> ontologyHierarchyProvider;
    private HierarchyProvider<OWLClass> classHierarchyProvider;
    private HierarchyProvider<OWLObjectProperty> objectPropertyHierarchyProvider;
    private HierarchyProvider<OWLDataProperty> dataPropertyHierarchyProvider;
    private HierarchyProvider<OWLAnnotationProperty> annotationPropertyHierarchyProvider;
    private HierarchyProvider<OWLDatatype> datatypeHierarchyProvider;
    private HierarchyProvider<OWLObject> individualsHierarchyProvider;

    private Map<URI, OWLOntologyIRIMapper> baseMapper = new HashMap<URI, OWLOntologyIRIMapper>();

    private ServerPropertiesAdapter<ServerProperty> properties;

    private final Set<OWLServerListener> listeners = new HashSet<OWLServerListener>();

    private boolean serverIsDead = false;


    private PropertyChangeListener propertyChangeListener = new PropertyChangeListener(){

        public void propertyChange(PropertyChangeEvent propertyChangeEvent) {
            try{
                ServerProperty p = ServerProperty.valueOf(propertyChangeEvent.getPropertyName());

                handlePropertyChange(p, propertyChangeEvent.getNewValue());
            }
            catch(IllegalArgumentException e){
                // do nothing - a user property
            }
        }
    };

    private OWLOntologyLoaderListener ontLoadListener = new OWLOntologyLoaderListener() {
        public void startedLoadingOntology(LoadingStartedEvent loadingStartedEvent) {
            // do nothing
        }

        public void finishedLoadingOntology(LoadingFinishedEvent loadingFinishedEvent) {
            final OWLOntology ont = getOntologyForIRI(loadingFinishedEvent.getOntologyID().getDefaultDocumentIRI());
            if (ont != null){
                loadedOntology(ont);
            }
        }
    };

    private String[] reasonerFactoryNames = {
            "org.semanticweb.owlapi.reasoner.structural.StructuralReasonerFactory",
            "uk.ac.manchester.cs.factplusplus.owlapiv3.FaCTPlusPlusReasonerFactory",
            "org.semanticweb.HermiT.Reasoner$ReasonerFactory"
            // TODO pellet
    };

    public OWLServerImpl(OWLOntologyManager mngr) {
        this.mngr = mngr;

        loadReasonerFactories();

        mngr.addOntologyLoaderListener(ontLoadListener);

        // always default to trying the URI of the ontology
        mngr.addIRIMapper(new NonMappingOntologyIRIMapper());

        if (!mngr.getOntologies().isEmpty()){
            setActiveOntology(getTopOfImportsTree());
        }
    }

    public void loadOntology(URI physicalURI) throws OWLOntologyCreationException {
        if (!getLocationsMap().containsValue(physicalURI)){

            handleCommonBaseMappers(physicalURI);

            OWLOntology ont = mngr.loadOntologyFromOntologyDocument(IRI.create(physicalURI));

            if (getActiveOntology() == null){
                // the active ontology is always the first one that was requested
                setActiveOntology(ont);
            }
        }
    }

    /**
     * Required because there are currently no listeners on the manager to tell this has happened
     */
    public void removeOntology(OWLOntology ont) {
        if (getActiveOntology().equals(ont)){
            setActiveOntology(null);
        }

        mngr.removeOntology(ont);

        if (getActiveOntology() == null){
            final Set<OWLOntology> ontologies = getOntologies();
            if (!ontologies.isEmpty()){
                setActiveOntology(ontologies.iterator().next());
            }
        }

        clear();
    }

    public void clearOntologies() {
        activeOntology = null;

        if (mngr != null){
            for (OWLOntology ont : mngr.getOntologies()){
                mngr.removeOntology(ont);
            }
        }

        clear();
    }

    public Map<OWLOntologyID, URI> getLocationsMap(){
        Map<OWLOntologyID, URI> ontology2locationMap = new HashMap<OWLOntologyID, URI>();
        final Set<OWLOntology> ontologies = getOntologies();
        for (OWLOntology ont : ontologies){
            ontology2locationMap.put(ont.getOntologyID(), getOWLOntologyManager().getOntologyDocumentIRI(ont).toURI());
            for (OWLImportsDeclaration importsDecl : ont.getImportsDeclarations()){
                final IRI importsIRI = importsDecl.getIRI();
                if (getOntologyForIRI(importsIRI) == null){
                    ontology2locationMap.put(new OWLOntologyID(importsIRI), null);
                }
            }
        }
        return ontology2locationMap;
    }


    public OWLOntology getOntologyForIRI(IRI iri) {
        for (OWLOntology ontology : getOntologies()){
            if (iri.equals(ontology.getOntologyID().getVersionIRI())){
                return ontology;
            }
        }
        for (OWLOntology ontology : getOntologies()){
            if (iri.equals(ontology.getOntologyID().getOntologyIRI())){
                return ontology;
            }
        }
        return null;
    }

    public void addServerListener(OWLServerListener l) {
        listeners.add(l);
    }

    public void removeServerListener(OWLServerListener l) {
        listeners.remove(l);
    }

    public ServerPropertiesAdapter<ServerProperty> getProperties() {
        if (properties == null){

            properties = new ServerPropertiesAdapterImpl<ServerProperty>(new ServerPropertiesImpl());

            properties.setAllowedValues(ServerProperty.optionReasonerEnabled, Arrays.asList(Boolean.TRUE.toString(),
                                                                                            Boolean.FALSE.toString()));
            properties.setBoolean(ServerProperty.optionReasonerEnabled, false);


            // make sure the deprecated names are updated on a load
            properties.addDeprecatedNames(ServerProperty.generateDeprecatedNamesMap());

            List<String> renderers = new ArrayList<String>();
            renderers.add(ServerConstants.RENDERER_FRAG);
            renderers.add(ServerConstants.RENDERER_LABEL);
            properties.setAllowedValues(ServerProperty.optionRenderer, renderers);

            properties.set(ServerProperty.optionRenderer, ServerConstants.RENDERER_FRAG);
            properties.set(ServerProperty.optionLabelUri, OWLRDFVocabulary.RDFS_LABEL.getURI().toString());
            properties.set(ServerProperty.optionLabelLang, "");

            properties.addPropertyChangeListener(propertyChangeListener);
        }
        return properties;
    }


    public OWLOntology getActiveOntology() {
        if (activeOntology == null){
            String ont = getProperties().get(ServerProperty.optionActiveOnt);
            if (ont != null){
                // @@TODO handle anonymous ontologies
                IRI activeOntIRI = IRI.create(ont);
                if (activeOntIRI != null){
                    activeOntology = getOntologyForIRI(activeOntIRI);
                }
            }
        }
        return activeOntology;
    }

    public void setActiveOntology(OWLOntology ont) {
        if (ont == null){
            getProperties().remove(ServerProperty.optionActiveOnt);
            return;
        }

        final OWLOntology activeOnt = getActiveOntology();
        if (activeOnt == null || !activeOnt.equals(ont)){
            // @@TODO handle anonymous ontologies
            getProperties().set(ServerProperty.optionActiveOnt, ont.getOntologyID().getDefaultDocumentIRI().toString());
        }
    }


    public Set<OWLOntology> getOntologies() {
        return mngr.getOntologies();
    }

    public Set<OWLOntology> getActiveOntologies() {
        return getActiveOntology() != null ? mngr.getImportsClosure(getActiveOntology()) : Collections.<OWLOntology>emptySet();
    }

    public OWLOntologyManager getOWLOntologyManager() {
        return mngr;
    }

    public synchronized OWLReasoner getOWLReasoner() {
        if (reasoner == null && !this.isDead()){

            final String selectedReasoner = properties.get(ServerProperty.optionReasoner);

            try {
                logger.debug("Creating reasoner");


                OWLReasonerFactory fac = getReasonerFactory(selectedReasoner);

                OWLReasoner r = fac.createReasoner(getActiveOntology());

                reasoner = new SynchronizedOWLReasoner(r);
            }
            catch (Throwable e) {
                throw new RuntimeException(selectedReasoner + ": " + e.getMessage(), e);
            }
        }
        return reasoner;
    }

    public HierarchyProvider<OWLOntology> getOntologyHierarchyProvider() {
        if (ontologyHierarchyProvider == null && !this.isDead()){
            ontologyHierarchyProvider = new OntologyHierarchyProvider(this);
        }
        return ontologyHierarchyProvider;
    }

    public HierarchyProvider<OWLClass> getClassHierarchyProvider() {
        if (classHierarchyProvider == null && !this.isDead()){
            classHierarchyProvider = new ClassHierarchyProvider(this);
        }
        return classHierarchyProvider;
    }

    public HierarchyProvider<OWLObjectProperty> getOWLObjectPropertyHierarchyProvider() {
        if (objectPropertyHierarchyProvider == null && !this.isDead()){
            objectPropertyHierarchyProvider = new OWLObjectPropertyHierarchyProvider(this);
        }
        return objectPropertyHierarchyProvider;
    }

    public HierarchyProvider<OWLDataProperty> getOWLDataPropertyHierarchyProvider() {
        if (dataPropertyHierarchyProvider == null && !this.isDead()){
            dataPropertyHierarchyProvider = new OWLDataPropertyHierarchyProvider(this);
        }
        return dataPropertyHierarchyProvider;
    }

    public HierarchyProvider<OWLAnnotationProperty> getOWLAnnotationPropertyHierarchyProvider() {
        if (annotationPropertyHierarchyProvider == null && !this.isDead()){
            annotationPropertyHierarchyProvider = new OWLAnnotationPropertyHierarchyProvider(this);
        }
        return annotationPropertyHierarchyProvider;
    }

    public HierarchyProvider<OWLObject> getOWLIndividualsHierarchyProvider() {
        if (individualsHierarchyProvider == null && !this.isDead()){
            individualsHierarchyProvider = new OWLIndividualByClassHierarchyProvider(this);
        }
        return individualsHierarchyProvider;
    }

    public HierarchyProvider<OWLDatatype> getOWLDatatypeHierarchyProvider() {
        if (datatypeHierarchyProvider == null && !this.isDead()){
            datatypeHierarchyProvider = new OWLDatatypeHierarchyProvider(this);
        }
        return datatypeHierarchyProvider;
    }

    public Comparator<OWLObject> getComparator() {
        if (comparator == null && !this.isDead()){
            comparator = new OWLObjectComparator<OWLObject>(this);
        }
        return comparator;
    }

    public OWLEntityFinder getFinder() {
        if (finder == null && !this.isDead()){
            finder = new OWLEntityFinderImpl(getNameCache(), this);
        }
        return finder;
    }


    public OWLEntityChecker getOWLEntityChecker() {
        if (owlEntityChecker == null && !this.isDead()){
            owlEntityChecker = new ShortFormEntityChecker(getNameCache());
        }
        return owlEntityChecker;
    }

    public ShortFormProvider getShortFormProvider() {
        if (shortFormProvider == null && !this.isDead()){
            String ren = properties.get(ServerProperty.optionRenderer);
            if (ren.equals(ServerConstants.RENDERER_FRAG)){
                shortFormProvider = new MySimpleShortFormProvider();
            }
            else if (ren.equals(ServerConstants.RENDERER_LABEL)){
                OWLAnnotationProperty prop = mngr.getOWLDataFactory().getOWLAnnotationProperty(IRI.create(properties.get(ServerProperty.optionLabelUri)));

                final OWLOntologySetProvider activeOntologiesSetProvider = new OWLOntologySetProvider() {
                    public Set<OWLOntology> getOntologies() {
                        return getActiveOntologies();
                    }
                };

                String lang = properties.get(ServerProperty.optionLabelLang);

                final Map<OWLAnnotationProperty, List<String>> langMap;
                if (lang.length() == 0){
                    langMap = Collections.emptyMap();
                }
                else{
                    langMap = Collections.singletonMap(prop, Collections.singletonList(lang));
                }

                shortFormProvider = new AnnotationValueShortFormProvider(Collections.singletonList(prop),
                                                                         langMap,
                                                                         activeOntologiesSetProvider,
                                                                         new MySimpleShortFormProvider());
            }
        }
        return shortFormProvider;
    }


    public OntologyIRIShortFormProvider getOntologyShortFormProvider() {
        if (uriShortFormProvider == null){
            uriShortFormProvider = new OntologyIRIShortFormProvider();
        }
        return uriShortFormProvider;
    }


    public final OWLClassExpressionParser getClassExpressionParser(String type){
        if (!this.isDead()){
            return parsers.get(type);
        }
        return null;
    }

    public final void registerDescriptionParser(String syntax, OWLClassExpressionParser parser) {
        parsers.put(syntax, parser);
    }

    public Set<String> getSupportedSyntaxes() {
        return parsers.keySet();
    }


    public void dispose() {

        clearOntologies();

        mngr = null;

        parsers.clear();

        properties.removePropertyChangeListener(propertyChangeListener);

        serverIsDead = true;
    }


    public boolean isDead() {
        return serverIsDead;
    }


    public void clear() {
        resetReasoner();
        resetRendererCache();
        resetHierarchies();
        comparator = null;
    }


    private void resetReasoner() {
        if (reasoner != null){
            reasoner.dispose();
            reasoner = null;
        }
    }

    private void resetHierarchies() {
        ontologyHierarchyProvider = null;
        classHierarchyProvider = null;
        objectPropertyHierarchyProvider = null;
        dataPropertyHierarchyProvider = null;
        individualsHierarchyProvider = null;
        datatypeHierarchyProvider = null;
    }


    private void resetRendererCache() {
        if (shortFormProvider != null){
            shortFormProvider.dispose();
            shortFormProvider = null;
        }
        if (finder != null){
            finder.dispose();
            finder = null;
        }
        if (nameCache != null){
            nameCache.dispose();
            nameCache = null;
        }
    }

    private void resetAllowedLabels() {
        Set<String> uriStrings = new HashSet<String>();
        Set<OWLAnnotationProperty> annotationProps = new HashSet<OWLAnnotationProperty>();
        for (OWLOntology ont : getActiveOntologies()){
            annotationProps.addAll(ont.getAnnotationPropertiesInSignature());
        }
        for (OWLAnnotationProperty prop : annotationProps){
            uriStrings.add(prop.getIRI().toString());
        }
        getProperties().setAllowedValues(ServerProperty.optionLabelUri, new ArrayList<String>(uriStrings));
    }

    private CachingBidirectionalShortFormProvider getNameCache(){
        if (nameCache == null){
            nameCache = new CachingBidirectionalShortFormProvider(){
                protected String generateShortForm(OWLEntity owlEntity) {
                    return getShortFormProvider().getShortForm(owlEntity);
                }
            };
            nameCache.rebuild(new ReferencedEntitySetProvider(getActiveOntologies()));
        }
        return nameCache;
    }

    private void loadedOntology(OWLOntology ont) {
        List<String> labelURIs = getProperties().getAllowedValues(ServerProperty.optionLabelUri);
        for (OWLAnnotationProperty ap : ont.getAnnotationPropertiesInSignature()){
            labelURIs.add(ap.getIRI().toString());
        }
        getProperties().setAllowedValues(ServerProperty.optionLabelUri, labelURIs);

        List<String> ontologies = new ArrayList<String>();
        for (OWLOntology o : getOntologies()){
            ontologies.add(o.getOntologyID().getDefaultDocumentIRI().toString());
        }
        getProperties().setAllowedValues(ServerProperty.optionActiveOnt, ontologies);
    }


    private OWLOntology getTopOfImportsTree() {
        return getOntologies().iterator().next();
//        return getOntologyHierarchyProvider().getRoots().iterator().next();
    }

    // create a set of CommonBaseURIMappers for finding ontologies
    // using the base of explicitly loaded ontologies as a hint
    private void handleCommonBaseMappers(URI physicalURI) {
        String baseURIStr = "";
        String uriParts[] = physicalURI.toString().split("/");
        for (int i=0; i<uriParts.length-1; i++){
            baseURIStr += uriParts[i] + "/";
        }
        URI baseURI = URI.create(baseURIStr);

        if (baseURI != null){
            if (baseMapper.get(baseURI) == null){
                final BaseURIMapper mapper = new BaseURIMapper(baseURI);
                baseMapper.put(baseURI, mapper);
                mngr.addIRIMapper(mapper);
            }
        }
    }

    private OWLReasonerFactory getReasonerFactory(String name) {
        for (OWLReasonerFactory fac : reasonerFactories){
            if (fac.getReasonerName().equals(name)){
                return fac;
            }
        }

        logger.warn("Couldn't find a reasoner factory for " + name + ". Using structural reasoner.");
        return new StructuralReasonerFactory(); //
    }

    // TODO: error handling should be better
    private void loadReasonerFactories() {
        List<String> reasonerNames = new ArrayList<String>();
        for (String reasonerFactoryName : reasonerFactoryNames){
            try {
                final OWLReasonerFactory fac = (OWLReasonerFactory) Class.forName(reasonerFactoryName).newInstance();
                reasonerNames.add(fac.getReasonerName());
                reasonerFactories.add(fac);
            }
            catch (InstantiationException e) {
                e.printStackTrace();
            }
            catch (IllegalAccessException e) {
                e.printStackTrace();
            }
            catch (ClassNotFoundException e) {
                e.printStackTrace();
            }
        }
        getProperties().setAllowedValues(ServerProperty.optionReasoner, reasonerNames);
    }

    private void handlePropertyChange(ServerProperty p, Object newValue) {

        switch(p){
            case optionReasoner:
                resetReasoner();
                break;
            case optionRenderer:     // DROPTHROUGH
            case optionLabelLang:
                resetRendererCache();
                break;
            case optionLabelUri:
                try {
                    new URI((String)newValue);
                    resetRendererCache();
                }
                catch (URISyntaxException e) {
                    // invalid URI - do not change the renderer
                }
                break;
            case optionActiveOnt:
                activeOntology = null; // this will force it to be taken from the properties

                clear();

                resetAllowedLabels();

                OWLOntology ont = getActiveOntology();

                for (OWLServerListener l : listeners){
                    l.activeOntologyChanged(ont);
                }
                break;
        }
    }
}
