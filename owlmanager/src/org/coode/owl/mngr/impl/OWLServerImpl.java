package org.coode.owl.mngr.impl;

import org.apache.log4j.Logger;
import org.coode.owl.mngr.*;
import org.coode.owl.util.ModelUtil;
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

    private Map<Class<? extends OWLObject>, HierarchyProvider> hps = new HashMap<Class<? extends OWLObject>, HierarchyProvider>();

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
            if (loadingFinishedEvent.isSuccessful()){
                OWLOntologyID id = loadingFinishedEvent.getOntologyID();
                OWLOntology ont = mngr.getOntology(id);
                if (ont == null){
                    ont = getOntologyForIRI(id.getDefaultDocumentIRI());
                }
                logger.info("loaded " + ModelUtil.getOntologyIdString(ont));
                loadedOntology(ont);
            }
        }
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

    public OWLOntology loadOntology(URI physicalURI) throws OWLOntologyCreationException {
        for (OWLOntologyID id : getLocationsMap().keySet()){
            if (physicalURI.equals(getLocationsMap().get(id))){
                return getOntologyForIRI(id.getDefaultDocumentIRI());
            }
        }

        handleCommonBaseMappers(physicalURI);

        OWLOntology ont = mngr.loadOntologyFromOntologyDocument(IRI.create(physicalURI));

//        if (getActiveOntology() == null){
        // the active ontology is always the first one that was requested
        setActiveOntology(ont);
//        }
        return ont;
    }


    public void loadOntologies(final Map<IRI, IRI> ontMap) {
        OWLOntologyIRIMapper mapper = new OWLOntologyIRIMapper(){
            public IRI getDocumentIRI(IRI ontologyIRI) {
                return ontMap.get(ontologyIRI);
            }
        };
        mngr.addIRIMapper(mapper);

        for (IRI iri : ontMap.keySet()){
            try {
                mngr.loadOntology(iri);
            }
            catch (OWLOntologyAlreadyExistsException e){
                // do nothing - as we're not trying to load in order just keep going
            }
            catch (OWLOntologyCreationException e) {
                e.printStackTrace();
            }
        }

        mngr.removeIRIMapper(mapper);
    }


    public OWLOntology reloadOntology(OWLOntology ontology) throws OWLOntologyCreationException {
        URI physicalLocation = getOWLOntologyManager().getOntologyDocumentIRI(ontology).toURI();

        mngr.removeOntology(ontology);

        OWLOntology ont = mngr.loadOntologyFromOntologyDocument(IRI.create(physicalLocation));

        clear();

        return ont;
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

        // look for an ontology with this location
        for (OWLOntology ontology : getOntologies()){
            if (iri.equals(getOWLOntologyManager().getOntologyDocumentIRI(ontology))){
                return ontology;
            }
        }
        return getAnonymousOntology(iri.toString());
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

            properties.setBoolean(ServerProperty.optionReasonerEnabled, false);
            properties.setAllowedValues(ServerProperty.optionReasonerEnabled, Arrays.asList(Boolean.TRUE.toString(),
                                                                                            Boolean.FALSE.toString()));

            // make sure the deprecated names are updated on a load
            properties.addDeprecatedNames(ServerProperty.generateDeprecatedNamesMap());

            properties.set(ServerProperty.optionRenderer, ServerConstants.RENDERER_LABEL);
            properties.setAllowedValues(ServerProperty.optionRenderer, Arrays.asList(ServerConstants.RENDERER_FRAG,
                                                                                     ServerConstants.RENDERER_LABEL));

            properties.set(ServerProperty.optionLabelUri, OWLRDFVocabulary.RDFS_LABEL.getIRI().toString());
            properties.set(ServerProperty.optionLabelLang, "");

            properties.set(ServerProperty.optionLabelPropertyUri, ServerConstants.FOAF_NAME);

            properties.set(ServerProperty.optionShowOntologies, ServerConstants.ALL_ONTOLOGIES);
            properties.setAllowedValues(ServerProperty.optionShowOntologies, Arrays.asList(ServerConstants.IMPORTS_CLOSURE,
                                                                                           ServerConstants.ALL_ONTOLOGIES,
                                                                                           ServerConstants.ACTIVE_ONTOLOGY));

            properties.addPropertyChangeListener(propertyChangeListener);
        }
        return properties;
    }


    public OWLOntology getActiveOntology() {
        if (activeOntology == null){
            String ont = getProperties().get(ServerProperty.optionActiveOnt);
            if (ont != null){
                IRI activeOntIRI = IRI.create(ont);
                if (activeOntIRI != null){
                    activeOntology = getOntologyForIRI(activeOntIRI);
                }
            }
        }
        return activeOntology;
    }

    private OWLOntology getAnonymousOntology(String id) {
        for (OWLOntology ontology : getOntologies()){
            if (id.equals(ontology.getOntologyID().toString())){
                return ontology;
            }
        }
        return null;
    }

    public void setActiveOntology(OWLOntology ont) {
        if (ont == null){
            getProperties().remove(ServerProperty.optionActiveOnt);
            return;
        }

        final OWLOntology activeOnt = getActiveOntology();
        if (activeOnt == null || !activeOnt.equals(ont)){
            getProperties().set(ServerProperty.optionActiveOnt, ModelUtil.getOntologyIdString(ont));
        }
    }

    public Set<OWLOntology> getOntologies() {
        return mngr.getOntologies();
    }

    public Set<OWLOntology> getActiveOntologies() {

        final String show = getProperties().get(ServerProperty.optionShowOntologies);
        if (show == null || show.equals(ServerConstants.IMPORTS_CLOSURE)){
            final OWLOntology activeOnt = getActiveOntology();
            if (activeOnt == null){
                return Collections.emptySet();
            }
            return mngr.getImportsClosure(activeOnt);
        }
        else if (show.equals(ServerConstants.ALL_ONTOLOGIES)){
            return mngr.getOntologies();
        }
        else if (show.equals(ServerConstants.ACTIVE_ONTOLOGY)){
            final OWLOntology activeOnt = getActiveOntology();
            if (activeOnt != null){
                return Collections.singleton(activeOnt);
            }
        }
        return Collections.emptySet();
    }

    public OWLOntologyManager getOWLOntologyManager() {
        return mngr;
    }

    public synchronized OWLReasoner getOWLReasoner() {
        if (isDead()){
            throw new RuntimeException("Cannot getOWLReasoner - server is dead");
        }
        if (reasoner == null){

            final String selectedReasoner = getProperties().get(ServerProperty.optionReasoner);

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

    @SuppressWarnings("unchecked")
    public <N extends OWLObject> HierarchyProvider<N> getHierarchyProvider(Class<N> cls) {
        HierarchyProvider<N> hp = (HierarchyProvider<N>)hps.get(cls);
        if (hp == null){
            if (OWLClass.class.isAssignableFrom(cls)){
                hp = (HierarchyProvider<N>)new ClassHierarchyProvider(this);
            }
            else if (OWLObjectProperty.class.isAssignableFrom(cls)){
                hp = (HierarchyProvider<N>)new OWLObjectPropertyHierarchyProvider(this);
            }
            else if (OWLDataProperty.class.isAssignableFrom(cls)){
                hp = (HierarchyProvider<N>)new OWLDataPropertyHierarchyProvider(this);
            }
            else if (OWLAnnotationProperty.class.isAssignableFrom(cls)){
                hp = (HierarchyProvider<N>)new OWLAnnotationPropertyHierarchyProvider(this);
            }
            else if (OWLNamedIndividual.class.isAssignableFrom(cls)){
                hp = (HierarchyProvider<N>)new OWLIndividualByClassHierarchyProvider(this);
            }
            else if (OWLDatatype.class.isAssignableFrom(cls)){
                hp = (HierarchyProvider<N>)new OWLDatatypeHierarchyProvider(this);
            }
            else if (OWLOntology.class.isAssignableFrom(cls)){
                hp = (HierarchyProvider<N>)new OntologyHierarchyProvider(this);
            }
        }
        return hp;
    }

    public Comparator<OWLObject> getComparator() {
        if (isDead()){
            throw new RuntimeException("Cannot getComparator - server is dead");
        }
        if (comparator == null){
            comparator = new OWLObjectComparator<OWLObject>(this);
        }
        return comparator;
    }

    public OWLEntityFinder getFinder() {
        if (isDead()){
            throw new RuntimeException("Cannot getFinder - server is dead");
        }

        if (finder == null){
            finder = new OWLEntityFinderImpl(getNameCache(), this);
        }
        return finder;
    }


    public OWLEntityChecker getOWLEntityChecker() {
        if (isDead()){
            throw new RuntimeException("Cannot getOWLEntityChecker - server is dead");
        }
        if (owlEntityChecker == null){
            owlEntityChecker = new ShortFormEntityChecker(getNameCache());
        }
        return owlEntityChecker;
    }

    public ShortFormProvider getShortFormProvider() {
        if (isDead()){
            throw new RuntimeException("Cannot getShortFormProvider - server is dead");
        }

        if (shortFormProvider == null){
            String ren = getProperties().get(ServerProperty.optionRenderer);
            if (ren.equals(ServerConstants.RENDERER_FRAG)){
                shortFormProvider = new MySimpleShortFormProvider();
            }
            else if (ren.equals(ServerConstants.RENDERER_LABEL)){

                final OWLOntologySetProvider activeOntologiesSetProvider = new OWLOntologySetProvider() {
                    public Set<OWLOntology> getOntologies() {
                        return getActiveOntologies();
                    }
                };

                String lang = getProperties().get(ServerProperty.optionLabelLang);

                // the property assertion sfp
                OWLDataProperty dataProp = mngr.getOWLDataFactory().getOWLDataProperty(
                        IRI.create(getProperties().get(ServerProperty.optionLabelPropertyUri)));
                List<OWLPropertyExpression> props = new ArrayList<OWLPropertyExpression>();
                props.add(dataProp);

                final Map<OWLDataPropertyExpression, List<String>> lMap;
                lMap = new HashMap<OWLDataPropertyExpression, List<String>>();
                if (lang.length() > 0){
                    lMap.put(dataProp, Collections.singletonList(lang));
                }
                ShortFormProvider pValueProvider = new PropertyAssertionValueShortFormProvider(props,
                                                                                               lMap,
                                                                                               activeOntologiesSetProvider,
                                                                                               new MySimpleShortFormProvider());

                // the annotation label sfp
                OWLAnnotationProperty annotProp = mngr.getOWLDataFactory().getOWLAnnotationProperty(
                        IRI.create(getProperties().get(ServerProperty.optionLabelUri)));
                final Map<OWLAnnotationProperty, List<String>> langMap = new HashMap<OWLAnnotationProperty, List<String>>();
                if (lang.length() > 0){
                    langMap.put(annotProp, Collections.singletonList(lang));
                }
                shortFormProvider = new AnnotationValueShortFormProvider(Collections.singletonList(annotProp),
                                                                         langMap,
                                                                         activeOntologiesSetProvider,
                                                                         pValueProvider);
            }
        }
        return shortFormProvider;
    }


    public OntologyIRIShortFormProvider getOntologyShortFormProvider() {
        if (uriShortFormProvider == null){
            uriShortFormProvider = new OntologyIRIShortFormProvider(){
                @Override
                public String getShortForm(OWLOntology ont) {
                    if (ont.isAnonymous()){
                        return getOWLOntologyManager().getOntologyDocumentIRI(ont).toString();
                    }
                    return super.getShortForm(ont);
                }
            };
        }
        return uriShortFormProvider;
    }


    public final OWLClassExpressionParser getClassExpressionParser(String type){
        if (isDead()){
            throw new RuntimeException("Cannot getClassExpressionParser - server is dead");
        }

        return parsers.get(type);
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

        if (properties != null){
            properties.removePropertyChangeListener(propertyChangeListener);
            properties = null;
        }

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
        hps.clear();
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
        for (OWLOntology ont : getActiveOntologies()){
            for (OWLAnnotationProperty p : ont.getAnnotationPropertiesInSignature()){
                uriStrings.add(p.getIRI().toString());
            }
        }
        getProperties().setAllowedValues(ServerProperty.optionLabelUri, new ArrayList<String>(uriStrings));

        Set<String> dataPropStrings = new HashSet<String>();
        for (OWLOntology ont : getActiveOntologies()){
            for (OWLDataProperty p : ont.getDataPropertiesInSignature()){
                dataPropStrings.add(p.getIRI().toString());
            }
        }
        getProperties().setAllowedValues(ServerProperty.optionLabelPropertyUri, new ArrayList<String>(dataPropStrings));
    }

    private CachingBidirectionalShortFormProvider getNameCache(){
        if (isDead()){
            throw new RuntimeException("Cannot getNameCache - server is dead");
        }
        if (nameCache == null){
            nameCache = new CachingBidirectionalShortFormProvider(){
                protected String generateShortForm(OWLEntity owlEntity) {
                    return getShortFormProvider().getShortForm(owlEntity);
                }
            };
            // TODO: should names also include all standard xsd datatypes - not just the ones referenced?
            nameCache.rebuild(new ReferencedEntitySetProvider(getActiveOntologies()));
        }
        return nameCache;
    }

    private void loadedOntology(OWLOntology ont) {
        resetAllowedLabels();

        List<String> ontologies = new ArrayList<String>();
        for (OWLOntology ontology : getOntologies()){
            ontologies.add(ModelUtil.getOntologyIdString(ontology));
        }
        getProperties().setAllowedValues(ServerProperty.optionActiveOnt, ontologies);
    }


    private OWLOntology getTopOfImportsTree() {
        return getOntologies().iterator().next(); // TODO: fix
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


    private String[] reasonerFactoryNames = {
            "org.semanticweb.owlapi.reasoner.structural.StructuralReasonerFactory",
//            "uk.ac.manchester.cs.factplusplus.owlapiv3.FaCTPlusPlusReasonerFactory",
            "org.semanticweb.HermiT.Reasoner$ReasonerFactory"
            // TODO pellet, etc
    };

    // TODO: error handling should be better
    private void loadReasonerFactories() {

        String selectedReasoner = null;

        List<String> reasonerNames = new ArrayList<String>();
        for (String reasonerFactoryName : reasonerFactoryNames){
            try {
                final OWLReasonerFactory fac = (OWLReasonerFactory) Class.forName(reasonerFactoryName).newInstance();
                reasonerNames.add(fac.getReasonerName());
                reasonerFactories.add(fac);

                // set the first reasoner factory as default
                if (selectedReasoner == null){
                    selectedReasoner = fac.getReasonerName();
                    getProperties().set(ServerProperty.optionReasoner, selectedReasoner);
                }
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
            case optionLabelUri:     // DROPTHROUGH
            case optionLabelPropertyUri:
                try {
                    new URI((String)newValue);
                    resetRendererCache();
                }
                catch (URISyntaxException e) {
                    // invalid URI - do not change the renderer
                }
                break;
            case optionActiveOnt:    // DROPTHROUGH
            case optionShowOntologies:
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
