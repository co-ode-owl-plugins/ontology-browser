package org.coode.owl.mngr.impl;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.lang.reflect.Constructor;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.log4j.Logger;
import org.coode.owl.mngr.HierarchyProvider;
import org.coode.owl.mngr.OWLClassExpressionParser;
import org.coode.owl.mngr.OWLEntityFinder;
import org.coode.owl.mngr.OWLServer;
import org.coode.owl.mngr.OWLServerListener;
import org.coode.owl.mngr.ServerConstants;
import org.coode.owl.mngr.ServerPropertiesAdapter;
import org.coode.owl.mngr.ServerProperty;
import org.coode.owl.util.OWLObjectComparator;
import org.semanticweb.owlapi.expression.OWLEntityChecker;
import org.semanticweb.owlapi.expression.ShortFormEntityChecker;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLAnnotationProperty;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyIRIMapper;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.model.OWLOntologySetProvider;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.reasoner.OWLReasonerException;
import org.semanticweb.owlapi.util.AnnotationValueShortFormProvider;
import org.semanticweb.owlapi.util.CachingBidirectionalShortFormProvider;
import org.semanticweb.owlapi.util.NonMappingOntologyIRIMapper;
import org.semanticweb.owlapi.util.OntologyIRIShortFormProvider;
import org.semanticweb.owlapi.util.ReferencedEntitySetProvider;
import org.semanticweb.owlapi.util.ShortFormProvider;
import org.semanticweb.owlapi.util.SimpleShortFormProvider;
import org.semanticweb.owlapi.vocab.OWLRDFVocabulary;


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

    private ShortFormProvider shortFormProvider;

    private OntologyIRIShortFormProvider uriShortFormProvider;

    private OWLEntityChecker owlEntityChecker;

    private CachingBidirectionalShortFormProvider nameCache;

    private OWLEntityFinder finder;

    private OWLObjectComparator<OWLObject> comparator;

    private Map<String, OWLClassExpressionParser> parsers = new HashMap<String, OWLClassExpressionParser>();

    private HierarchyProvider<OWLClass> classHierarchyProvider;
    private HierarchyProvider<OWLObjectProperty> objectPropertyHierarchyProvider;
    private HierarchyProvider<OWLDataProperty> dataPropertyHierarchyProvider;

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


    public OWLServerImpl(OWLOntologyManager mngr) {
        this.mngr = mngr;

        // always default to trying the URI of the ontology
        mngr.addIRIMapper(new NonMappingOntologyIRIMapper());

        if (!mngr.getOntologies().isEmpty()){
            setActiveOntology(getTopOfImportsTree(mngr.getOntologies()));
        }
    }



    private OWLOntology getTopOfImportsTree(Set<OWLOntology> ontologies) {
        // @@TODO implement
        return ontologies.iterator().next();
    }

    public void loadOntology(URI physicalURI) throws OWLOntologyCreationException {

        handleCommonBaseMappers(physicalURI);

        OWLOntology ont = mngr.loadOntologyFromOntologyDocument(IRI.create(physicalURI));

        if (getActiveOntology() == null){
            setActiveOntology(ont); // the active ontology is always the first that was loaded
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
            final Set<OWLOntology> activeOntologies = getActiveOntologies();
            if (!activeOntologies.isEmpty()){
                setActiveOntology(activeOntologies.iterator().next());
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

    public void addServerListener(OWLServerListener l) {
        listeners.add(l);
    }

    public void removeServerListener(OWLServerListener l) {
        listeners.remove(l);
    }

    public ServerPropertiesAdapter<ServerProperty> getProperties() {
        if (properties == null){

            properties = new ServerPropertiesAdapterImpl<ServerProperty>(new ServerPropertiesImpl());

            // make sure the deprecated names are updated on a load
            properties.addDeprecatedNames(ServerProperty.generateDeprecatedNamesMap());

            List<String> renderers = new ArrayList<String>();
            renderers.add(ServerConstants.RENDERER_FRAG);
            renderers.add(ServerConstants.RENDERER_LABEL);
            properties.setAllowedValues(ServerProperty.optionRenderer, renderers);

            properties.set(ServerProperty.optionRenderer, ServerConstants.RENDERER_FRAG);
            properties.set(ServerProperty.optionLabelUri, OWLRDFVocabulary.RDFS_LABEL.getURI().toString());
            properties.set(ServerProperty.optionReasoner, ServerConstants.PELLET);
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
                    activeOntology = mngr.getOntology(activeOntIRI);
                }
            }
        }
        return activeOntology;
    }

    public void setActiveOntology(OWLOntology ont) {
        if (getActiveOntology() == null || !getActiveOntology().equals(ont)){
            activeOntology = ont;

            if (ont != null){
                // @@TODO handle anonymous ontologies
                getProperties().set(ServerProperty.optionActiveOnt, ont.getOntologyID().getOntologyIRI().toString());
            }
            else{
                getProperties().remove(ServerProperty.optionActiveOnt);
            }

            clear();

            resetAllowedLabels();

            for (OWLServerListener l : listeners){
                l.activeOntologyChanged(ont);
            }
        }
    }


    public Set<OWLOntology> getOntologies() {
        return mngr.getOntologies();
    }

    public Set<OWLOntology> getActiveOntologies() {
        return mngr.getImportsClosure(getActiveOntology());
    }

    public OWLOntologyManager getOWLOntologyManager() {
        return mngr;
    }

    public synchronized OWLReasoner getOWLReasoner() {
        if (reasoner == null && !this.isDead()){
            try {
                logger.debug("Creating reasoner");
                OWLReasoner reasoner = null;

                final String selectedReasoner = properties.get(ServerProperty.optionReasoner);

                /*********************
                 * TODO -- This code is broken...
                 */
                if (ServerConstants.PELLET.equals(selectedReasoner)){
                    logger.debug("  pellet");
                    Class cls = Class.forName("org.mindswap.pellet.owlapi.Reasoner");
                    Constructor constructor = cls.getConstructor(OWLOntologyManager.class);
                    reasoner = (OWLReasoner) constructor.newInstance(mngr);
                }
                else if (ServerConstants.FACTPLUSPLUS.equals(selectedReasoner)){
                    logger.debug("  FaCT++");
                    Class cls = Class.forName("uk.ac.manchester.cs.factplusplus.owlapi.Reasoner");
                    Constructor constructor = cls.getConstructor(OWLOntologyManager.class);
                    reasoner = (OWLReasoner) constructor.newInstance(mngr);
                }
                else if (ServerConstants.DIG.equals(selectedReasoner)){
                    throw new RuntimeException("DIG not supported");
//                    logger.debug("  DIG");
//                    DIGReasonerPreferences.getInstance().setReasonerURL(new URL(properties.get(ServerProperty.optionReasonerUrl)));
//                    reasoner = new DIGReasoner(mngr);
                }

                if (reasoner != null){
                    this.reasoner = new SynchronizedOWLReasoner(reasoner);
                    this.reasoner.prepareReasoner();
                }
            }
            catch (Throwable e) {
                logger.error("Error trying to get reasoner", e);
            }
        }
        return reasoner;
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


    public ShortFormProvider getShortFormProvider() {
        if (shortFormProvider == null && !this.isDead()){
            String ren = properties.get(ServerProperty.optionRenderer);
            if (ren.equals(ServerConstants.RENDERER_FRAG)){
                shortFormProvider = new SimpleShortFormProvider();
            }
            else if (ren.equals(ServerConstants.RENDERER_LABEL)){
                String lang = properties.get(ServerProperty.optionLabelLang);
                if ("".equals(lang)){
                    lang = null;
                }
                OWLAnnotationProperty prop = mngr.getOWLDataFactory().getOWLAnnotationProperty(IRI.create(properties.get(ServerProperty.optionLabelUri)));
                Map<OWLAnnotationProperty, List<String>> annot2LangMap = new HashMap<OWLAnnotationProperty, List<String>>();
                annot2LangMap.put(prop, Collections.singletonList(lang));
                shortFormProvider = new AnnotationValueShortFormProvider(Collections.singletonList(prop),
                                                                         annot2LangMap,
                                                                         new OWLOntologySetProvider(){
                                                                             public Set<OWLOntology> getOntologies() {
                                                                                 return getActiveOntologies();
                                                                             }
                                                                         });
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
        comparator = null;
    }


    private void resetReasoner() {
        if (reasoner != null){
            reasoner.dispose();
            reasoner = null;
        }
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
        }
    }

    private void resetAllowedLabels() {
        List<String> uriStrings = new ArrayList<String>();
        Set<OWLAnnotationProperty> annotationProps = new HashSet<OWLAnnotationProperty>();
        for (OWLOntology ont : getActiveOntologies()){
            annotationProps.addAll(ont.getAnnotationPropertiesInSignature());
        }
        for (OWLAnnotationProperty prop : annotationProps){
            uriStrings.add(prop.getIRI().toString());
        }
        getProperties().setAllowedValues(ServerProperty.optionLabelUri, uriStrings);
    }
}
