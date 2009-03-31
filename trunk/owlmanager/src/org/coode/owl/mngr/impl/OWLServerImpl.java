package org.coode.owl.mngr.impl;

import org.apache.log4j.Logger;
import org.coode.owl.mngr.*;
import org.coode.owl.util.OWLObjectComparator;
import org.semanticweb.owl.inference.OWLReasoner;
import org.semanticweb.owl.inference.OWLReasonerException;
import org.semanticweb.owl.model.*;
import org.semanticweb.owl.util.NonMappingOntologyURIMapper;
import org.semanticweb.owl.util.ToldClassHierarchyReasoner;
import org.semanticweb.owl.vocab.OWLRDFVocabulary;
import uk.ac.manchester.cs.owl.inference.dig11.DIGReasoner;
import uk.ac.manchester.cs.owl.inference.dig11.DIGReasonerPreferences;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.lang.reflect.Constructor;
import java.net.URI;
import java.net.URL;
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

    private OWLReasoner reasoner;

    private NamedObjectShortFormProvider renderer;

    private OWLObjectComparator<OWLObject> comparator;

    private OWLNamedObjectFinder finder;

    private Map<String, OWLDescriptionParser> parsers = new HashMap<String, OWLDescriptionParser>();

    private ToldClassHierarchyReasoner toldClassHierarchyReasoner;

    private ToldPropertyHierarchyReasoner toldPropertyHierarchyReasoner;

    private Map<URI, OWLOntologyURIMapper> baseMapper = new HashMap<URI, OWLOntologyURIMapper>();

    private final ServerProperties properties;

    private final Set<OWLServerListener> listeners = new HashSet<OWLServerListener>();

    private boolean serverIsDead = false;

    private String id;

    private OWLOntology activeOntology;

    private OWLOntology topOntology;

    private PropertyChangeListener propertyChangeListener = new PropertyChangeListener(){

        public void propertyChange(PropertyChangeEvent propertyChangeEvent) {
            String property = propertyChangeEvent.getPropertyName();

            if (ServerConstants.OPTION_REASONER.equals(property) && reasoner != null){
                try {
                    reasoner.dispose();
                }
                catch (OWLReasonerException e) {
                    logger.error("Failed to dispose of current reasoner: " + reasoner.getClass(), e);
                }
                reasoner = null;
            }
            else if (ServerConstants.OPTION_REN.equals(property) ||
                     ServerConstants.OPTION_LABEL_LANG.equals(property)){
                resetRendererCache();
            }
            else if (ServerConstants.OPTION_LABEL_URI.equals(property)){
                try {
                    new URI((String)propertyChangeEvent.getNewValue());
                    resetRendererCache();
                }
                catch (URISyntaxException e) {
                    // invalid URI - do not change the renderer
                }
            }
        }
    };


    private void resetRendererCache() {
        if (renderer != null){
            renderer.dispose();
            renderer = null;
        }
        if (finder != null){
            finder.dispose();
            finder = null;
        }
    }


    public OWLServerImpl(String id, OWLOntologyManager mngr) {
        this.id = id;
        this.mngr = mngr;

        // always default to trying the URI of the ontology
        mngr.addURIMapper(new NonMappingOntologyURIMapper());

        this.properties = new ServerPropertiesImpl();

        properties.set(ServerConstants.OPTION_REN, ServerConstants.RENDERER_FRAG);
        properties.set(ServerConstants.OPTION_RENDER_SUBS, ServerConstants.TRUE);
        properties.set(ServerConstants.OPTION_LABEL_URI, OWLRDFVocabulary.RDFS_LABEL.getURI().toString());

        properties.addPropertyChangeListener(propertyChangeListener);

        if (!mngr.getOntologies().isEmpty()){
            setActiveOntology(getTopOfImportsTree(mngr.getOntologies()));
        }
    }

    private OWLOntology getTopOfImportsTree(Set<OWLOntology> ontologies) {
        // @@TODO implement
        return ontologies.iterator().next();
    }

    public void loadOntology(URI physicalURI) throws OWLOntologyCreationException {

        Set<OWLOntology> existingLoadedOntologies = mngr.getOntologies();

        handleCommonBaseMappers(physicalURI);

        OWLOntology ont = mngr.loadOntologyFromPhysicalURI(physicalURI);

        if (properties.isSet(ServerConstants.OPTION_CREATE_TOP_ONTOLOGY)){
            handleTopOntology(existingLoadedOntologies);
        }

        if (getActiveOntology() == null){
            setActiveOntology(ont); // the active ontology is always the first that was loaded
        }
    }

    /**
     * Required because there are currently no listeners on the manager to tell this has happened
     * @param ontURI
     */
    public void removeOntology(URI ontURI) {
        if (getActiveOntology().getURI().equals(ontURI)){
            setActiveOntology(null);
        }

        mngr.removeOntology(ontURI);

        if (getActiveOntology() == null){
            final Set<OWLOntology> visibleOntologies = getActiveOntologies();
            if (!visibleOntologies.isEmpty()){
                setActiveOntology(visibleOntologies.iterator().next());
            }
        }

        clear();
    }

    public void clearOntologies() {
        activeOntology = null;
        topOntology = null;

        if (mngr != null){
            for (OWLOntology ont : mngr.getOntologies()){
                mngr.removeOntology(ont.getURI());
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

    public ServerProperties getProperties() {
        return properties;
    }

    public String getID() {
        return id;
    }

    public OWLOntology getActiveOntology() {
        if (activeOntology == null){
            String ont = getProperties().get(ServerConstants.OPTION_ACTIVE_ONT);
            if (ont != null){
                URI activeOntURI = URI.create(ont);
                if (activeOntURI != null){
                    activeOntology = getOntology(activeOntURI);
                }
            }
        }
        return activeOntology;
    }

    public void setActiveOntology(OWLOntology ont) {
        if (getActiveOntology() == null || !getActiveOntology().equals(ont)){
            activeOntology = ont;

            if (ont != null){
                getProperties().set(ServerConstants.OPTION_ACTIVE_ONT, ont.getURI().toString());
            }
            else{
                getProperties().remove(ServerConstants.OPTION_ACTIVE_ONT);
            }



            clear();

            for (OWLServerListener l : listeners){
                l.activeOntologyChanged(ont);
            }
        }
    }

    public Set<OWLOntology> getOntologies() {
        Set<OWLOntology> allOnts = new HashSet<OWLOntology>(mngr.getOntologies());
        if (topOntology != null){
            allOnts.remove(topOntology);
        }
        return allOnts;
    }

    public Set<OWLOntology> getActiveOntologies() {
        Set<OWLOntology> closure = new HashSet<OWLOntology>(mngr.getImportsClosure(getActiveOntology()));
        if (topOntology != null){
            closure.add(topOntology);
        }
        return closure;
    }

    public OWLOntologyManager getOWLOntologyManager() {
        return mngr;
    }

    public synchronized OWLReasoner getOWLReasoner() {
        if (reasoner == null && !serverIsDead()){
            try {
                logger.debug("Creating reasoner");
                OWLReasoner reasoner = null;

                final String selectedReasoner = properties.get(ServerConstants.OPTION_REASONER);

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
                    logger.debug("  DIG");
                    DIGReasonerPreferences.getInstance().setReasonerURL(new URL(properties.get(ServerConstants.OPTION_DIG_REASONER_URL)));
                    reasoner = new DIGReasoner(mngr);
                }

                if (reasoner != null){
                    this.reasoner = new SynchronizedOWLReasoner(reasoner);
                    this.reasoner.loadOntologies(getActiveOntologies());
                    this.reasoner.classify();
                }
            }
            catch (Throwable e) {
                logger.error("Error trying to get reasoner", e);
            }
        }
        return reasoner;
    }

    public ToldClassHierarchyReasoner getClassHierarchyProvider() {
        if (toldClassHierarchyReasoner == null && !serverIsDead()){
            toldClassHierarchyReasoner = new ToldClassHierarchyReasoner(mngr);
            toldClassHierarchyReasoner.loadOntologies(getActiveOntologies());
            toldClassHierarchyReasoner.classify();
        }
        return toldClassHierarchyReasoner;
    }

    public ToldPropertyHierarchyReasoner getPropertyHierarchyProvider() {
        if (toldPropertyHierarchyReasoner == null && !serverIsDead()){
            toldPropertyHierarchyReasoner = new ToldPropertyHierarchyReasoner(mngr);
            try {
                toldPropertyHierarchyReasoner.loadOntologies(getActiveOntologies());
                toldPropertyHierarchyReasoner.classify();
            }
            catch (OWLReasonerException e) {
                logger.error("Could not get property hierarchy provider", e);
            }
        }
        return toldPropertyHierarchyReasoner;
    }

    public Comparator<OWLObject> getComparator() {
        if (comparator == null && !serverIsDead()){
            comparator = new OWLObjectComparator<OWLObject>(this);
        }
        return comparator;
    }

    public OWLNamedObjectFinder getFinder() {
        if (finder == null && !serverIsDead()){
            finder = new OWLNamedObjectFinderImpl(new OWLNameMapperImpl(this), this);
        }
        return finder;
    }

    public NamedObjectShortFormProvider getNameRenderer() {
        if (renderer == null && !serverIsDead()){
            String ren = properties.get(ServerConstants.OPTION_REN);
            if (ren.equals(ServerConstants.RENDERER_FRAG)){
                renderer = new FragmentShortFormProvider();
            }
            else if (ren.equals(ServerConstants.RENDERER_LABEL)){
                String lang = properties.get(ServerConstants.OPTION_LABEL_LANG);
                if ("".equals(lang)){
                    lang = null;
                }
                renderer = new LanguageLabelShortFormProvider(this,
                                                              URI.create(properties.get(ServerConstants.OPTION_LABEL_URI)),
                                                              lang);
            }
        }
        return renderer;
    }


    public final OWLDescriptionParser getDescriptionParser(String type){
        if (!serverIsDead()){
            return parsers.get(type);
        }
        return null;
    }

    public final void registerDescriptionParser(String syntax, OWLDescriptionParser parser) {
        parsers.put(syntax, parser);
    }

    public Set<String> getSupportedSyntaxes() {
        return parsers.keySet();
    }

    public void clear() {
        if (finder != null){
            finder.dispose();
            finder = null;
        }

        if (reasoner != null){
            try {
                reasoner.dispose();
            }
            catch (OWLReasonerException e) {
                logger.error(e);
            }
            reasoner = null;
        }

        renderer = null;
        comparator = null;
        toldClassHierarchyReasoner = null;
        toldPropertyHierarchyReasoner = null;
    }

    public void dispose() {

        clearOntologies();

        mngr = null;

        parsers.clear();

        properties.removePropertyChangeListener(propertyChangeListener);

        serverIsDead = true;
    }

    public OWLOntology getOntology(URI uri) {
        for (OWLOntology ont : getOntologies()){
            if (ont.getURI().equals(uri)){
                return ont;
            }
        }
        return null;
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
                final URIBaseURIMapper mapper = new URIBaseURIMapper(baseURI);
                baseMapper.put(baseURI, mapper);
                mngr.addURIMapper(mapper);
            }
        }
    }


//    private void updateReasoner() {
//        if (reasoner != null){
//            try {
//                reasoner.clearOntologies();
//                reasoner.loadOntologies(getActiveOntologies());
//                reasoner.classify();
//            }
//            catch (OWLReasonerException e) {
//                logger.error(e);
//            }
//        }
//    }

    protected boolean serverIsDead() {
        return serverIsDead;
    }


    // called when new ontologies are loaded to add
    private void handleTopOntology(Set<OWLOntology> existingLoadedOntologies) throws OWLOntologyCreationException {
        try {
            if (topOntology == null){
                topOntology = createTopOntology();
            }
            Set<OWLOntology> newlyLoadedOntologies = new HashSet<OWLOntology>(mngr.getOntologies());
            newlyLoadedOntologies.removeAll(existingLoadedOntologies);
            List<OWLOntologyChange> changes = new ArrayList<OWLOntologyChange>();
            for (OWLOntology newOnt : newlyLoadedOntologies){
                changes.addAll(updateTopOntology(newOnt));
            }
            mngr.applyChanges(changes);
        }
        catch (OWLOntologyChangeException e) {
            throw new OWLOntologyCreationException(e);
        }

    }

    /**
     * Build the top level ontology we will send to the reasoner that will contain additional axioms to
     * assist the application - eg a top level property etc
     * @return
     * @throws OWLOntologyCreationException
     */
    private OWLOntology createTopOntology() throws OWLOntologyCreationException {
        OWLOntology top = mngr.createOntology(ServerConstants.TOP_ONTOLOGY_URI);
        OWLDataFactory df = mngr.getOWLDataFactory();
        OWLObjectProperty isRelatedTo = df.getOWLObjectProperty(ServerConstants.RELATED_TO);
        List<OWLOntologyChange> changes = new ArrayList<OWLOntologyChange>();

        OWLConstant label = df.getOWLUntypedConstant("related to");
        OWLAnnotation labelAnnot = df.getOWLConstantAnnotation(OWLRDFVocabulary.RDFS_LABEL.getURI(), label);
        changes.add(new AddAxiom(top, df.getOWLEntityAnnotationAxiom(isRelatedTo, labelAnnot)));

        try {
            mngr.applyChanges(changes);
        }
        catch (OWLOntologyChangeException e) {
            throw new OWLOntologyCreationException(e);
        }
        return top;
    }


    private List<OWLOntologyChange> updateTopOntology(OWLOntology newOnt) {
        List<OWLOntologyChange> changes = new ArrayList<OWLOntologyChange>();
        OWLDataFactory df = mngr.getOWLDataFactory();

        // make all root object properties subs of the top property
        OWLObjectProperty isRelatedTo = df.getOWLObjectProperty(ServerConstants.RELATED_TO);
        for (OWLObjectProperty p : newOnt.getReferencedObjectProperties()){
            if (p.getSuperProperties(mngr.getOntologies()).isEmpty()){
                changes.add(new AddAxiom(topOntology, df.getOWLSubObjectPropertyAxiom(p, isRelatedTo)));
            }
        }

        return changes;
    }
}
