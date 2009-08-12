package org.coode.owl.mngr.impl;

/**
 * Author: Nick Drummond<br>
 * nick.drummond@cs.manchester.ac.uk<br>
 * http://www.cs.man.ac.uk/~drummond<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jun 4, 2007<br><br>
 * <p/>
 * code made available under Mozilla Public License (http://www.mozilla.org/MPL/MPL-1.1.html)<br>
 * copyright 2006, The University of Manchester<br>
 *
 * Caching rendering to URI map. Gets rebuilt when the ontology has changed (only when next used).
 * Implementation of finder interface searches on string rendering of URI first
 * and then absolute rendered name (not partial matches)
 *
 * Could be more lazy and only rebuild the indices that are being queried
 */
public class OWLNameCacheImpl{// implements OWLNameCache {
//
//    // caches
//    private Map<String, Set<URI>> classmap = new HashMap<String, Set<URI>>();
//    private Map<String, Set<URI>> objpropmap = new HashMap<String, Set<URI>>();
//    private Map<String, Set<URI>> datapropmap = new HashMap<String, Set<URI>>();
//    private Map<String, Set<URI>> individualsmap = new HashMap<String, Set<URI>>();
//    private Map<String, Set<URI>> datatypemap = new HashMap<String, Set<URI>>();
//    private Map<String, Set<URI>> ontologymap = new HashMap<String, Set<URI>>();
//    private Set<String> entityNames;
//
//
//    private OWLServer server;
//
//    private boolean rebuildRequired = false;
//
//    private OWLOntologyChangeListener ontologyChangeListener = new OWLOntologyChangeListener(){
//        public void ontologiesChanged(List<? extends OWLOntologyChange> changes) throws OWLException {
//            rebuildRequired = true;
//        }
//    };
//
//    private OWLServerListener serverListener = new OWLServerListener(){
//        public void activeOntologyChanged(OWLOntology ont) {
//            rebuildRequired = true;
//        }
//    };
//
//
//    public OWLNameCacheImpl(OWLServer server) {
//        this.server = server;
//
//        server.addServerListener(serverListener);
//
//        buildCache();
//
//        server.getOWLOntologyManager().addOntologyChangeListener(ontologyChangeListener);
//    }
//
//
//    private synchronized void rebuildCache() {
//        clearCaches();
//        buildCache();
//    }
//
//    private synchronized void buildCache() {
//        ShortFormProvider ren = server.getShortFormProvider();
//
//        final OWLDataFactory df = server.getOWLOntologyManager().getOWLDataFactory();
//        addNamedObject(df.getOWLThing(), classmap, ren); // make sure owl:Thing is in there (this is not always referenced by the ontologies)
//
//        for (OWLOntology ont : server.getActiveOntologies()){
//            addNamedObject(ont, ontologymap, ren);
//            for (OWLClass cls : ont.getReferencedClasses()){
//                addNamedObject(cls, classmap, ren);
//            }
//            for (OWLObjectProperty p : ont.getReferencedObjectProperties()){
//                addNamedObject(p, objpropmap, ren);
//            }
//            for (OWLDataProperty p : ont.getReferencedDataProperties()){
//                addNamedObject(p, datapropmap, ren);
//            }
//            for (OWLIndividual i : ont.getReferencedIndividuals()){
//                addNamedObject(i, individualsmap, ren);
//            }
//            for (URI dtURI : XSDVocabulary.ALL_DATATYPES){
//                OWLDatatype d = df.getOWLDatatype(dtURI);
//                addNamedObject(d, datatypemap, ren);
//            }
//        }
//
//        rebuildRequired = false;
//    }
//
//
//    public synchronized Set<String> getClassNames() {
//        if (rebuildRequired){
//            rebuildCache();
//        }
//        return classmap.keySet();
//    }
//
//    public synchronized Set<String> getObjectPropertyNames() {
//        if (rebuildRequired){
//            rebuildCache();
//        }
//        return objpropmap.keySet();
//    }
//
//    public synchronized Set<String> getDataPropertyNames() {
//        if (rebuildRequired){
//            rebuildCache();
//        }
//        return datapropmap.keySet();
//    }
//
//    public synchronized Set<String> getIndividualNames() {
//        if (rebuildRequired){
//            rebuildCache();
//        }
//        return individualsmap.keySet();
//    }
//
//    public Set<String> getDatatypeNames() {
//        if (rebuildRequired){
//            rebuildCache();
//        }
//        return datatypemap.keySet();
//    }
//
//    public Set<String> getEntityNames() {
//        if (rebuildRequired){
//            rebuildCache();
//        }
//        if (entityNames == null){
//            entityNames = new HashSet<String>();
//            entityNames.addAll(classmap.keySet());
//            entityNames.addAll(objpropmap.keySet());
//            entityNames.addAll(datapropmap.keySet());
//            entityNames.addAll(individualsmap.keySet());
//            entityNames.addAll(datatypemap.keySet());
//        }
//        return entityNames;
//    }
//
//    public Set<String> getOntologyNames() {
//        if (rebuildRequired){
//            rebuildCache();
//        }
//        return ontologymap.keySet();
//    }
//
//    public Set<String> getNames(NamedObjectType type) {
//        switch(type){
//            case classes:
//                return getClassNames();
//            case objectproperties:
//                return getObjectPropertyNames();
//            case dataproperties:
//                return getDataPropertyNames();
//            case individuals:
//                return getIndividualNames();
//            case datatypes:
//                return getDatatypeNames();
//            case ontologies:
//                return getOntologyNames();
//            case entities:
//                return getEntityNames();
//        }
//        return Collections.emptySet();
//    }
//
//    public void dispose() {
//        clearCaches();
//        server.removeServerListener(serverListener);
//        server.getOWLOntologyManager().removeOntologyChangeListener(ontologyChangeListener);
//    }
//
//    public <T extends OWLNamedObject> void get(String exactMatch, Set<T> results, NamedObjectType type) {
//        if (rebuildRequired){
//            rebuildCache();
//        }
//
//        switch(type){
//            case entities: // call on the subtypes of entities
//                for (NamedObjectType subType : NamedObjectType.entitySubtypes()){
//                    get(exactMatch, results, subType);
//                }
//                break;
//            default:
//                final Map<String, Set<URI>> cache = getCache(type);
//                final Set<URI> matches = cache.get(exactMatch);
//                if (matches != null){
//                    for (URI uri : matches){
//                        results.add((T)type.getExistingObject(uri, server));
//                    }
//                }
//        }
//    }
//
//    private void clearCaches() {
//        classmap.clear();
//        objpropmap.clear();
//        datapropmap.clear();
//        individualsmap.clear();
//        datatypemap.clear(); // not needed really
//        entityNames = null;
//    }
//
//    private synchronized void addNamedObject(OWLNamedObject o, Map<String, Set<URI>> map, ShortFormProvider ren) {
//        final String label = ren.getShortForm(o);
//        Set<URI> uris = map.get(label);
//        if (uris == null){
//            uris = new HashSet<URI>();
//        }
//        uris.add(o.getURI());
//        map.put(label, uris);
//    }
//
//
//    private Map<String, Set<URI>>getCache(NamedObjectType type){
//        switch(type){
//            case classes:
//                return classmap;
//            case objectproperties:
//                return objpropmap;
//            case dataproperties:
//                return datapropmap;
//            case individuals:
//                return individualsmap;
//            case datatypes:
//                return datatypemap;
//            case ontologies:
//                return ontologymap;
//        }
//        return null;
//    }
}
