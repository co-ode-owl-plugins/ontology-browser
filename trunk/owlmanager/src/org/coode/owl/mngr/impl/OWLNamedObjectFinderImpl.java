package org.coode.owl.mngr.impl;

import org.coode.owl.mngr.NamedObjectType;
import org.coode.owl.mngr.OWLNameMapper;
import org.coode.owl.mngr.OWLNamedObjectFinder;
import org.coode.owl.mngr.OWLServer;
import org.semanticweb.owl.model.*;

import java.net.URI;
import java.util.HashSet;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Author: Nick Drummond<br>
 * nick.drummond@cs.manchester.ac.uk<br>
 * http://www.cs.man.ac.uk/~drummond<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jun 20, 2007<br><br>
 * <p/>
 * code made available under Mozilla Public License (http://www.mozilla.org/MPL/MPL-1.1.html)<br>
 * copyright 2006, The University of Manchester<br>
 *
 * Implementation of finder that takes a partial string and does a regexp search based on the renderings
 * in the name mapper provided.
 * - Stars (*) are replaced with .* for wildcard searching
 * - Searches are not case sensitive
 * - Search string must be valid java regexp (apart from variations described above)
 */
public class OWLNamedObjectFinderImpl implements OWLNamedObjectFinder {

    private OWLNameMapper mapper;
    private OWLServer server;

    public OWLNamedObjectFinderImpl(OWLNameMapper mapper, OWLServer server) {
        this.mapper = mapper;
        this.server = server;
    }

    public Set<OWLClass> getOWLClasses (String str){
        Set<OWLClass> results = new HashSet<OWLClass>();
        getMatches(str, results, NamedObjectType.classes);
        return results;
    }

    public Set<OWLObjectProperty> getOWLObjectProperties(String str) {
        Set<OWLObjectProperty> results = new HashSet<OWLObjectProperty>();
        getMatches(str, results, NamedObjectType.objectproperties);
        return results;
    }

    public Set<OWLDataProperty> getOWLDataProperties(String str) {
        Set<OWLDataProperty> results = new HashSet<OWLDataProperty>();
        getMatches(str, results, NamedObjectType.dataproperties);
        return results;
    }

    public Set<OWLIndividual> getOWLIndividuals(String str) {
        Set<OWLIndividual> results = new HashSet<OWLIndividual>();
        getMatches(str, results, NamedObjectType.individuals);
        return results;
    }

    public Set<OWLDataType> getOWLDatatypes(String str) {
        Set<OWLDataType> results = new HashSet<OWLDataType>();
        getMatches(str, results, NamedObjectType.datatypes);
        return results;
    }

    public Set<OWLOntology> getOWLOntologies(String str) {
        Set<OWLOntology> results = new HashSet<OWLOntology>();
        getMatches(str, results, NamedObjectType.ontologies);
        return results;
    }

    public Set<OWLProperty> getOWLProperties(String str) {
        Set<OWLProperty> results = new HashSet<OWLProperty>();
        results.addAll(getOWLObjectProperties(str));
        results.addAll(getOWLDataProperties(str));
        return results;
    }

    public Set<OWLEntity> getOWLEntities(String str) {
        Set<OWLEntity> results = new HashSet<OWLEntity>();
        for (NamedObjectType subType : NamedObjectType.entitySubtypes()){
            getMatches(str, results, subType);
        }
        return results;
    }

    public Set<? extends OWLNamedObject> getOWLNamedObjects(String str, NamedObjectType type) {
        Set<? extends OWLNamedObject> results = new HashSet<OWLNamedObject>();
        getMatches(str, results, type);
        return results;
    }

    public Set<? extends OWLNamedObject> getOWLNamedObjects(String str, NamedObjectType type, OWLOntology ont) {
        final Set<? extends OWLNamedObject> results = getOWLNamedObjects(str, type);
        if (ont != null){
            for (OWLNamedObject result : results){
                if (!type.containsReference(ont, result.getURI())){
                    results.remove(result);
                }
            }
        }
        return results;
    }

    public Set<? extends OWLNamedObject> getOWLNamedObjects(URI uri, NamedObjectType type) {
        Set<OWLNamedObject> results = new HashSet<OWLNamedObject>();
        if (uri.isAbsolute()){
            switch(type){
                case ontologies:
                    OWLNamedObject ontology = type.getExistingObject(uri, server);
                    if (ontology != null){
                        results.add(ontology);
                    }
                    break;
                case entities:
                    for (OWLOntology ont : server.getActiveOntologies()){
                        for (NamedObjectType subType : NamedObjectType.entitySubtypes()){
                            if (subType.containsReference(ont, uri)){
                                results.add(subType.getExistingObject(uri, server));
                            }
                        }
                    }
                    break;
                default:
                    for (OWLOntology ont : server.getActiveOntologies()){
                        if (type.containsReference(ont, uri)){
                            results.add(type.getExistingObject(uri, server));
                        }
                    }
            }
        }
        return results;
    }

    public Set<? extends OWLNamedObject> getOWLNamedObjects(URI uri, NamedObjectType type, OWLOntology ont) {
        final Set<? extends OWLNamedObject> results = getOWLNamedObjects(uri, type);
        if (ont != null){
            for (OWLNamedObject result : results){
                if (!type.containsReference(ont, result.getURI())){
                    results.remove(result);
                }
            }
        }
        return results;
    }

    public void dispose() {
        mapper.dispose();
        mapper = null;
        server = null;
    }

    private <T extends OWLNamedObject> void getMatches(String str, Set<T> results, NamedObjectType type){
        Pattern pattern = Pattern.compile(str.toLowerCase());
        for (String rendering : mapper.getNames(type)) {
            Matcher m = pattern.matcher(rendering.toLowerCase());
            if (m.find()) {
                mapper.get(rendering, results, type);
            }
        }
    }
}
