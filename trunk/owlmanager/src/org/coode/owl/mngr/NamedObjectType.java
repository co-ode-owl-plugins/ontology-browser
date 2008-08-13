/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.owl.mngr;

import org.semanticweb.owl.model.*;

import java.net.URI;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jan 11, 2008<br><br>
 *
 * entity types (actually, more specifically named things)
 */
public enum NamedObjectType {
    entities ("Entities", "Entity"),
    ontologies ("Ontologies", "Ontology"),
    classes ("Classes", "Class"),
    objectproperties ("Object Properties", "Object Property"),
    dataproperties ("Data Properties", "Data Property"),
    individuals ("Individuals", "Individual"),
    datatypes ("Datatypes", "Datatype");

    private static NamedObjectType[] entitySubTypes = new NamedObjectType[]{classes, objectproperties, dataproperties, individuals};

    private String plural;
    private String singular;

    NamedObjectType(String plural, String singular){
        this.plural = plural;
        this.singular = singular;
    }

    public String getPluralRendering(){
        return plural;
    }

    public String getSingularRendering() {
        return singular;
    }

    /**
     * Will get an object from the df (or the ont mngr for ontologies).
     * In most cases, you should already be sure that the object exists
     * @param uri
     * @param server
     * @return
     */
    public OWLNamedObject getExistingObject(URI uri, OWLServer server){
        switch(this){
            case classes:
                return server.getOWLOntologyManager().getOWLDataFactory().getOWLClass(uri);
            case objectproperties:
                return server.getOWLOntologyManager().getOWLDataFactory().getOWLObjectProperty(uri);
            case dataproperties:
                return server.getOWLOntologyManager().getOWLDataFactory().getOWLDataProperty(uri);
            case individuals:
                return server.getOWLOntologyManager().getOWLDataFactory().getOWLIndividual(uri);
            case datatypes:
                return server.getOWLOntologyManager().getOWLDataFactory().getOWLDataType(uri);
            case ontologies:
                return server.getOWLOntologyManager().getOntology(uri);
            case entities:
                throw new IllegalArgumentException("Cannot call NamedObjectType.getExistingObject() on entities");
        }
        return null;
    }

    public boolean containsReference(OWLOntology ont, URI uri){
        switch(this){
            case classes:
                return ont.containsClassReference(uri);
            case objectproperties:
                return ont.containsObjectPropertyReference(uri);
            case dataproperties:
                return ont.containsDataPropertyReference(uri);
            case individuals:
                return ont.containsIndividualReference(uri);
            case datatypes:
                return ont.containsDataTypeReference(uri);
            case ontologies:
                throw new IllegalArgumentException("Cannot call NamedObjectType.containsReference() on ontologies");
            case entities:
                return ont.containsClassReference(uri) ||
                       ont.containsObjectPropertyReference(uri) ||
                       ont.containsDataPropertyReference(uri) ||
                       ont.containsIndividualReference(uri) ||
                       ont.containsDataTypeReference(uri);
        }
        return false;
    }
    
    public OWLNamedObject getNamedObjectFromOntology(OWLOntology ont, URI uri, OWLServer server){
        if (containsReference(ont, uri)){
            return getExistingObject(uri, server);
        }
        return null;
    }

    public static NamedObjectType getType(OWLNamedObject object){
        if (object instanceof OWLClass){
            return classes;
        }
        else if (object instanceof OWLObjectProperty){
            return objectproperties;
        }
        else if (object instanceof OWLDataProperty){
            return dataproperties;
        }
        else if (object instanceof OWLIndividual){
            return individuals;
        }
        else if (object instanceof OWLDataType){
            return datatypes;
        }
        else if (object instanceof OWLOntology){
            return ontologies;
        }
        return null;
    }

    public Set<? extends OWLNamedObject> getNamedObjectsFromOntology(OWLOntology ont) {
        switch(this){
            case classes:
                return ont.getReferencedClasses();
            case objectproperties:
                return ont.getReferencedObjectProperties();
            case dataproperties:
                return ont.getReferencedDataProperties();
            case individuals:
                return ont.getReferencedIndividuals();
            case entities:
                return ont.getReferencedEntities();
            case ontologies:
                throw new IllegalArgumentException("Cannot call NamedObjectType.getNamedObjectsFromOntology() on ontologies");
        }
        return Collections.emptySet();
    }

    public static Set<String> getRenderings() {
        Set<String> renderings = new HashSet<String>();
        for (NamedObjectType type : NamedObjectType.values()){
            renderings.add(type.toString());
        }
        return renderings;
    }

    public static NamedObjectType[] entitySubtypes() {
        return entitySubTypes;
    }
}
