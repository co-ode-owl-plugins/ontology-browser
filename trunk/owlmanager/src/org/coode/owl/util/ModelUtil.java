package org.coode.owl.util;

import org.coode.owl.mngr.OWLServer;
import org.coode.owl.mngr.NamedObjectType;
import org.semanticweb.owlapi.model.*;

import java.util.HashSet;
import java.util.Set;
import java.net.URI;

/**
 * Author: Nick Drummond<br>
 * nick.drummond@cs.manchester.ac.uk<br>
 * http://www.cs.man.ac.uk/~drummond<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jun 29, 2007<br><br>
 * <p/>
 * code made available under Mozilla Public License (http://www.mozilla.org/MPL/MPL-1.1.html)<br>
 * copyright 2006, The University of Manchester<br>
 */
public class ModelUtil {

    public static boolean isDeprecated(OWLEntity entity, Set<OWLOntology> ontologies){
        return false; // there is currently no implementation of deprecated in the OWL API
    }

    public static Set<OWLClass> filterClasses(Set<Set<OWLClass>> original, OWLDataFactory df) {
        Set<OWLClass> result = new HashSet<OWLClass>();

        final OWLClass owlNothing = df.getOWLNothing();

        for (Set<OWLClass> set : original) {
            if (!set.contains(owlNothing)){
                result.addAll(set);
            }
        }
        return result;
    }


//        /**
//     * Will get an object from the df (or the ont mngr for ontologies).
//     * In most cases, you should already be sure that the object exists
//     * @param iri
//     *@param server  @return
//     */
//    public static OWLEntity getExistingObject(IRI iri, OWLServer server){
//        switch(this){
//            case classes:
//                return server.getOWLOntologyManager().getOWLDataFactory().getOWLClass(iri);
//            case objectproperties:
//                return server.getOWLOntologyManager().getOWLDataFactory().getOWLObjectProperty(iri);
//            case dataproperties:
//                return server.getOWLOntologyManager().getOWLDataFactory().getOWLDataProperty(iri);
//            case annotationproperties:
//                return server.getOWLOntologyManager().getOWLDataFactory().getOWLAnnotationProperty(iri);
//            case individuals:
//                return server.getOWLOntologyManager().getOWLDataFactory().getOWLNamedIndividual(iri);
//            case datatypes:
//                return server.getOWLOntologyManager().getOWLDataFactory().getOWLDatatype(iri);
//            case entities:
//                throw new IllegalArgumentException("Cannot call NamedObjectType.getExistingObject() on entities");
//        }
//        return null;
//    }
//
//    public boolean containsReference(OWLOntology ont, IRI iri){
//        URI uri = iri.toURI();
//        switch(this){
//            case classes:
//                return ont.containsClassReference(uri);
//            case objectproperties:
//                return ont.containsObjectPropertyReference(uri);
//            case dataproperties:
//                return ont.containsDataPropertyReference(uri);
//            case annotationproperties:
//                return ont.containsAnnotationPropertyReference(uri);
//            case individuals:
//                return ont.containsIndividualReference(uri);
//            case datatypes:
//                return ont.containsDatatypeReference(uri);
//            case entities:
//                return ont.containsEntityReference(uri);
//            default:
//                throw new RuntimeException("Entity type not known: " + this);
//        }
//    }
//
//    public OWLEntity getNamedObjectFromOntology(OWLOntology ont, IRI iri, OWLServer server){
//        if (containsReference(ont, iri)){
//            return getExistingObject(iri, server);
//        }
//        return null;
//    }
//
//    public static NamedObjectType getType(OWLEntity entity){
//        if (entity instanceof OWLClass){
//            return NamedObjectType.classes;
//        }
//        else if (entity instanceof OWLObjectProperty){
//            return NamedObjectType.objectproperties;
//        }
//        else if (entity instanceof OWLDataProperty){
//            return NamedObjectType.dataproperties;
//        }
//        else if (entity instanceof OWLAnnotationProperty){
//            return NamedObjectType.annotationproperties;
//        }
//        else if (entity instanceof OWLIndividual){
//            return NamedObjectType.individuals;
//        }
//        else if (entity instanceof OWLDatatype){
//            return NamedObjectType.datatypes;
//        }
//        throw new RuntimeException("Entity type not known: " + entity);
//    }
//
    public static Set<? extends OWLEntity> getOWLEntitiesFromOntology(NamedObjectType type, OWLOntology ont) {
        switch(type){
            case classes: return ont.getReferencedClasses();
            case objectproperties: return ont.getReferencedObjectProperties();
            case dataproperties: return ont.getReferencedDataProperties();
            case annotationproperties: return ont.getReferencedAnnotationProperties();
            case individuals: return ont.getReferencedIndividuals();
            case datatypes: return ont.getReferencedDatatypes();
            case entities: return ont.getReferencedEntities();
            default: throw new RuntimeException("Object type not known: " + type);
        }
    }
}
