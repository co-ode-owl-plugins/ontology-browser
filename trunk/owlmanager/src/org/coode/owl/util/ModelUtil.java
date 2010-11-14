package org.coode.owl.util;

import org.coode.owl.mngr.NamedObjectType;
import org.semanticweb.owlapi.model.*;

import java.util.Set;

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

    public static String getOntologyIdString(OWLOntology ont){
        if (ont.isAnonymous()){
            return ont.getOWLOntologyManager().getOntologyDocumentIRI(ont).toString();
        }
        else{
            return ont.getOntologyID().getDefaultDocumentIRI().toString();
        }
    }

    public static boolean isDeprecated(OWLEntity entity, Set<OWLOntology> ontologies) {
        return false; // there is currently no implementation of deprecated in the OWL API
    }

    
    public static Set<? extends OWLEntity> getOWLEntitiesFromOntology(NamedObjectType type, OWLOntology ont) {
        switch(type){
            case classes:
                Set<OWLClass> clses = ont.getClassesInSignature();
                clses.add(ont.getOWLOntologyManager().getOWLDataFactory().getOWLThing());
                clses.add(ont.getOWLOntologyManager().getOWLDataFactory().getOWLNothing());
                return clses;
            case objectproperties:
                Set<OWLObjectProperty> ops = ont.getObjectPropertiesInSignature();
                ops.add(ont.getOWLOntologyManager().getOWLDataFactory().getOWLTopObjectProperty());
                ops.add(ont.getOWLOntologyManager().getOWLDataFactory().getOWLBottomObjectProperty());
                return ops;
            case dataproperties:
                Set<OWLDataProperty> dps = ont.getDataPropertiesInSignature();
                dps.add(ont.getOWLOntologyManager().getOWLDataFactory().getOWLTopDataProperty());
                dps.add(ont.getOWLOntologyManager().getOWLDataFactory().getOWLBottomDataProperty());
                return dps;
            case annotationproperties: return ont.getAnnotationPropertiesInSignature();
            case individuals: return ont.getIndividualsInSignature();
            case datatypes:
                Set<OWLDatatype> dts = ont.getDatatypesInSignature();
                dts.add(ont.getOWLOntologyManager().getOWLDataFactory().getTopDatatype());
                return dts;
            case entities: return ont.getSignature();
            default: throw new RuntimeException("Object type not known: " + type);
        }
    }
}
