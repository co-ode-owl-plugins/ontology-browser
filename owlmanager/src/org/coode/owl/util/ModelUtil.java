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

    public static boolean isDeprecated(OWLEntity entity, Set<OWLOntology> ontologies) {
        return false; // there is currently no implementation of deprecated in the OWL API
    }

    
    public static Set<? extends OWLEntity> getOWLEntitiesFromOntology(NamedObjectType type, OWLOntology ont) {
        switch(type){
            case classes: return ont.getClassesInSignature();
            case objectproperties: return ont.getObjectPropertiesInSignature();
            case dataproperties: return ont.getDataPropertiesInSignature();
            case annotationproperties: return ont.getAnnotationPropertiesInSignature();
            case individuals: return ont.getIndividualsInSignature();
            case datatypes: return ont.getDatatypesInSignature();
            case entities: return ont.getSignature();
            default: throw new RuntimeException("Object type not known: " + type);
        }
    }
}
