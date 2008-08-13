package org.coode.owl.util;

import org.coode.owl.mngr.OWLServer;
import org.semanticweb.owl.model.OWLClass;
import org.semanticweb.owl.model.OWLDataFactory;
import org.semanticweb.owl.model.OWLEntity;
import org.semanticweb.owl.model.OWLOntology;

import java.util.HashSet;
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

    public static boolean isDeprecated(OWLEntity entity, Set<OWLOntology> ontologies){
        return false; // there is currently no implementation of deprecated in the OWL API
    }

    public static Set<OWLClass> filterClasses(Set<Set<OWLClass>> original, OWLServer server) {
        Set<OWLClass> result = new HashSet<OWLClass>();

        final OWLDataFactory df = server.getOWLOntologyManager().getOWLDataFactory();
        final OWLClass owlNothing = df.getOWLNothing();

        for (Set<OWLClass> set : original) {
            if (!set.contains(owlNothing)){
                result.addAll(set);
            }
        }
        return result;
    }
}
