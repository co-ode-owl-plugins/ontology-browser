/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.doclet;

import org.coode.html.OWLHTMLServer;
import org.semanticweb.owl.model.*;

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jan 25, 2008<br><br>
 */
public class MembersDoclet extends AbstractOWLElementsDoclet<OWLClass, OWLIndividual> {


    public MembersDoclet(OWLHTMLServer server) {
        super("Disjoints", Format.csv, server);
    }

    protected Collection<OWLIndividual> getElements(Set<OWLOntology> onts) {
        Collection<OWLIndividual> members = new HashSet<OWLIndividual>();
        OWLClass cls = getUserObject();
        for (OWLOntology ont : onts){
            for (OWLAxiom ax : ont.getReferencingAxioms(cls)){
                if (ax instanceof OWLClassAssertionAxiom &&
                    ((OWLClassAssertionAxiom)ax).getDescription().equals(cls)){
                    members.add(((OWLClassAssertionAxiom)ax).getIndividual());
                }
            }
        }
        return members;
    }
}
