/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.doclet;

import org.coode.html.OWLHTMLKit;
import org.semanticweb.owlapi.model.*;

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


    public MembersDoclet(OWLHTMLKit kit) {
        super("Disjoints", Format.csv, kit);
    }

    protected Collection<OWLIndividual> getElements(Set<OWLOntology> onts) {
        Collection<OWLIndividual> members = new HashSet<OWLIndividual>();
        OWLClass cls = getUserObject();
        for (OWLOntology ont : onts){
            for (OWLAxiom ax : ont.getReferencingAxioms(cls)){
                if (ax instanceof OWLClassAssertionAxiom &&
                    ((OWLClassAssertionAxiom)ax).getClassExpression().equals(cls)){
                    members.add(((OWLClassAssertionAxiom)ax).getIndividual());
                }
            }
        }
        return members;
    }
}
