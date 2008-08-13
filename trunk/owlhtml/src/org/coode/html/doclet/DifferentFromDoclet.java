/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.doclet;

import org.coode.html.OWLHTMLServer;
import org.semanticweb.owl.model.OWLIndividual;
import org.semanticweb.owl.model.OWLOntology;

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Feb 5, 2008<br><br>
 */
public class DifferentFromDoclet extends AbstractOWLElementsDoclet<OWLIndividual, OWLIndividual> {

    public DifferentFromDoclet(OWLHTMLServer server) {
        super("Different From", Format.csv, server);
    }

    protected Collection<OWLIndividual> getElements(Set<OWLOntology> onts) {
        Set<OWLIndividual> sameAs = new HashSet<OWLIndividual>();
        for (OWLOntology ont : onts){
            sameAs.addAll(getUserObject().getDifferentIndividuals(ont));
        }
        return sameAs;
    }
}
