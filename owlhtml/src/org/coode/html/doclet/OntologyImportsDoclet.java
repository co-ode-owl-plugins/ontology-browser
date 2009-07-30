/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.doclet;

import org.coode.html.OWLHTMLServer;
import org.semanticweb.owl.model.OWLImportsDeclaration;
import org.semanticweb.owl.model.OWLOntology;

import java.util.Collection;
import java.util.Set;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Feb 5, 2008<br><br>
 */
public class OntologyImportsDoclet extends AbstractOWLElementsDoclet<OWLOntology, OWLImportsDeclaration> {

    public OntologyImportsDoclet(OWLHTMLServer server) {
        super("Imports", Format.list, server);
    }

    protected Collection<OWLImportsDeclaration> getElements(Set<OWLOntology> onts) {
        return getUserObject().getImportsDeclarations();
    }
}
