/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.doclet;

import org.coode.html.OWLHTMLServer;
import org.semanticweb.owl.model.OWLOntology;
import org.semanticweb.owl.model.OWLPropertyExpression;
import org.semanticweb.owl.model.OWLPropertyRange;

import java.util.Collection;
import java.util.Set;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jan 25, 2008<br><br>
 */
public class RangesDoclet<O extends OWLPropertyExpression> extends AbstractOWLElementsDoclet<O, OWLPropertyRange> {

    public RangesDoclet(OWLHTMLServer server) {
        super("Ranges", Format.list, server);
    }

    protected Collection<OWLPropertyRange> getElements(Set<OWLOntology> onts) {
        return getUserObject().getRanges(onts);
    }
}
