/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.doclet;

import org.coode.html.OWLHTMLServer;
import org.semanticweb.owl.model.OWLDescription;
import org.semanticweb.owl.model.OWLOntology;
import org.semanticweb.owl.model.OWLProperty;

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
public class DomainsDoclet<O extends OWLProperty> extends AbstractOWLElementsDoclet<O, OWLDescription> {

    public DomainsDoclet(OWLHTMLServer server) {
        super("Domains", Format.list, server);
    }

    protected Collection<OWLDescription> getElements(Set<OWLOntology> onts) {
        return getUserObject().getDomains(onts);
    }
}
