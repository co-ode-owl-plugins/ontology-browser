/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.doclet;

import java.util.Collection;
import java.util.Set;

import org.coode.html.OWLHTMLKit;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLProperty;
import org.semanticweb.owlapi.search.EntitySearcher;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jan 25, 2008<br><br>
 */
public class AssertedSuperpropertiesDoclet<O extends OWLProperty> extends AbstractOWLElementsDoclet<O, O> {

    public AssertedSuperpropertiesDoclet(OWLHTMLKit kit) {
        super("Superproperties", Format.list, kit);
    }

    @Override
    protected Collection<O> getAssertedElements(Set<OWLOntology> onts) {
        return EntitySearcher.getSuperProperties(
        getUserObject(), onts);
    }
}
