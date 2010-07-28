/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.doclet;

import org.coode.html.OWLHTMLKit;
import org.coode.html.impl.OWLHTMLProperty;
import org.coode.owl.mngr.NamedObjectType;
import org.semanticweb.owlapi.model.OWLEntity;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Feb 7, 2008<br><br>
 */
public class OWLEntityTitleDoclet<O extends OWLEntity> extends AbstractTitleDoclet<O> {


    public OWLEntityTitleDoclet(OWLHTMLKit kit) {
        super(kit);
    }

    public String getTitle() {
        final O object = getUserObject();
        String title = "";
        if (!isShowMiniHierarchiesEnabled()){
            title = NamedObjectType.getType(object).getSingularRendering() + ": ";
        }
        return title + getOWLHTMLKit().getOWLServer().getShortFormProvider().getShortForm(object);
    }

    public String getSubtitle() {
        return getUserObject().getIRI().toString();
    }

    private boolean isShowMiniHierarchiesEnabled() {
        return getOWLHTMLKit().getHTMLProperties().isSet(OWLHTMLProperty.optionShowMiniHierarchies);
    }
}
