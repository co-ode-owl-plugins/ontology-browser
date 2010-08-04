/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.summary;

import org.coode.html.OWLHTMLKit;
import org.coode.html.doclet.HTMLDoclet;
import org.coode.html.doclet.OWLEntityTitleDoclet;
import org.coode.html.page.OWLDocPage;
import org.semanticweb.owlapi.model.OWLEntity;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jan 24, 2008<br><br>
 */
public abstract class AbstractOWLEntitySummaryHTMLPage<O extends OWLEntity> extends OWLDocPage<O> {

    private HTMLDoclet<O> hierarchy;
    private OWLEntityTitleDoclet<O> titleDoclet;

    protected AbstractOWLEntitySummaryHTMLPage(OWLHTMLKit kit) {
        super(kit);
        titleDoclet = new OWLEntityTitleDoclet<O>(kit);
        addDoclet(titleDoclet);
    }


    public final void setNavigationRenderer(HTMLDoclet doclet){
        if (doclet == null && this.hierarchy != null){
            removeDoclet(this.hierarchy);
        }
        else{
            addDoclet(doclet, indexOf(getDoclet(OWLEntityTitleDoclet.ID)));
        }
        this.hierarchy = doclet;
    }

    protected String getTitle() {
        return titleDoclet.getTitle();
    }
}
