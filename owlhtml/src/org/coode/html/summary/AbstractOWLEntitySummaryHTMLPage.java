/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.summary;

import org.coode.html.OWLHTMLKit;
import org.coode.html.doclet.HierarchyRootDoclet;
import org.coode.html.doclet.OWLEntityTitleDoclet;
import org.coode.html.page.EmptyOWLDocPage;
import org.semanticweb.owlapi.model.OWLEntity;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jan 24, 2008<br><br>
 */
public abstract class AbstractOWLEntitySummaryHTMLPage<O extends OWLEntity> extends EmptyOWLDocPage<O> {

    private HierarchyRootDoclet<O> hierarchy;
    private OWLEntityTitleDoclet<O> titleDoclet;

    protected AbstractOWLEntitySummaryHTMLPage(OWLHTMLKit kit) {
        super(kit);
        titleDoclet = new OWLEntityTitleDoclet<O>(kit);
        addDoclet(titleDoclet);
    }


    public final void setOWLHierarchyRenderer(HierarchyRootDoclet<O> hierarchyRenderer){
        if (hierarchyRenderer == null && this.hierarchy != null){
            removeDoclet(this.hierarchy);
        }
        else{
            addDoclet(hierarchyRenderer, indexOf(getDoclet(OWLEntityTitleDoclet.ID))+1);
        }
        this.hierarchy = hierarchyRenderer;
    }

    protected String getTitle() {
        return titleDoclet.getTitle();
    }


//    public void renderContent(URL pageURL, PrintWriter out) {
//        // @@TODO temp fix, as pageURL does not currently contain all of the params when using a servlet URL scheme
//        super.renderContent(getServer().getURLScheme().getURLForNamedObject(getUserObject()), out);
//    }
}
