/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.doclet;

import org.coode.html.OWLHTMLServer;
import org.coode.html.hierarchy.TreeFragment;
import org.coode.owl.mngr.ServerConstants;
import org.semanticweb.owl.model.OWLNamedObject;

import java.io.PrintWriter;
import java.net.URL;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Feb 7, 2008<br><br>
 */
public class HierarchyRootDoclet<O extends OWLNamedObject> extends AbstractHierarchyNodeDoclet<O>{

    public HierarchyRootDoclet(OWLHTMLServer server, TreeFragment<O> model) {
        super(server, model);
        setPinned(false);
    }

    protected void renderHeader(URL pageURL, PrintWriter out) {
        renderBoxStart(getModel().getTitle(), out);
        out.println("<ul style='list-style-type: disc;'>");
    }

    protected void renderFooter(URL pageURL, PrintWriter out) {
        out.println("</ul>");
        renderBoxEnd(getModel().getTitle(), out);
    }

    /**
     * This will be the <em>focus</em> object, not the root
     * @param object
     */
    public void setUserObject(O object) {
        clear();
        getModel().setFocus(object);
        if (object != null){
            boolean firstBranchVisited = false;
            for (O root : getModel().getRoots()){
                final HierarchyNodeDoclet<O> subDoclet = new HierarchyNodeDoclet<O>(getServer(), getModel());
                if(!firstBranchVisited){
                    // only show subs for the first branch
                    subDoclet.setShowSubs(getServer().getProperties().isSet(ServerConstants.OPTION_RENDER_SUBS));
                }
                subDoclet.setAutoExpandEnabled(isAutoExpandSubs());
                subDoclet.setUserObject(root);
                addDoclet(subDoclet);
                firstBranchVisited = true;
            }
        }
        super.setUserObject(object);
    }
}
