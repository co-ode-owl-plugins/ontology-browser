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
        out.println("<ul class='minihierarchy'>");
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
            HierarchyNodeDoclet<O> lastPathContainingFocusedNode = null;
            for (O root : getModel().getRoots()){
                HierarchyNodeDoclet<O> subDoclet = new HierarchyNodeDoclet<O>(getServer(), getModel());
                subDoclet.setAutoExpandEnabled(isAutoExpandSubs());
                subDoclet.setUserObject(root);
                addDoclet(subDoclet);
                if (getModel().pathContainsNode(root, getModel().getFocus())){
                    lastPathContainingFocusedNode = subDoclet;
                }
            }
            if(lastPathContainingFocusedNode != null){ // only show subs for the last branch
                lastPathContainingFocusedNode.setShowSubs(getServer().getProperties().isSet(ServerConstants.OPTION_RENDER_SUBS));
            }
            else{
                throw new RuntimeException("Root: cannot find a path containing the node: " + object);
            }
        }
        super.setUserObject(object);
    }
}
