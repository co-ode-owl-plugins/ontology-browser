/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.doclet;

import org.coode.html.OWLHTMLKit;
import org.coode.html.hierarchy.TreeFragment;
import org.coode.html.renderer.OWLHTMLRenderer;
import org.semanticweb.owlapi.model.OWLEntity;
import org.apache.log4j.Logger;

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
public class HierarchyNodeDoclet<O extends OWLEntity> extends AbstractHierarchyNodeDoclet<O>{

    private static final Logger logger = Logger.getLogger(HierarchyNodeDoclet.class);


    public HierarchyNodeDoclet(OWLHTMLKit kit, TreeFragment<O> model) {
        super(kit, model);
    }

    protected void renderHeader(URL pageURL, PrintWriter out) {
        renderNode(getUserObject(), new OWLHTMLRenderer(getHTMLGenerator()), pageURL, out);
        if (getSubDocletCount() > 0){
            out.println("<ul class='minihierarchy'>");
        }
    }

    protected void renderFooter(URL pageURL, PrintWriter out) {
        if (getSubDocletCount() > 0){
            out.println("</ul>");
        }
    }

    public void setUserObject(O object) {
        clear();
        super.setUserObject(object);
        if (getUserObject() != null){
            if (getUserObject().equals(getModel().getFocus())){
                final HierarchyNodeSubsDoclet<O> nodeSubsDoclet = new HierarchyNodeSubsDoclet<O>(getHTMLGenerator(), getModel());
                nodeSubsDoclet.setShowSubs(isShowSubsEnabled());
                nodeSubsDoclet.setPinned(true);
                nodeSubsDoclet.setAutoExpandEnabled(isAutoExpandSubs());
                nodeSubsDoclet.setUserObject(object);
                addDoclet(nodeSubsDoclet);
            }
            else{
                HierarchyNodeDoclet<O> lastPathContainingFocusedNode = null;
                for (O child : getModel().getChildren(object)){
                    HierarchyNodeDoclet<O> subDoclet = new HierarchyNodeDoclet<O>(getHTMLGenerator(), getModel());
                    subDoclet.setPinned(true); // you will never change the subs as they will be regenerated each time this changed
                    subDoclet.setAutoExpandEnabled(isAutoExpandSubs());
                    subDoclet.setUserObject(child);
                    addDoclet(subDoclet);
                    if (getModel().pathContainsNode(child, getModel().getFocus())){
                        lastPathContainingFocusedNode = subDoclet;
                    }
                }
                if (lastPathContainingFocusedNode != null){ // only show for the last node
                    lastPathContainingFocusedNode.setShowSubs(isShowSubsEnabled());
                }
            }
        }
    }
}
