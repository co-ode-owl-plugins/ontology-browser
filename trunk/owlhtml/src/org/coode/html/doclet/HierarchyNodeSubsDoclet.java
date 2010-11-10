/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.doclet;

import org.coode.html.OWLHTMLKit;
import org.coode.html.impl.OWLHTMLConstants;
import org.coode.html.renderer.OWLHTMLRenderer;
import org.coode.owl.mngr.HierarchyProvider;
import org.semanticweb.owlapi.model.OWLEntity;

import java.io.PrintWriter;
import java.net.URL;
import java.util.List;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Feb 7, 2008<br><br>
 *
 * Special case, when the focused OWL Object is rendered, its subs are likely to be rendered differently from the rest of the tree
 */
public class HierarchyNodeSubsDoclet<O extends OWLEntity> extends LeafNodeDoclet<O> {

    private int subThreshold = 3;

    public HierarchyNodeSubsDoclet(OWLHTMLKit kit, O focus, HierarchyProvider<O> model) {
        super(kit, focus, model);
    }

    /**
     * the number of subclasses below the current class that you can see without expanding - default is 3
     * @param subThreshold
     */
    public void setSubThreshold(int subThreshold) {
        this.subThreshold = subThreshold;
    }


    protected void renderHeader(URL pageURL, PrintWriter out) {

//        System.out.println("isShowSubsEnabled() = " + isShowSubsEnabled());
//
//        if (isShowSubsEnabled()){

            List<O> children = getChildren();

            if (!children.isEmpty()){

//                out.println("<ul>");

                //+1 takes into account additional line used for link
                final boolean hideSomeChildren = hideSomeChildren();

                final OWLHTMLRenderer owlhtmlRenderer = new OWLHTMLRenderer(getOWLHTMLKit());

                for (int i=0; i<children.size(); i++){
                    if (hideSomeChildren && i == subThreshold){
                        out.println("<span id='" + getID() + "' style='display: none;'>");
                    }
                    renderNode(children.get(i), owlhtmlRenderer, pageURL, out);
                }

                if (hideSomeChildren){
                    out.println("</span><!-- subs -->");
                    int count = children.size() - subThreshold;
                    out.println("<span id='" + getID() + "-expand'>" +
                                "<a class='subsexpand' id='showSubs' href='#' onClick=\"showSubs('" + getID() + "');\">"
                                + count + " more...</a></span>");
                }

//                out.println("</ul>");
            }
//        }
    }

    private List<O> getChildren() {
        return asOrderedList(getHierarchyProvider().getChildren(getUserObject()));
    }

    private boolean hideSomeChildren() {
        return !isAutoExpandSubs() && getChildren().size() > subThreshold + 1;
    }

    public List<URL> getRequiredJS() {
        List<URL> js = super.getRequiredJS();
        if (hideSomeChildren()){
            js.add(getOWLHTMLKit().getURLScheme().getURLForRelativePage(OWLHTMLConstants.JS_TREE));
        }
        return js;
    }
}
