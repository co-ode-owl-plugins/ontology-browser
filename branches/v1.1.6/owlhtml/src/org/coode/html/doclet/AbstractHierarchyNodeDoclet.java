/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.doclet;

import org.apache.log4j.Logger;
import org.coode.html.OWLHTMLKit;
import org.coode.html.impl.OWLHTMLConstants;
import org.coode.html.impl.OWLHTMLProperty;
import org.coode.html.hierarchy.TreeFragment;
import org.coode.html.renderer.OWLHTMLRenderer;
import org.coode.html.util.URLUtils;
import org.semanticweb.owlapi.model.OWLEntity;

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
public abstract class AbstractHierarchyNodeDoclet<O extends OWLEntity> extends AbstractOWLDocDoclet<O>{

    private static final Logger logger = Logger.getLogger(AbstractHierarchyNodeDoclet.class);

    private final TreeFragment<O> model;

    private boolean autoExpandSubs = false;

    private boolean showSubs = false;


    public AbstractHierarchyNodeDoclet(OWLHTMLKit kit, TreeFragment<O> model) {
        super(kit);
        setPinned(true); // you will never change the subs as they will be regenerated each time this changed
        this.model = model;
    }


    public final void setAutoExpandEnabled(boolean enabled) {
        if (autoExpandSubs != enabled){
            this.autoExpandSubs = enabled;
            for (HTMLDoclet subdoclet : getDoclets()){
                if (subdoclet instanceof AbstractHierarchyNodeDoclet){
                    ((AbstractHierarchyNodeDoclet)subdoclet).setAutoExpandEnabled(enabled);
                }
            }
        }
    }


    public final void setShowSubs(boolean enabled) {
        if (showSubs != enabled){
            this.showSubs = enabled;
            AbstractHierarchyNodeDoclet lastPathContainingFocusedNode = null;
            for (HTMLDoclet subdoclet : getDoclets()){
                if (subdoclet instanceof AbstractHierarchyNodeDoclet &&
                    getModel().pathContainsNode(((AbstractHierarchyNodeDoclet<O>)subdoclet).getUserObject(), getModel().getFocus())){
                    lastPathContainingFocusedNode = ((AbstractHierarchyNodeDoclet)subdoclet);
                }
            }
            if (lastPathContainingFocusedNode != null){
                lastPathContainingFocusedNode.setShowSubs(enabled);
            }
        }
    }


    protected TreeFragment<O> getModel() {
        return model;
    }


    protected final boolean isAutoExpandSubs() {
        return autoExpandSubs;
    }


    protected final boolean isShowSubsEnabled(){
        return showSubs;
    }


    public String getID() {
        return getHTMLGenerator().getOWLServer().getShortFormProvider().getShortForm(getUserObject());
    }


    protected void renderNode(O node, OWLHTMLRenderer objRenderer, URL pageURL, PrintWriter out) {
        if (!model.isLeaf(node)){
            out.print("<li class='expandable'>");
            renderExpandLink(node, pageURL, out);
        }
        else{
            out.print("<li>");
        }
        objRenderer.render(node, pageURL, out);
        for (O synonym : model.getSynonyms(node)){
            out.print(" ");
            out.print(OWLHTMLConstants.EQUIV_CHAR);
            out.print(" ");
            objRenderer.render(synonym, pageURL, out);
        }
        out.print("</li>");
    }


    protected void renderExpandLink(O node, URL pageURL, PrintWriter out) {
        if (isRenderSubExpandLinksEnabled()){
            String link = URLUtils.createRelativeURL(pageURL, getHTMLGenerator().getURLScheme().getURLForOWLObject(node));
            if (!link.contains(OWLHTMLConstants.START_QUERY)){
                link += OWLHTMLConstants.START_QUERY;
            }
            else{
                link += OWLHTMLConstants.PARAM_SEP;
            }

            out.println(" <a href='" + link + "expanded=true'>[+]</a>");
        }
        else{
            out.println(" +");
        }
    }


    protected boolean isRenderSubsEnabled(){
        return getHTMLGenerator().getHTMLProperties().isSet(OWLHTMLProperty.optionRenderSubs);
    }


    protected boolean isRenderSubExpandLinksEnabled() {
        return getHTMLGenerator().getHTMLProperties().isSet(OWLHTMLProperty.optionRenderSubExpandLinks);
    }
}
