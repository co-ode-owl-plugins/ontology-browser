/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.doclet;

import org.coode.html.OWLHTMLKit;
import org.coode.html.impl.OWLHTMLConstants;
import org.coode.html.impl.OWLHTMLProperty;
import org.coode.html.util.URLUtils;
import org.semanticweb.owlapi.model.OWLEntity;

import java.io.PrintWriter;
import java.net.URL;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jan 25, 2008<br><br>
 */
public class LinkDoclet<O extends OWLEntity> implements HTMLDoclet<O>{

    private O object;

    private OWLHTMLKit kit;

    private OWLHTMLConstants.LinkTarget target;

    private Map<String, String> attributes = new HashMap<String, String>();


    public LinkDoclet(O object, OWLHTMLKit kit) {
        this.object = object;
        this.kit = kit;
    }

    public String getID() {
        return "doclet.link." + object.toString();
    }

    public void renderContent(URL pageURL, PrintWriter out) {
        String shortName = kit.getOWLServer().getShortFormProvider().getShortForm(object);
        URL href = kit.getURLScheme().getURLForOWLObject(object);

        out.print("<a href='" + URLUtils.createRelativeURL(pageURL, href) + "'");

        for (String attr : attributes.keySet()){
            out.print(" " + attr + "='" + attributes.get(attr) + "'");
        }
        if (target != null && !isSingleFrameNavigation()){
            out.print(" target='" + target + "'");
        }
        out.println(" >" + shortName + "</a>");
    }

    public void renderAll(URL pageURL, PrintWriter out) {
        renderContent(pageURL, out);
    }

    public void setUserObject(O object) {
        this.object = object;
    }

    public O getUserObject() {
        return object;
    }

    public boolean isPinned() {
        return true;
    }

    public Set<URL> getRequiredCSS() {
        return Collections.emptySet();
    }

    public Set<URL> getRequiredJS() {
        return Collections.emptySet();
    }

    public void setCSSClass(String cssClass){
        attributes.put("class", cssClass);
    }

    public void setCSS(String css){
        attributes.put("style", css);
    }

    public void setTarget(OWLHTMLConstants.LinkTarget target){
        this.target = target;
    }

    private boolean isSingleFrameNavigation() {
        return kit.getHTMLProperties().get(OWLHTMLProperty.optionContentWindow) == null;
    }

    public void addAttribute(String attr, String value) {
        attributes.put(attr, value);
    }
}
