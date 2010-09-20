/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.page;

import org.coode.html.doclet.AbstractHTMLDoclet;
import org.coode.html.impl.OWLHTMLConstants;
import org.coode.html.util.URLUtils;

import java.io.PrintWriter;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jan 24, 2008<br><br>
 *
 * Non-OWL specific HTML page doclet - header and footer are implemented so extensions only
 * need to add doclets for the body of the page.
 *
 * Also has convenience methods for adding JS and CSS imports, and an onLoad JS action.
 */
public class DefaultHTMLPage<O> extends AbstractHTMLDoclet<O> {

    private List<URL> cssImports = new ArrayList<URL>();
    private List<URL> jsImports = new ArrayList<URL>();

    private String onload = "";
    private String focusedComponent;
    private String title = "Untitled";


    protected void renderHeader(URL pageURL, PrintWriter out) {
        out.println("<html><head>");

        out.println("<title>" + getTitle() + "</title>");

        out.println("<meta http-equiv='content-type' content='text/html;charset=" + getEncoding() + "'>");

        for (URL cssURL : getRequiredCSS()){
            out.println("<link rel='stylesheet' href='" + URLUtils.createRelativeURL(pageURL, cssURL) +
                        "' type='text/css' />");
        }

        for (URL jsURL : getRequiredJS()){
            out.println("<script src='" + URLUtils.createRelativeURL(pageURL, jsURL) +
                        "' type='text/javascript'></script>");
        }

        out.println("</head>");

        out.print("<body");

        String onloadJS = onload;
        if (onloadJS.length() > 0){
            onloadJS += ";";
        }
        if (focusedComponent != null){
            onloadJS += "document.getElementById(\"" + focusedComponent + "\").focus();";
        }
        if (onloadJS.length() > 0){
            out.print(" onload='" + onloadJS + "'");
        }
        out.println(">");
    }


    protected String getEncoding() {
        return OWLHTMLConstants.DEFAULT_ENCODING;
    }


    protected void renderFooter(URL pageURL, PrintWriter out) {
        out.println("</body></html>");
    }

    protected void addCSS(URL css){
        cssImports.add(css);
    }

    protected void addJavascript(URL js){
        jsImports.add(js);
    }

    public Set<URL> getRequiredCSS() {
        Set<URL> css = super.getRequiredCSS(); // pick up all required from doclets
        css.addAll(cssImports); // and any hand added ones
        return css;
    }

    public Set<URL> getRequiredJS() {
        Set<URL> js = super.getRequiredJS(); // pick up all required from doclets
        js.addAll(jsImports); // and any hand added ones
        return js;
    }

    public void addOnLoad(String jsAction) {
        this.onload += jsAction;
    }

    public void setAutoFocusedComponent(String focusedComponent) {
        this.focusedComponent = focusedComponent;
    }

    public void setTitle(String title){
        this.title = title;
    }

    protected String getTitle() {
        return OWLHTMLConstants.ONTOLOGY_SERVER_NAME + ": " + title;
    }

    public String getID() {
        return "doclet.page." + getTitle();
    }
}
