/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.doclet;

import org.apache.log4j.Logger;
import org.coode.html.OWLHTMLKit;
import org.coode.html.impl.OWLHTMLConstants;
import org.coode.html.impl.OWLHTMLProperty;
import org.coode.html.util.URLUtils;
import org.coode.owl.mngr.ServerProperty;
import org.semanticweb.owlapi.util.SimpleShortFormProvider;

import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.net.URL;
import java.net.URLEncoder;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jan 24, 2008<br><br>
 */
public class MenuBarDoclet extends AbstractOWLDocDoclet {

    private static final Logger logger = Logger.getLogger(MenuBarDoclet.class);

    public static final String ID = "doclet.menubar";

    private static final String RENDERER_NAME = "renderLabels";

    private static final String RENDERER_FORM = "rendererForm";


    public MenuBarDoclet(OWLHTMLKit kit) {
        super(kit);
    }

    protected void renderHeader(URL pageURL, PrintWriter out) {
        out.println("<div id='menu'>");

        renderOptions(pageURL, out);

        out.println("<a style='display: none;' href='#content'>skip to content</a> ");

        renderLink(OWLHTMLConstants.CONTENTS_LABEL, getHTMLGenerator().getURLScheme().getURLForRelativePage(OWLHTMLConstants.CONTENTS_HTML), OWLHTMLConstants.LinkTarget.content, "", isSingleFrameNavigation(), pageURL, out);
        out.print(" | ");
        renderLink(OWLHTMLConstants.MANAGE_LABEL, getHTMLGenerator().getURLScheme().getURLForRelativePage(OWLHTMLConstants.MANAGE_HTML), OWLHTMLConstants.LinkTarget.content, null, isSingleFrameNavigation(), pageURL, out);
        out.print(" | ");
        renderLink(OWLHTMLConstants.RESTART_LABEL, getHTMLGenerator().getURLScheme().getURLForRelativePage(OWLHTMLConstants.SIGNOUT_HTML), OWLHTMLConstants.LinkTarget._top, "", isSingleFrameNavigation(), pageURL, out);
    }

    protected void renderFooter(URL pageURL, PrintWriter out) {
        out.println("</div> <!-- menu -->");
    }

    public void addToMenu(MenuItemDoclet menuItem){
        addDoclet(menuItem); // will insert before the sign out item
    }

    private void renderOptions(URL pageURL, PrintWriter out) {
        String pageRelToBase = URLUtils.createRelativeURL(getHTMLGenerator().getBaseURL(), pageURL);
        String optionsURL = URLUtils.createRelativeURL(pageURL, getHTMLGenerator().getURLScheme().getURLForRelativePage(OWLHTMLConstants.OPTIONS_HTML));

        out.println("<div id='options'>");

        if (isSingleFrameNavigation()){
            try {
                String encodedURI = URLEncoder.encode(pageRelToBase, OWLHTMLConstants.DEFAULT_ENCODING);
                String relOptionURL = URLUtils.createRelativeURL(pageURL, getHTMLGenerator().getURLScheme().getURLForRelativePage("?content=" + encodedURI));
                out.println("<a onclick=\"option('" + OWLHTMLProperty.optionUseFrames + "', 'true', '" + relOptionURL + "', '" + optionsURL + "');\">frames</a> | ");
            }
            catch (UnsupportedEncodingException e) {
                logger.error("Could not encode URL: " + pageRelToBase, e);
            }
        }
        else{
            out.println("<a onclick=\"option('" + OWLHTMLProperty.optionUseFrames + "', 'false', getContentURL(), '" + optionsURL + "');\">hide frames</a> | ");
        }

        final boolean renderLabels = !getHTMLGenerator().getOWLServer().getShortFormProvider().getClass().equals(SimpleShortFormProvider.class);
        out.println("<form id='" + RENDERER_FORM + "' style='display: inline;'>");
        out.println("<label for='" + RENDERER_NAME + "' />Render labels</label>");
        out.println("<input type='checkbox' name='"+ RENDERER_NAME +"' onclick='" + renderCheckAction(optionsURL) + "'");
        if (renderLabels){
            out.println(" checked='checked'");
        }
        out.println(" />");
        out.println("</form>");

        out.print(" | ");
        renderLink("more options", getHTMLGenerator().getURLScheme().getURLForRelativePage(OWLHTMLConstants.OPTIONS_HTML), OWLHTMLConstants.LinkTarget.content, null, isSingleFrameNavigation(), pageURL, out);

        out.println("</div> <!-- options -->");
    }


    private String renderCheckAction(String optionsURL) {
        return "var rendererName = \"" + OWLHTMLConstants.RENDERER_FRAG + "\";" +
               "if (document.getElementById(\"" + RENDERER_FORM + "\")." + RENDERER_NAME + ".checked == true){" +
               "rendererName = \"" + OWLHTMLConstants.RENDERER_LABEL + "\";" +
               "}" +
               "option(\"" + ServerProperty.optionRenderer.name() + "\", rendererName, null, \"" + optionsURL + "\");";
    }


    public String getID() {
        return ID;
    }
}
