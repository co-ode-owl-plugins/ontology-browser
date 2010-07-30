/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.doclet;

import org.apache.log4j.Logger;
import org.coode.html.OWLHTMLKit;
import org.coode.html.impl.OWLHTMLConstants;
import org.coode.html.url.URLScheme;
import org.coode.html.util.URLUtils;
import org.coode.owl.mngr.ServerProperty;
import org.semanticweb.owlapi.util.AnnotationValueShortFormProvider;
import org.semanticweb.owlapi.util.ShortFormProvider;

import java.io.PrintWriter;
import java.net.URL;

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

        final OWLHTMLKit kit = getOWLHTMLKit();
        final URLScheme urlScheme = kit.getURLScheme();

        out.println("\n\n<div id='menu'>");

        out.println("<a style='display: none;' href='#content'>skip to content</a> ");

        out.print("<a id='signout' href='");
        out.print(urlScheme.getURLForRelativePage(OWLHTMLConstants.SIGNOUT_HTML));
        out.print("' target='");
        out.print(OWLHTMLConstants.LinkTarget._top);
        out.print("'>");
        out.print("<img src='");
        out.print(urlScheme.getURLForRelativePage("images/close.png"));
        out.print("' width='16' height='16' title='close' />");
        out.println("</a>");
    }

    protected void renderFooter(URL pageURL, PrintWriter out) {

        renderOptions(pageURL, out);

        out.println("</div> <!-- menu -->\n");
    }

    private void renderOptions(URL pageURL, PrintWriter out) {
        out.println("\n<div id='options'>");

        final URL optionsURL = getOWLHTMLKit().getURLScheme().getURLForRelativePage(OWLHTMLConstants.OPTIONS_HTML);

        renderLink("Options", optionsURL, OWLHTMLConstants.LinkTarget.content, null, isSingleFrameNavigation(), pageURL, out);

        out.print("| <form id='");
        out.print(RENDERER_FORM);
        out.println("' style='display: inline;'>");

        out.print("<label for='");
        out.print(RENDERER_NAME);
        out.println("'>Render labels</label>");

        out.print("<input type='checkbox' name='");
        out.print(RENDERER_NAME);
        out.print("' id=");
        out.print(RENDERER_NAME);
        out.print("' onclick='");
        printCheckAction(URLUtils.createRelativeURL(pageURL, optionsURL), out);
        out.print("'");
        if (isRenderLabels()){
            out.print(" checked='checked'");
        }
        out.println(" />");

        out.println("</form>");

        out.println("</div> <!-- options -->\n");
    }

    private boolean isRenderLabels() {
        final ShortFormProvider sfp = getOWLHTMLKit().getOWLServer().getShortFormProvider();
        return sfp.getClass().equals(AnnotationValueShortFormProvider.class);
    }


    private void printCheckAction(String optionsURL, PrintWriter out) {
        out.print("var rendererName = \"");
        out.print(OWLHTMLConstants.RENDERER_FRAG);
        out.println("\";");
        out.print("if (document.getElementById(\"");
        out.print(RENDERER_FORM);
        out.print("\").");
        out.print(RENDERER_NAME);
        out.print(".checked == true){rendererName = \"");
        out.print(OWLHTMLConstants.RENDERER_LABEL);
        out.print("\";}option(\"");
        out.print(ServerProperty.optionRenderer.name());
        out.print("\", rendererName, null);");
    }


    public String getID() {
        return ID;
    }
}
