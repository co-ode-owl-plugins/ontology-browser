/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.doclet;

import org.apache.log4j.Logger;
import org.coode.html.OWLHTMLServer;
import org.coode.html.impl.OWLHTMLConstants;
import org.coode.html.util.URLUtils;
import org.coode.owl.mngr.impl.FragmentShortFormProvider;

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


    public MenuBarDoclet(OWLHTMLServer server) {
        super(server);
    }

    protected void renderHeader(URL pageURL, PrintWriter out) {
        out.println("<div id='menu'>");

        renderOptions(pageURL, out);

        out.println("<a style='display: none;' href='#content'>skip to content</a> ");

        renderLink(OWLHTMLConstants.CONTENTS_LABEL, getServer().getURLScheme().getURLForRelativePage(OWLHTMLConstants.CONTENTS_HTML), OWLHTMLConstants.LinkTarget.content, "", isSingleFrameNavigation(), pageURL, out);
        out.print(" | ");
        renderLink(OWLHTMLConstants.MANAGE_LABEL, getServer().getURLScheme().getURLForRelativePage(OWLHTMLConstants.MANAGE_HTML), OWLHTMLConstants.LinkTarget.content, null, isSingleFrameNavigation(), pageURL, out);
        out.print(" | ");
        renderLink(OWLHTMLConstants.RESTART_LABEL, getServer().getURLScheme().getURLForRelativePage(OWLHTMLConstants.SIGNOUT_HTML), OWLHTMLConstants.LinkTarget._top, "", isSingleFrameNavigation(), pageURL, out);
    }

    protected void renderFooter(URL pageURL, PrintWriter out) {
        out.println("</div> <!-- menu -->");
    }

    public void addToMenu(MenuItemDoclet menuItem){
        addDoclet(menuItem); // will insert before the sign out item
    }

    private void renderOptions(URL pageURL, PrintWriter out) {
        String pageRelToBase = URLUtils.createRelativeURL(getServer().getBaseURL(), pageURL);
        String optionsURL = URLUtils.createRelativeURL(pageURL, getServer().getURLScheme().getURLForRelativePage(OWLHTMLConstants.OPTIONS_HTML));

        out.println("<div id='options'>");

        if (isSingleFrameNavigation()){
            try {
                String encodedURI = URLEncoder.encode(pageRelToBase, "UTF-8");
                String relOptionURL = URLUtils.createRelativeURL(pageURL, getServer().getURLScheme().getURLForRelativePage("?content=" + encodedURI));
                out.println("<a onclick=\"option('frames', 'true', '" + relOptionURL + "', '" + optionsURL + "');\">frames</a> | ");
            }
            catch (UnsupportedEncodingException e) {
                logger.error("Could not encode URL: " + pageRelToBase, e);
            }
        }
        else{
            out.println("<a onclick=\"option('frames', 'false', getContentURL(), '" + optionsURL + "');\">no frames</a> | ");
        }

        out.println("renderer: "); // only render the non-selected one as a link
        if (getServer().getNameRenderer().getClass().equals(FragmentShortFormProvider.class)){
            out.print(OWLHTMLConstants.RENDERER_FRAG + "/");
            out.println("<a onclick=\"option('ren', '" + OWLHTMLConstants.RENDERER_LABEL + "', null, '" + optionsURL + "');\">" + OWLHTMLConstants.RENDERER_LABEL + "</a>");
        }
        else{
            out.print("<a onclick=\"option('ren', '" + OWLHTMLConstants.RENDERER_FRAG + "', null, '" + optionsURL + "');\">" + OWLHTMLConstants.RENDERER_FRAG + "</a>/");
            out.println(OWLHTMLConstants.RENDERER_LABEL);
        }

        out.print(" | ");
        renderLink("more options", getServer().getURLScheme().getURLForRelativePage(OWLHTMLConstants.OPTIONS_HTML), OWLHTMLConstants.LinkTarget.content, null, isSingleFrameNavigation(), pageURL, out);

        out.println("</div> <!-- options -->");
    }

    public String getID() {
        return ID;
    }
}
