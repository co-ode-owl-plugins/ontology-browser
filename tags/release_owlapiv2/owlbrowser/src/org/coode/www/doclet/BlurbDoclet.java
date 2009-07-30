/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.www.doclet;

import org.coode.html.doclet.AbstractHTMLDoclet;
import org.coode.html.impl.OWLHTMLConstants;
import org.coode.www.OntologyBrowserConstants;

import java.io.PrintWriter;
import java.net.URL;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jan 25, 2008<br><br>
 */
public class BlurbDoclet extends AbstractHTMLDoclet {

    private static final String ID = "doclet.blurb";

    private static final String DOCUMENTATION_BOX = "Documentation";


    protected void renderHeader(URL pageURL, PrintWriter out) {
        out.println("<h1>" + OntologyBrowserConstants.ONTOLOGY_SERVER_NAME + " " + OntologyBrowserConstants.VERSION + "</h1>");
        renderBoxStart(DOCUMENTATION_BOX, out);
        out.println("<p style='font-weight: bold;/*text-decoration: underline;*/'><a href=\"" + OWLHTMLConstants.HOME_PAGE + "\" target=\"docs\">Ontology Browser Homepage</a><br />" +
                    "<a href=\"" + OWLHTMLConstants.HOME_PAGE + "wiki/GettingStarted\" target=\"docs\">Getting started</a><br />" +
                    "<a href=\"" + OWLHTMLConstants.HOME_PAGE + "wiki/ReleaseNotes\" target=\"docs\">Release notes</a><br />" +
                    "<a href=\"" + OWLHTMLConstants.HOME_PAGE + "issues/list\" target=\"docs\">Bugs/Feature requests</a></p>");
    }

    protected void renderFooter(URL pageURL, PrintWriter out) {
        renderBoxEnd(DOCUMENTATION_BOX, out);
    }

    public String getID() {
        return ID;
    }
}
