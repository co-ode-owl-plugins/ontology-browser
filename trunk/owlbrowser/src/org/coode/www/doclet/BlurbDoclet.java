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

    protected void renderHeader(URL pageURL, PrintWriter out) {
        out.println("<h1>" + OntologyBrowserConstants.ONTOLOGY_SERVER_NAME + " " + OntologyBrowserConstants.VERSION + "</h1>");
        renderBoxStart("Author", out);
        out.println("<p>Nick Drummond, The University of Manchester</p>\n" +
                    "<p>Made available as part of the <a href=\"http://www.co-ode.org/downloads/owldoc-server/\">CO-ODE</a> project.</p>" +
                    "<p><a href=\"" + OWLHTMLConstants.ONTOLOGY_SERVER + "docs/index.html\">Documentation</a></p>");
    }

    protected void renderFooter(URL pageURL, PrintWriter out) {
        renderBoxEnd("Author", out);
    }

    public String getID() {
        return ID;
    }
}
