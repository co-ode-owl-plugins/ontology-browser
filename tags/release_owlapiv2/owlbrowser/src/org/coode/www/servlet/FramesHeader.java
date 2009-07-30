/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.www.servlet;

import org.coode.html.OWLHTMLServer;
import org.coode.html.doclet.HTMLDoclet;
import org.coode.html.doclet.MenuBarDoclet;
import org.coode.html.doclet.TabsDoclet;
import org.coode.html.impl.OWLHTMLConstants;
import org.coode.html.page.EmptyOWLDocPage;
import org.coode.owl.mngr.OWLServer;
import org.coode.www.exception.OntServerException;

import java.io.PrintWriter;
import java.net.URL;
import java.util.Collections;
import java.util.Map;
import java.util.Set;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jan 15, 2008<br><br>
 */
public class FramesHeader extends AbstractOntologyServerServlet {

    protected void handleXMLRequest(Map<String, String> params, OWLHTMLServer server, URL servletURL, PrintWriter out) throws OntServerException {
        // no implementation
    }

    protected HTMLDoclet handleHTMLRequest(Map<String, String> params, OWLHTMLServer server, URL pageURL) throws OntServerException {

        EmptyOWLDocPage ren = new EmptyOWLDocPage(server) {
            public String getTitle() {
                return "OWLDoc Frames Header";
            }

            public void renderContent(PrintWriter out) {
                out.println("<h2>Javascript Log:</h2>\n" +
                            "<textarea id=\"log\" style=\"width:100%\" rows=\"10\" cols=\"148\"></textarea>");
            }

            public Set<URL> getRequiredJS() {
                Set<URL> js = super.getRequiredJS();
                js.add(getServer().getURLScheme().getURLForRelativePage(OWLHTMLConstants.JS_DEFAULT)); // force the js to be available for the menus
                return js;
            }
        };

        // force the menus to be painted
        ren.addDoclet(new MenuBarDoclet(server));
        ren.addDoclet(new TabsDoclet(server));
        return ren;
    }

    protected Map<String, Set<String>> getRequiredParams(OWLServer server) {
        return Collections.emptyMap();
    }
}