/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.www.servlet;

import org.coode.html.OWLHTMLKit;
import org.coode.html.doclet.HTMLDoclet;
import org.coode.html.doclet.MenuBarDoclet;
import org.coode.html.doclet.TabsDoclet;
import org.coode.html.impl.OWLHTMLConstants;
import org.coode.html.impl.OWLHTMLParam;
import org.coode.html.page.OWLDocPage;
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

    protected void handleXMLRequest(Map<OWLHTMLParam, String> params, OWLHTMLKit kit, URL servletURL, PrintWriter out) throws OntServerException {
        // no implementation
    }

    protected HTMLDoclet handleHTMLRequest(Map<OWLHTMLParam, String> params, OWLHTMLKit kit, URL pageURL) throws OntServerException {

        OWLDocPage ren = new OWLDocPage(kit) {
            public String getTitle() {
                return "OWLDoc Frames Header";
            }

            public void renderContent(PrintWriter out) {
                out.println("<h2>Javascript Log:</h2>\n" +
                            "<textarea id=\"log\" style=\"width:100%\" rows=\"10\" cols=\"148\"></textarea>");
            }

            public Set<URL> getRequiredJS() {
                Set<URL> js = super.getRequiredJS();
                js.add(getHTMLGenerator().getURLScheme().getURLForRelativePage(OWLHTMLConstants.JS_DEFAULT)); // force the js to be available for the menus
                return js;
            }
        };

        // force the menus to be painted
        ren.addDoclet(new MenuBarDoclet(kit));
        ren.addDoclet(new TabsDoclet(kit));
        return ren;
    }

    protected Map<OWLHTMLParam, Set<String>> getRequiredParams(OWLServer server) {
        return Collections.emptyMap();
    }
}