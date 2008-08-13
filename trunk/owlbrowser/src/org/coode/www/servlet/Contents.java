package org.coode.www.servlet;

import org.coode.html.OWLHTMLServer;
import org.coode.html.doclet.HTMLDoclet;
import org.coode.html.doclet.TabsDoclet;
import org.coode.html.index.OWLContentsHTMLPage;
import org.coode.owl.mngr.OWLServer;
import org.coode.www.doclet.CloudIndexDoclet;
import org.coode.www.doclet.SearchOntologiesDoclet;
import org.coode.www.exception.OntServerException;

import java.io.PrintWriter;
import java.net.URL;
import java.util.Collections;
import java.util.Map;
import java.util.Set;

/**
 * Author: Nick Drummond<br>
 * nick.drummond@cs.manchester.ac.uk<br>
 * http://www.cs.man.ac.uk/~drummond<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jun 20, 2007<br><br>
 * <p/>
 * code made available under Mozilla Public License (http://www.mozilla.org/MPL/MPL-1.1.html)<br>
 * copyright 2006, The University of Manchester<br>
 */
public class Contents extends AbstractOntologyServerServlet {

    private static final String SEARCH_BOX_ID = "searchBox";

    protected void handleXMLRequest(Map<String, String> params, OWLHTMLServer server, URL servletURL, PrintWriter out) throws OntServerException {
        // not implemented
    }

    protected HTMLDoclet handleHTMLRequest(Map<String, String> params, OWLHTMLServer server, URL pageURL) throws OntServerException {
        try {
            OWLContentsHTMLPage ren = new OWLContentsHTMLPage(server);

            ren.setAutoFocusedComponent(SEARCH_BOX_ID);

            int insertPoint = -1;
            final HTMLDoclet tabs = ren.getDoclet(TabsDoclet.ID);
            if (tabs != null){
                insertPoint = ren.indexOf(tabs);
            }
            ren.addDoclet(new SearchOntologiesDoclet(server, SEARCH_BOX_ID), insertPoint+1);

            ren.addDoclet(new CloudIndexDoclet(server)); // after all contents

            return ren;
        }
        catch (Exception e) {
            throw new OntServerException(e);
        }
    }

    protected Map<String, Set<String>> getRequiredParams(OWLServer server) {
        return Collections.emptyMap(); // no params
    }
}
