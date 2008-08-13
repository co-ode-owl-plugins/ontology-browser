package org.coode.www.servlet;

import org.coode.html.OWLHTMLServer;
import org.coode.html.doclet.HTMLDoclet;
import org.coode.html.impl.OWLHTMLConstants;
import org.coode.owl.mngr.OWLServer;
import org.coode.www.exception.OntServerException;
import org.coode.www.exception.RedirectException;
import org.coode.www.page.FramesHTMLPage;

import java.io.PrintWriter;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 * Author: Nick Drummond<br>
 * nick.drummond@cs.manchester.ac.uk<br>
 * http://www.cs.man.ac.uk/~drummond<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jun 19, 2007<br><br>
 * <p/>
 * code made available under Mozilla Public License (http://www.mozilla.org/MPL/MPL-1.1.html)<br>
 * copyright 2006, The University of Manchester<br>
 *
 * HTML application Frame renderer - in code to avoid hard coding the page URLs
 */
public class Index extends AbstractOntologyServerServlet {

    private static final String PARAM_CONTENT = "content";

    protected void handleXMLRequest(Map<String, String> params, OWLHTMLServer server, URL servletURL, PrintWriter out) throws OntServerException {
        // no implementation
    }

    protected HTMLDoclet handleHTMLRequest(Map<String, String> params, OWLHTMLServer server, URL pageURL) throws OntServerException {
        String page = params.get(PARAM_CONTENT);

        if (isSingleFrameNavigation(server)){
            if (page == null){
                throw new RedirectException(server.getURLScheme().getURLForRelativePage(OWLHTMLConstants.CONTENTS_HTML));
            }
            else{
                throw new RedirectException(server.getURLScheme().getURLForRelativePage(page));
            }
        }
        else{
            if (page == null){
                return new FramesHTMLPage(server.getURLScheme().getURLForNamedObject(server.getActiveOntology()), server);
            }
            else{
                return new FramesHTMLPage(server.getURLScheme().getURLForRelativePage(page), server);
            }
        }
    }


    protected Map<String, Set<String>> getRequiredParams(OWLServer server) {
        Map<String, Set<String>> required = new HashMap<String, Set<String>>();
//        required.put(PARAM_CONTENT, Collections.singleton("<page url>")); optional
        return required;
    }
}
