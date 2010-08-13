package org.coode.www.servlet;

import org.coode.html.OWLHTMLKit;
import org.coode.html.doclet.HTMLDoclet;
import org.coode.html.impl.OWLHTMLParam;
import org.coode.owl.mngr.NamedObjectType;
import org.coode.www.exception.OntServerException;
import org.coode.www.exception.RedirectException;
import org.coode.www.page.FramesHTMLPage;

import java.io.PrintWriter;
import java.net.URL;
import java.util.Map;

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

    protected void handleXMLRequest(Map<OWLHTMLParam, String> params, OWLHTMLKit kit, URL servletURL, PrintWriter out) throws OntServerException {
        // no implementation
    }

    protected HTMLDoclet handleHTMLRequest(Map<OWLHTMLParam, String> params, OWLHTMLKit kit, URL pageURL) throws OntServerException {

        String page = params.get(OWLHTMLParam.content);

        
        if (isSingleFrameNavigation(kit)){
            if (page == null){
                throw new RedirectException(kit.getURLScheme().getURLForRelativePage(NamedObjectType.ontologies.getPluralRendering().toLowerCase()));
            }
            else{
                throw new RedirectException(kit.getURLScheme().getURLForRelativePage(page));
            }
        }
        else{
            if (page == null){
                return new FramesHTMLPage(kit.getURLScheme().getURLForOWLObject(kit.getOWLServer().getActiveOntology()), kit);
            }
            else{
                return new FramesHTMLPage(kit.getURLScheme().getURLForRelativePage(page), kit);
            }
        }
    }
}
