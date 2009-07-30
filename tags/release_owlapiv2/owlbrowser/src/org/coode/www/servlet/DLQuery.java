/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.www.servlet;

import org.coode.html.OWLHTMLServer;
import org.coode.html.doclet.HTMLDoclet;
import org.coode.owl.mngr.OWLDescriptionParser;
import org.coode.owl.mngr.OWLServer;
import org.coode.www.exception.OntServerException;
import org.coode.www.page.DLQueryHTMLPage;
import org.semanticweb.owl.model.OWLDescription;

import java.io.PrintWriter;
import java.net.URL;
import java.text.ParseException;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jan 11, 2008<br><br>
 */
public class DLQuery extends AbstractOntologyServerServlet {

    private static final String PARAM_SYNTAX = "syntax";

    protected void handleXMLRequest(Map<String, String> params, OWLHTMLServer server, URL servletURL, PrintWriter out) throws OntServerException {
        //@@TODO implement
    }

    protected HTMLDoclet handleHTMLRequest(Map<String, String> params, OWLHTMLServer server, URL pageURL) throws OntServerException {
        String q = params.get(DLQueryHTMLPage.PARAM_EXPRESSION);

        DLQueryHTMLPage acRenderer = new DLQueryHTMLPage(server);
        acRenderer.setQuery(q);

        if (q != null){
            String syntax = params.get(PARAM_SYNTAX);
            OWLDescriptionParser parse = server.getDescriptionParser(syntax);
            try {
                OWLDescription descr = parse.parse(q);
            }
            catch (ParseException e) {
                acRenderer.addError(e);
            }
        }

        return acRenderer;
    }

    protected Map<String, Set<String>> getRequiredParams(OWLServer server) {
        Map<String, Set<String>> required = new HashMap<String, Set<String>>();
//        required.put(PARAM_EXPRESSION, Collections.singleton("<owl description>"));
        return required;
    }
}
