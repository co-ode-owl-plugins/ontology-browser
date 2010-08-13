/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.www.servlet;

import org.coode.html.OWLHTMLKit;
import org.coode.html.doclet.HTMLDoclet;
import org.coode.html.impl.OWLHTMLParam;
import org.coode.owl.mngr.OWLClassExpressionParser;
import org.coode.www.exception.OntServerException;
import org.coode.www.page.DLQueryHTMLPage;
import org.semanticweb.owlapi.model.OWLClassExpression;

import java.io.PrintWriter;
import java.net.URL;
import java.text.ParseException;
import java.util.Map;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jan 11, 2008<br><br>
 */
public class DLQuery extends AbstractOntologyServerServlet {


    protected void handleXMLRequest(Map<OWLHTMLParam, String> params, OWLHTMLKit kit, URL servletURL, PrintWriter out) throws OntServerException {
        //@@TODO implement
    }

    protected HTMLDoclet handleHTMLRequest(Map<OWLHTMLParam, String> params, OWLHTMLKit kit, URL pageURL) throws OntServerException {

        String q = params.get(OWLHTMLParam.expression);

        DLQueryHTMLPage acRenderer = new DLQueryHTMLPage(kit);
        acRenderer.setQuery(q);

        if (q != null){
            String syntax = params.get(OWLHTMLParam.syntax);    
            OWLClassExpressionParser parser = kit.getOWLServer().getClassExpressionParser(syntax);
            try {
                OWLClassExpression descr = parser.parse(q);
            }
            catch (ParseException e) {
                acRenderer.addError(e);
            }
        }

        return acRenderer;
    }
}
