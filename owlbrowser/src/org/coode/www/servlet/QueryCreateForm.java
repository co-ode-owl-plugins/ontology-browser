package org.coode.www.servlet;

import org.coode.html.OWLHTMLServer;
import org.coode.html.doclet.HTMLDoclet;
import org.coode.html.page.EmptyOWLDocPage;
import org.coode.owl.mngr.OWLDescriptionParser;
import org.coode.owl.mngr.OWLServer;
import org.coode.www.doclet.SimpleQueryFormDoclet;
import org.coode.www.exception.OntServerException;
import org.semanticweb.owl.model.OWLDescription;

import java.io.PrintWriter;
import java.net.URL;
import java.text.ParseException;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
/*
* Copyright (C) 2007, University of Manchester
*
* Modifications to the initial code base are copyright of their
* respective authors, or their employers as appropriate.  Authorship
* of the modifications may be determined from the ChangeLog placed at
* the end of this file.
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.

* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.

* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*/

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jul 24, 2007<br><br>
 *
 * PARAMS:
 * syntax - eg "qd", "man" - from ServerConstants
 */
public class QueryCreateForm extends AbstractOntologyServerServlet{

    private static final String PARAM_EXPRESSION = "expression";
    private static final String PARAM_SYNTAX = "syntax";

    protected void handleXMLRequest(Map<String, String> params, OWLHTMLServer server, URL servletURL, PrintWriter out) throws OntServerException {
        // no implementation
    }

    protected HTMLDoclet handleHTMLRequest(Map<String, String> params, OWLHTMLServer server, URL pageURL) throws OntServerException {
        final String syntax = params.get(PARAM_SYNTAX);
        final String expression = params.get(PARAM_EXPRESSION);

        OWLDescription descr = server.getOWLOntologyManager().getOWLDataFactory().getOWLThing();

        if (expression != null){

            OWLDescriptionParser dParser = server.getDescriptionParser(syntax);
            try {
                descr = dParser.parse(expression);
            }
            catch (ParseException e) {
                throw new OntServerException(e);
            }
        }

        final SimpleQueryFormDoclet formDoclet = new SimpleQueryFormDoclet(server);

        EmptyOWLDocPage<OWLDescription> formRenderer = new EmptyOWLDocPage<OWLDescription>(server);
        formRenderer.setTitle("Query Form");
        formRenderer.addOnLoad("setAddVisible(true); sendQuery(getCurrentQuery());");

        formRenderer.addDoclet(formDoclet);
        formRenderer.setUserObject(descr);
//                    if (!QUICK_DESCRIPTION_SYNTAX.equals(syntax)){
//                                formDoclet.setSuggestorManager(session.getSuggestorManager());
//                    }
        return formRenderer;
    }

    protected Map<String, Set<String>> getRequiredParams(OWLServer server) {
        Map<String, Set<String>> params = new HashMap<String,  Set<String>>();
        params.put(PARAM_EXPRESSION, Collections.singleton("<owl description>"));
        params.put(PARAM_SYNTAX, server.getSupportedSyntaxes());
        return params;
    }
}
