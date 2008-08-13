package org.coode.www.servlet;

import org.coode.html.OWLHTMLServer;
import org.coode.html.doclet.HTMLDoclet;
import org.coode.owl.mngr.OWLDescriptionParser;
import org.coode.owl.mngr.OWLServer;
import org.coode.www.QueryType;
import org.coode.www.doclet.ReasonerResultsDoclet;
import org.coode.www.exception.OntServerException;
import org.semanticweb.owl.model.OWLDescription;
import org.semanticweb.owl.model.OWLNamedObject;

import java.io.PrintWriter;
import java.net.URL;
import java.text.ParseException;
import java.util.*;
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
 * Date: Jul 17, 2007<br><br>
 */
public class QueryReasoner extends AbstractOntologyServerServlet {

    private static final String PARAM_QUERY = "query";
    private static final String PARAM_EXPRESSION = "expression";
    private static final String PARAM_SYNTAX = "syntax";

    protected void handleXMLRequest(Map<String, String> params,
                                    OWLHTMLServer server, URL servletURL, PrintWriter out) throws OntServerException {
        final String query = params.get(PARAM_QUERY);

        OWLDescription classDescription = parse(params.get(PARAM_EXPRESSION), params.get(PARAM_SYNTAX), server);

        final Set<OWLNamedObject> results = new HashSet<OWLNamedObject>(QueryType.valueOf(query).getResults(classDescription, server));

        renderXMLResults(results, server, out);
    }

    protected HTMLDoclet handleHTMLRequest(Map<String, String> params, OWLHTMLServer server, URL pageURL) throws OntServerException {
        final String query = params.get(PARAM_QUERY);
        OWLDescription classDescription = parse(params.get(PARAM_EXPRESSION), params.get(PARAM_SYNTAX), server);
        return new ReasonerResultsDoclet(QueryType.valueOf(query), classDescription, server);
    }

    protected Map<String, Set<String>> getRequiredParams(OWLServer server) {
        Map<String, Set<String>> params = new HashMap<String,  Set<String>>();
        params.put(PARAM_QUERY, getQueryTypes());
        params.put(PARAM_EXPRESSION, Collections.singleton("<owl description>"));
        params.put(PARAM_SYNTAX, server.getSupportedSyntaxes());
        return params;
    }

    private Set<String> getQueryTypes(){
        Set<String> renderings = new HashSet<String>();
        for (QueryType type : QueryType.values()){
            renderings.add(type.toString());
        }
        return renderings;
    }

    private OWLDescription parse(String expression, String syntax, OWLHTMLServer server) throws OntServerException {
        try {
            final OWLDescriptionParser parser = server.getDescriptionParser(syntax);
            return parser.parse(expression);
        }
        catch (ParseException e1) {
            throw new OntServerException(e1);
        }
    }
}
