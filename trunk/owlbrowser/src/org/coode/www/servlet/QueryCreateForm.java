package org.coode.www.servlet;

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
public class QueryCreateForm{// extends AbstractOntologyServerServlet{
//
//    private static final String PARAM_EXPRESSION = "expression";
//    private static final String PARAM_SYNTAX = "syntax";
//
//    protected void handleXMLRequest(Map<String, String> params, OWLHTMLKit kit, URL servletURL, PrintWriter out) throws OntServerException {
//        // no implementation
//    }
//
//    protected HTMLDoclet handleHTMLRequest(Map<String, String> params, OWLHTMLKit kit, URL pageURL) throws OntServerException {
//        final String syntax = params.get(PARAM_SYNTAX);
//        final String expression = params.get(PARAM_EXPRESSION);
//
//        OWLClassExpression descr = kit.getOWLOntologyManager().getOWLDataFactory().getOWLThing();
//
//        if (expression != null){
//
//            OWLClassExpressionParser dParser = kit.getClassExpressionParser(syntax);
//            try {
//                descr = dParser.parse(expression);
//            }
//            catch (ParseException e) {
//                throw new OntServerException(e);
//            }
//        }
//
//        final SimpleQueryFormDoclet formDoclet = new SimpleQueryFormDoclet(kit);
//
//        OWLDocPage<OWLClassExpression> formRenderer = new OWLDocPage<OWLClassExpression>(kit);
//        formRenderer.setTitle("Query Form");
//        formRenderer.addOnLoad("setAddVisible(true); sendQuery(getCurrentQuery());");
//
//        formRenderer.addDoclet(formDoclet);
//        formRenderer.setUserObject(descr);
////                    if (!QUICK_DESCRIPTION_SYNTAX.equals(syntax)){
////                                formDoclet.setSuggestorManager(session.getSuggestorManager());
////                    }
//        return formRenderer;
//    }
//
//    protected Map<String, Set<String>> getRequiredParams(OWLServer kit) {
//        Map<String, Set<String>> params = new HashMap<String,  Set<String>>();
//        params.put(PARAM_EXPRESSION, Collections.singleton("<owl description>"));
//        params.put(PARAM_SYNTAX, kit.getSupportedSyntaxes());
//        return params;
//    }
}
