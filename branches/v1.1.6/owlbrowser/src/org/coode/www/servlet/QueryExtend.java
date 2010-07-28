package org.coode.www.servlet;

/**
 * Author: drummond<br>
 * The University Of Manchester<br>
 * Medical Informatics Group<br>
 * Date: Jul 11, 2006<br><br>
 * <p/>
 * nick.drummond@cs.manchester.ac.uk<br>
 * www.cs.man.ac.uk/~drummond<br><br>
 */
public class QueryExtend {// extends AbstractOntologyServerServlet {
//
//    private static final String PARAM_EXPRESSION = "expression";
//    private static final String PARAM_SYNTAX = "syntax";
//
//    protected void handleXMLRequest(Map<String, String> params, OWLHTMLKit kit, URL servletURL, PrintWriter out) throws OntServerException {
//        // no implementation
//    }
//
//    protected HTMLDoclet handleHTMLRequest(Map<String, String> params, OWLHTMLKit kit, URL pageURL) throws OntServerException {
//
//        String expression = params.get(PARAM_EXPRESSION);
//        String syntax = params.get(PARAM_SYNTAX);
//
//        try {
//            ConjunctDoclet ren = null;
//
//            OWLClassExpression descr = kit.getClassExpressionParser(syntax).parse(expression);
//            if (descr != null){
//                Set<OWLProperty> properties = new HashSet<OWLProperty>();
//                final PropertySuggestor ps = getSuggestorManager(kit).getPropertySuggestor();
//                for (OWLPropertyExpression prop : ps.getPossibleProperties(descr, false)){
//                    if (prop instanceof OWLProperty){
//                        properties.add((OWLProperty)prop);
//                    }
//                }
//
//                // fillers should be blank until a property is selected
//                Set<OWLPropertyRange> fillers = new HashSet<OWLPropertyRange>();
//
//                ren = new ConjunctDoclet(properties, fillers, kit);
//            }
//
//            if (ren == null){
//                ren = new ConjunctDoclet(kit);
//            }
//
//            ren.setRenderRemoveButton(true);
//            return ren;
//        }
//        catch(Exception e){
//            throw new OntServerException(e);
//        }
//    }
//
//    protected Map<String, Set<String>> getRequiredParams(OWLServer kit) {
//        Map<String, Set<String>> params = new HashMap<String,  Set<String>>();
//        params.put(PARAM_EXPRESSION, Collections.singleton("<owl description>"));
//        params.put(PARAM_SYNTAX, kit.getSupportedSyntaxes());
//        return params;
//    }
}

