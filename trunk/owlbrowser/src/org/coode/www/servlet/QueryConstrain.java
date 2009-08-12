package org.coode.www.servlet;

/**
 * Author: drummond<br>
 * The University Of Manchester<br>
 * Medical Informatics Group<br>
 * Date: Jul 13, 2006<br><br>
 * <p/>
 * nick.drummond@cs.manchester.ac.uk<br>
 * www.cs.man.ac.uk/~drummond<br><br>
 */
public class QueryConstrain{// extends AbstractOntologyServerServlet {
//
//    private static final String PARAM_PROPERTY = "property";
//    private static final String PARAM_EXPRESSION = "expression";
//    private static final String PARAM_SYNTAX = "syntax";
//    private static final String PARAM_FILLER = "filler";
//
//    protected void handleXMLRequest(Map<String, String> params, OWLHTMLKit kit, URL servletURL, PrintWriter out) throws OntServerException {
//        //@@TODO implement
//    }
//
//    protected HTMLDoclet handleHTMLRequest(Map<String, String> params, OWLHTMLKit kit, URL pageURL) throws OntServerException {
//        try{
//            String propertyURI = params.get(PARAM_PROPERTY);
//            String expression = params.get(PARAM_EXPRESSION);
//            String syntax = params.get(PARAM_SYNTAX);
//            String current = params.get(PARAM_FILLER);
//
//            Set<OWLProperty> props = kit.getFinder().getOWLProperties(propertyURI);
//            OWLProperty prop = props.iterator().next();
//
//            if (prop instanceof OWLObjectProperty){
//                Set<OWLEntity> results = getResults(expression, syntax, (OWLObjectProperty)prop, kit);
//
//                OWLEntity currentSelection = kit.getOWLOntologyManager().getOWLDataFactory().getOWLClass(new URI(current));
//
//                return new SelectorContentsDoclet(results, currentSelection, kit.getShortFormProvider(), kit);
//            }
////            else{
////                Set<OWLObject> results = fs.getSanctionedFillers(descr, prop, true);
////            }
//
//        }
//        catch (Throwable e){
//            throw new OntServerException(e);
//        }
//        return null;
//    }
//
//
//    protected Map<String, Set<String>> getRequiredParams(OWLServer kit) {
//        Map<String, Set<String>> required = new HashMap<String, Set<String>>();
//        required.put(PARAM_PROPERTY, Collections.singleton("<object property uri>"));
//        required.put(PARAM_EXPRESSION, Collections.singleton("<owl description>"));
//        required.put(PARAM_SYNTAX, kit.getSupportedSyntaxes());
//        required.put(PARAM_FILLER, Collections.singleton("<class uri>"));
//        return required;
//    }
//
//    private Set<OWLEntity> getResults(String expression, String syntax, OWLObjectProperty prop, OWLHTMLKit kit) throws Exception {
//        Set<OWLEntity> results;
//
//        // @@TODO below is clearly wrong - all of this needs reworking
//        if (ServerConstants.SOME.equals(syntax)){
//            OWLClassExpression descr = kit.getClassExpressionParser(syntax).parse(expression);
//            final FillerSuggestor fs = getSuggestorManager(kit).getFillerSuggestor();
//            results = new HashSet<OWLEntity>(fs.getSanctionedNamedFillers(descr, prop));
//        }
//        else{
//            results = naiveLocalRestr(prop, syntax, kit);
//        }
//
//        if (results != null && results.size() == 0){
//            for (OWLOntology ont: kit.getOntologies()){
//                if (ServerConstants.HAS_VALUE.equals(syntax)){
//                    results.addAll(ont.getReferencedIndividuals());
//                }
//                else{
//                    results.addAll(ont.getReferencedClasses());
//                }
//            }
//        }
//        return results;
//    }
//
//    private Set<OWLEntity> naiveLocalRestr(OWLObjectProperty prop, String type, OWLServer kit) throws Exception {
//        final OWLReasoner r = kit.getOWLReasoner();
//
//        HashSet<OWLEntity> results = new HashSet<OWLEntity>();
//
//        for (OWLOntology ont: kit.getOntologies()){
//            Set<OWLClassExpression> ranges = prop.getRanges(ont);
//            if (ranges.size() > 0){
//                for (OWLClassExpression range: ranges){
//
//                    if (range instanceof OWLClass){
//                        Set<OWLClass> desc = new HashSet<OWLClass>();
//                        for (Set<OWLClass> equivs : r.getDescendantClasses(range)){
//                            desc.addAll(equivs);
//                        }
//                        desc.add((OWLClass)range);
//                        for (OWLClass cls : desc){
//                            if (ServerConstants.HAS_VALUE.equals(type)){
//                                results.addAll(r.getIndividuals(cls, false));
//                            }
//                            else{
//                                results.add(cls);
//                            }
//                        }
//                    }
//                }
//            }
//        }
//        return results;
//    }
}
