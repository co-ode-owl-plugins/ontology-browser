package org.coode.www.query;

import org.apache.log4j.Logger;
import org.coode.owl.mngr.OWLDescriptionParser;
import org.coode.owl.mngr.OWLServer;
import org.coode.owl.mngr.ServerConstants;
import org.coode.owl.util.ModelUtil;
import org.semanticweb.owl.inference.OWLClassReasoner;
import org.semanticweb.owl.inference.OWLReasonerException;
import org.semanticweb.owl.model.*;
import org.semanticweb.owl.util.OWLObjectVisitorAdapter;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

/**
 * Attempts to find "sensible" rather than logically correct answers to the query.
 * Could be thought of as "Which X might reasonably have these features?"
 * Or "Which classes could contain SOME individuals for which this is true?"
 *
 * eg
 *
 * Topping and isIngredientOf some FruttiDiMare
 *  logically, no ingredients are always ingredients of the Frutti Di MAre pizza
 *  realistically, Frutti Di Mare hasToppings MixedSeafood, Tomato and Garlic
 *  this is what we wish to return -
 *
 *
 * Author: drummond<br>
 * The University Of Manchester<br>
 * Medical Informatics Group<br>
 * Date: Jul 13, 2006<br><br>
 * <p/>
 * nick.drummond@cs.manchester.ac.uk<br>
 * www.cs.man.ac.uk/~drummond<br><br>
 */
public class StructureQuery {

    private static final Logger logger = Logger.getLogger(StructureQuery.class.getName());

    private Set<OWLEntity> results = new HashSet<OWLEntity>();

    private MyOWLDescriptionVisitorAdapter descrVisitor;

    private OWLClass baseClass;

    private OWLServer server;

    private OWLClassReasoner r;

    public StructureQuery(String query, String syntax, OWLServer server) throws Exception{

        this.server = server;

        OWLDescriptionParser parser = server.getDescriptionParser(syntax);

        //this.r = session.getOntologyServer().getOWLClassReasoner();

        descrVisitor = new MyOWLDescriptionVisitorAdapter();

        // @@TODO need to test thoroughly with multiple conjuncts
        for (String token: query.split(ServerConstants.INTERSECTION)){
            descrVisitor.reset();

            OWLDescription descr = parser.parse(token.trim());
            descr.accept(descrVisitor);

            // merge the values into the results performing AND operation
            if (descrVisitor.getVisitedRestriction()){
                Set<OWLEntity> newValues = descrVisitor.getValues();
                if (newValues.size() > 0){
                    if (results.size() > 0){
                        for (Iterator<OWLEntity> i = results.iterator(); i.hasNext();){
                            OWLEntity result = i.next();
                            if (!newValues.contains(result)){
                                i.remove();
                            }
                        }
                    }
                    else{
                        results.addAll(newValues);
                    }
                }
                else{
                    results.clear();
                    break;
                }
            }
        }
    }

    public Set<OWLEntity> getResults() {
        return results;
    }

    /**
     * Get the some value restrictions on a given class, including the inherited restrictions
     */
    private Set<OWLObjectSomeRestriction> getSomeValueRestrictions(OWLClass aClass) {

        Set<OWLObjectSomeRestriction> results = new HashSet<OWLObjectSomeRestriction>();

        try {

            Set<OWLClass> ancestors = ModelUtil.filterClasses(r.getAncestorClasses(aClass), server);
            ancestors.add(aClass);

            for (OWLClass ancestor : ancestors) {
                for (OWLOntology ont : server.getOntologies()){
                    for (OWLDescription s: ancestor.getSuperClasses(ont)){
                        if (s instanceof OWLObjectSomeRestriction) {
                            results.add((OWLObjectSomeRestriction) s);
                        }
                    }
                }
            }
        }
        catch (OWLException e) {
            logger.error("Caught Exception: ", e);
        }

        return results;
    }

    /**
     * Get the some value restrictions on a given class and its subclasses
     */
    private Set<OWLObjectSomeRestriction> getSubclassSomeValueRestrictions(OWLClass aClass) {

        Set<OWLObjectSomeRestriction> results = new HashSet<OWLObjectSomeRestriction>();

        try {

            Set<OWLClass> descendents = ModelUtil.filterClasses(r.getDescendantClasses(aClass), server);

            for (OWLClass descendent : descendents) {
                for (OWLOntology ont : server.getOntologies()){
                    for (OWLDescription s: descendent.getSuperClasses(ont)){
                        if (s instanceof OWLObjectSomeRestriction) {
                            results.add((OWLObjectSomeRestriction) s);
                        }
                    }
                }
            }
        }
        catch (OWLReasonerException e) {
            logger.error("Caught Exception: ", e);
        }

        return results;
    }

    /**
     * Return a set of (named) fillers for restrictions along the given inverseProps
     */
    private Set<OWLEntity> getSomeToAllRelations(OWLClass cls, OWLObjectPropertyExpression inverseProp) {

        Set<OWLEntity> inverseFillers = new HashSet<OWLEntity>();

        Set<OWLObjectSomeRestriction> restrs = getSomeValueRestrictions(cls);
        restrs.addAll(getSubclassSomeValueRestrictions(cls));

        for (OWLObjectSomeRestriction restr: restrs){

            OWLObjectPropertyExpression restrictedProp = restr.getProperty();

            if (restrictedProp instanceof OWLObjectProperty){

                OWLDescription filler = restr.getFiller();

                if (filler instanceof OWLClass){

//                    if (r.isSubClassOf(filler, baseClass)){
//
//                        Set<OWLPropertyExpression> rPropsupers = r.getSuperProperties(restrictedProp);
//                        rPropsupers.add(restrictedProp);
//
//                        for (OWLObjectProperty hasIngredientProp : inverseProps){
//                            // if the restricted property is a sub property of this inverse
//                            if (rPropsupers.contains(hasIngredientProp)){
//                                inverseFillers.add((OWLClass)filler);
//                            }
//                        }
//                    }
                }
            }
        }
        return inverseFillers;
    }
    //
    private class MyOWLDescriptionVisitorAdapter extends OWLObjectVisitorAdapter {
        private Set<OWLEntity> accumulator = new HashSet<OWLEntity>();
        private boolean visitedRestriction = false;
        private OWLOntologyManager mngr;

        public void setOntology(OWLOntologyManager mngr){
            this.mngr = mngr;
        }

        public Set<OWLEntity>getValues(){
            return accumulator;
        }

        public void reset(){
            accumulator.clear();
            visitedRestriction = false;
        }

        public boolean getVisitedRestriction(){
            return visitedRestriction;
        }

        public void visit(OWLClass owlClass) {
            baseClass = owlClass;

            logger.debug("baseClass = " + baseClass);
        }

        public void visit(OWLObjectSomeRestriction someRestr) {
            visitedRestriction = true;

            OWLObjectPropertyExpression prop = someRestr.getProperty();

            OWLObjectPropertyExpression inverse = mngr.getOWLDataFactory().getOWLObjectPropertyInverse(prop);

            OWLDescription filler = someRestr.getFiller();

            if (filler instanceof OWLClass){
                Set<OWLEntity> fillersOfInversRestrictions = getSomeToAllRelations((OWLClass)filler, inverse);
                accumulator.addAll(fillersOfInversRestrictions);
            }
        }
    }
}

