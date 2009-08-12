/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.hierarchy;

import org.coode.html.OWLHTMLKit;
import org.coode.owl.mngr.HierarchyProvider;
import org.semanticweb.owlapi.inference.OWLReasonerException;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLObjectIntersectionOf;

import java.util.Set;


/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jan 23, 2008<br><br>
 */
public class OWLClassHierarchyTreeFragment extends AbstractTreeFragment<OWLClass> {

    private OWLHTMLKit kit;

    private HierarchyProvider<OWLClass> hp;

    private String title;


    public OWLClassHierarchyTreeFragment(OWLHTMLKit kit, HierarchyProvider<OWLClass> hp, String title) {
        this.kit = kit;
        this.hp = hp;
        this.title = title;
        setComparator(kit.getOWLServer().getComparator());
    }


    public String getTitle() {
        return title;
    }


    protected void generateDescendantHierarchy(OWLClass currentCls, int depth) throws OWLReasonerException {
        if (getDescendantLevels() < 0 || depth < getDescendantLevels()){
            // search for subclasses of the node
            Set<OWLClass> namedSubs = hp.getChildren(currentCls);

            // and recurse
            for (OWLClass namedSub : namedSubs){
                addChild(namedSub, currentCls);
                generateDescendantHierarchy(namedSub, depth+1);
            }
        }
    }


    protected void generateAncestorHierarchy(OWLClass cls, int depth) throws OWLReasonerException {
        if (depth < getAncestorLevels()){
            Set<OWLClass> namedSupers = hp.getParents(cls);


            // check equivalent classes for a named member of an intersection
            for (OWLClass equivClass : hp.getEquivalents(cls)){
                    namedSupers.remove(equivClass);
                    addSynonym(cls, equivClass);
                    generateAncestorHierarchy(equivClass, depth+1);
            }

            namedSupers.remove(cls);

            // add owlThing if no supers found
            if (!getOWLThing().equals(cls) && namedSupers.isEmpty()){
                namedSupers.add(getOWLThing());
            }
            else if (namedSupers.size() > 1 && namedSupers.contains(getOWLThing())){
                namedSupers.remove(getOWLThing());
            }
            for (OWLClass namedSuper : namedSupers){
                addChild(cls, namedSuper);
                generateAncestorHierarchy(namedSuper, depth+1);
            }
        }
    }


    private void addClsesFromFlatIntersection(OWLObjectIntersectionOf inters, Set<OWLClass> accum) {
        for (OWLClassExpression op : inters.getOperands()){
            if (op instanceof OWLClass){
                accum.add((OWLClass)op);
            }
            else if (op instanceof OWLObjectIntersectionOf){
                addClsesFromFlatIntersection((OWLObjectIntersectionOf)op, accum);
            }
        }
    }

    private OWLClass getOWLThing() {
        return kit.getOWLServer().getOWLOntologyManager().getOWLDataFactory().getOWLThing();
    }
}
