/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.hierarchy;

import org.coode.html.OWLHTMLServer;
import org.semanticweb.owl.inference.OWLClassReasoner;
import org.semanticweb.owl.inference.OWLReasonerAdapter;
import org.semanticweb.owl.inference.OWLReasonerException;
import org.semanticweb.owl.model.OWLClass;
import org.semanticweb.owl.model.OWLDescription;
import org.semanticweb.owl.model.OWLObjectIntersectionOf;
import org.apache.log4j.Logger;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import edu.unika.aifb.rdf.api.syntax.RDFParser;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jan 23, 2008<br><br>
 */
public class OWLClassHierarchyTreeFragment extends AbstractTreeFragment<OWLClass> {

    private OWLHTMLServer server;

    private OWLClassReasoner hp;

    private String title;


    public OWLClassHierarchyTreeFragment(OWLHTMLServer server, OWLClassReasoner hp, String title) {
        this.server = server;
        this.hp = hp;
        this.title = title;
        setComparator(server.getComparator());
    }

    public String getTitle() {
        return title;
    }
    
    protected void generateDescendantHierarchy(OWLClass currentCls, int depth) throws OWLReasonerException {
        if (getDescendantLevels() < 0 || depth < getDescendantLevels()){
            // search for subclasses of the node
            Set<OWLClass> namedSubs = new HashSet<OWLClass>();

            for (OWLDescription subclass : OWLReasonerAdapter.flattenSetOfSets(hp.getSubClasses(currentCls))){
                if (subclass instanceof OWLClass){
                    namedSubs.add((OWLClass)subclass);
                }
            }

            // and recurse
            for (OWLClass namedSub : namedSubs){
                addChild(namedSub, currentCls);
                generateDescendantHierarchy(namedSub, depth+1);
            }
        }
    }

    protected void generateAncestorHierarchy(OWLClass currentCls, int depth) throws OWLReasonerException {
        if (depth < getAncestorLevels() && !currentCls.equals(getOWLThing())){
            // search for superclasses of the node
            Set<OWLClass> namedSupers = new HashSet<OWLClass>();

            for (OWLDescription superclass : OWLReasonerAdapter.flattenSetOfSets(hp.getSuperClasses(currentCls))){
                if (superclass instanceof OWLClass){
                    namedSupers.add((OWLClass)superclass);
                }
            }

            // check equivalent classes for a named member of an intersection
            for (OWLDescription equivClass : hp.getEquivalentClasses(currentCls)){
                if (equivClass instanceof OWLObjectIntersectionOf){
                    addClsesFromFlatIntersection((OWLObjectIntersectionOf)equivClass, namedSupers);
                }
            }

            // add owlThing if no supers found
            if (namedSupers.isEmpty()){
                namedSupers.add(getOWLThing());
            }

            // and recurse
            for (OWLClass namedSuper : namedSupers){
                addChild(currentCls, namedSuper);
                generateAncestorHierarchy(namedSuper, depth+1);
            }
        }
        else{
            addRoot(currentCls);
        }
    }

    private void addClsesFromFlatIntersection(OWLObjectIntersectionOf inters, Set<OWLClass> accum) {
        for (OWLDescription op : inters.getOperands()){
            if (op instanceof OWLClass){
                accum.add((OWLClass)op);
            }
            else if (op instanceof OWLObjectIntersectionOf){
                addClsesFromFlatIntersection((OWLObjectIntersectionOf)op, accum);
            }
        }
    }

    private OWLClass getOWLThing() {
        return server.getOWLOntologyManager().getOWLDataFactory().getOWLThing();
    }
}
