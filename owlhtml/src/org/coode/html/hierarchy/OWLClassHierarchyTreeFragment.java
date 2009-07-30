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


    protected void generateAncestorHierarchy(OWLClass cls, int depth) throws OWLReasonerException {
        if (depth < getAncestorLevels()){
            Set<OWLClass> namedSupers = new HashSet<OWLClass>();

            for (OWLDescription superclass : OWLReasonerAdapter.flattenSetOfSets(hp.getSuperClasses(cls))){
                if (superclass instanceof OWLClass){
                    namedSupers.add((OWLClass)superclass);
                }
            }

            // check equivalent classes for a named member of an intersection
            for (OWLDescription equivClass : hp.getEquivalentClasses(cls)){
                if (equivClass instanceof OWLObjectIntersectionOf){
                    addClsesFromFlatIntersection((OWLObjectIntersectionOf)equivClass, namedSupers);
                }
                else if (equivClass instanceof OWLClass){
                    namedSupers.remove((OWLClass)equivClass);
                    addSynonym(cls, (OWLClass)equivClass);
                    generateAncestorHierarchy((OWLClass)equivClass, depth+1);
                }
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
