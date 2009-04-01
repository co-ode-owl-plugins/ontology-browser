/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.hierarchy;

import org.coode.html.OWLHTMLServer;
import org.semanticweb.owl.inference.OWLPropertyReasoner;
import org.semanticweb.owl.inference.OWLReasonerAdapter;
import org.semanticweb.owl.inference.OWLReasonerException;
import org.semanticweb.owl.model.OWLObjectProperty;
import org.semanticweb.owl.model.OWLProperty;
import org.semanticweb.owl.model.OWLDataProperty;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.Iterator;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jan 23, 2008<br><br>
 */
public class OWLPropertyHierarchyTreeFragment<O extends OWLProperty> extends AbstractTreeFragment<O> {

    private OWLPropertyReasoner hp;

    public OWLPropertyHierarchyTreeFragment(OWLHTMLServer server, OWLPropertyReasoner hp) {
        this.hp = hp;
        setComparator(server.getComparator());
    }

    public String getTitle() {
        return "Asserted Object Property Hierarchy";
    }

    protected void generateDescendantHierarchy(O node, int depth) throws OWLReasonerException {
        if (depth < getDescendantLevels()){
            // search for subclasses of the node
            Set<O> namedSubs = new HashSet<O>();

            if (node instanceof OWLObjectProperty){
                Set<OWLObjectProperty> subs = OWLReasonerAdapter.flattenSetOfSets(hp.getSubProperties((OWLObjectProperty)node));
                for (OWLObjectProperty sub : subs) {
                    namedSubs.add((O) sub);
                }
            }
            else if (node instanceof OWLDataProperty){
                Set<OWLDataProperty> subs = OWLReasonerAdapter.flattenSetOfSets(hp.getSubProperties((OWLDataProperty)node));
                for (OWLDataProperty sub : subs) {
                    namedSubs.add((O) sub);
                }
            }

            // and recurse
            for (O namedSub : namedSubs){
                addChild(namedSub, node);
                generateDescendantHierarchy(namedSub, depth+1);
            }
        }
    }

    protected void generateAncestorHierarchy(O node, int depth) throws OWLReasonerException {
//        if (depth < getAncestorLevels()){
//            // search for supers of the node
//            Set<O> namedSupers = new HashSet<O>();
//
//            if (node instanceof OWLObjectProperty){
//                Set<OWLObjectProperty> supers = OWLReasonerAdapter.flattenSetOfSets(hp.getSubProperties((OWLObjectProperty)node));
//                for (OWLObjectProperty s : supers) {
//                    namedSupers.add((O) s);
//                }
//            }
//            else if (node instanceof OWLDataProperty){
//                Set<OWLDataProperty> supers = OWLReasonerAdapter.flattenSetOfSets(hp.getSubProperties((OWLDataProperty)node));
//                for (OWLDataProperty s : supers) {
//                    namedSupers.add((O) s);
//                }
//            }
//
//            if (namedSupers.isEmpty()){
//                addRoot(node);
//            }
//            else{
//                // recurse
//                for (O namedSuper : namedSupers){
//                    addChild(node, namedSuper);
//                    generateAncestorHierarchy(namedSuper, depth+1);
//                }
//            }
//        }
//        else{
//            addRoot(node);
//        }
    }
}
