/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.hierarchy;

import org.coode.html.OWLHTMLServer;
import org.semanticweb.owl.inference.OWLPropertyReasoner;
import org.semanticweb.owl.inference.OWLReasonerAdapter;
import org.semanticweb.owl.inference.OWLReasonerException;
import org.semanticweb.owl.model.*;

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

    protected void generateAncestorHierarchy(O prop, int depth) throws OWLReasonerException {
        if (depth < getAncestorLevels()){
            Set<O> namedSupers = new HashSet<O>();
            Set<O> equivs = new HashSet<O>();

            if (prop instanceof OWLObjectProperty){
                Set<OWLObjectProperty> subs = OWLReasonerAdapter.flattenSetOfSets(hp.getSuperProperties((OWLObjectProperty)prop));
                for (OWLObjectProperty sub : subs) {
                    namedSupers.add((O) sub);
                }
                Set<OWLObjectProperty> equivObjProps = hp.getEquivalentProperties((OWLObjectProperty)prop);
                for (OWLObjectProperty equiv : equivObjProps) {
                    equivs.add((O) equiv);
                }
            }
            else if (prop instanceof OWLDataProperty){
                Set<OWLDataProperty> subs = OWLReasonerAdapter.flattenSetOfSets(hp.getSuperProperties((OWLDataProperty)prop));
                for (OWLDataProperty sub : subs) {
                    namedSupers.add((O) sub);
                }
                Set<OWLDataProperty> equivObjProps = hp.getEquivalentProperties((OWLDataProperty)prop);
                for (OWLDataProperty equiv : equivObjProps) {
                    equivs.add((O) equiv);
                }
            }

            // check equivalent classes for a named member of an intersection
            for (O equiv : equivs){
                    namedSupers.remove((O)equiv);
                    addSynonym(prop, (O)equiv);
                    generateAncestorHierarchy((O)equiv, depth+1);
            }

            namedSupers.remove(prop);

            for (O namedSuper : namedSupers){
                addChild(prop, namedSuper);
                generateAncestorHierarchy(namedSuper, depth+1);
            }
        }
    }
}
