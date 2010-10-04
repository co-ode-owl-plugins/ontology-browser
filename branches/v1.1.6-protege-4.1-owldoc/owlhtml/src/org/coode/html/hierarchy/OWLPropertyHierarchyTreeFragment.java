/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.hierarchy;

import java.util.Set;

import org.coode.html.OWLHTMLKit;
import org.coode.owl.mngr.HierarchyProvider;
import org.semanticweb.owlapi.model.OWLProperty;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jan 23, 2008<br><br>
 */
public class OWLPropertyHierarchyTreeFragment<O extends OWLProperty> extends AbstractTreeFragment<O> {

    private HierarchyProvider<O> hp;

    public OWLPropertyHierarchyTreeFragment(OWLHTMLKit kit, HierarchyProvider<O> hp) {
        this.hp = hp;
        setComparator(kit.getOWLServer().getComparator());
    }

    public String getTitle() {
        return "Asserted Object Property Hierarchy";
    }

    protected void generateDescendantHierarchy(O node, int depth)  {
        if (depth < getDescendantLevels()){
            // search for subclasses of the node
            Set<O> namedSubs = hp.getChildren(node);

            // and recurse
            for (O namedSub : namedSubs){
                addChild(namedSub, node);
                generateDescendantHierarchy(namedSub, depth+1);
            }
        }
    }

    protected void generateAncestorHierarchy(O prop, int depth) {
        if (depth < getAncestorLevels()){
            Set<O> namedSupers = hp.getParents(prop);
            Set<O> equivs = hp.getEquivalents(prop);


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
