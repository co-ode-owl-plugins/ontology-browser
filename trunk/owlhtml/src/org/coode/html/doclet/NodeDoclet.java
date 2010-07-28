/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.doclet;

import org.coode.html.OWLHTMLKit;
import org.coode.owl.mngr.HierarchyProvider;
import org.semanticweb.owlapi.model.OWLObject;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Feb 7, 2008<br><br>
 */
public class NodeDoclet<O extends OWLObject> extends LeafNodeDoclet<O>{


    public NodeDoclet(OWLHTMLKit kit, O nodeObject, HierarchyProvider<O> model) {
        super(kit, nodeObject, model);
    }

    public void setUserObject(O focus) {
        clear();
        super.setUserObject(focus);
        final O nodeObject = getNodeObject();
        if (nodeObject != null){
            final HierarchyProvider<O> hp = getHierarchyProvider();
            if (nodeObject.equals(focus)){
                for (O child : asOrderedList(hp.getChildren(nodeObject))){
                    addDoclet(new LeafNodeDoclet<O>(getOWLHTMLKit(), child, hp));
                }
            }
            else if (hp.hasAncestor(focus, nodeObject)){
                for (O child : asOrderedList(hp.getChildren(nodeObject))){
                    if (hp.getDescendants(child).contains(nodeObject)){
                        // loop
                        addDoclet(new LeafNodeDoclet<O>(getOWLHTMLKit(), child, hp));
                    }
                    else{
                        NodeDoclet<O> subDoclet = new NodeDoclet<O>(getOWLHTMLKit(), child, hp);
                        subDoclet.setAutoExpandEnabled(isAutoExpandSubs());
                        subDoclet.setUserObject(focus);
                        addDoclet(subDoclet);
                    }
                }
            }
        }
    }
}
