/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.hierarchy;

import org.semanticweb.owl.model.OWLObject;
import org.semanticweb.owl.model.OWLClass;
import org.semanticweb.owl.inference.OWLReasonerException;
import org.apache.log4j.Logger;

import java.util.*;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jan 23, 2008<br><br>
 */
public abstract class AbstractTreeFragment<O extends OWLObject> implements TreeFragment<O> {

    private static final Logger logger = Logger.getLogger(AbstractTreeFragment.class);

    private Set<O> roots = new HashSet<O>();
    private final Map<O, Set<O>> nodeMap = new HashMap<O, Set<O>>();

    private Comparator<OWLObject> comparator;

    protected boolean refreshRequired = true;

    private O focus;

    private int ancestorLevels = 3;
    private int descendantLevels = 2; // actually needs one more layer than displayed, as this is required for determining if leaf

    private Set<O> toldNodes = new HashSet<O>();


    public final void setFocus(O focus){
        this.focus = focus;
        refreshRequired = true;
    }

    /**
     * the number of levels of the hierarchy above the current class that you can see - default is 3
     * @param threshold
     */
    public final void setAncestorLevels(int threshold){
        this.ancestorLevels = threshold;
        refreshRequired = true;
    }

    /**
     * the number of levels of the hierarchy above the current class that you can see - default is 3
     * @param threshold
     */
    public final void setDescendantLevels(int threshold){
        this.descendantLevels = threshold;
        refreshRequired = true;
    }

    protected final int getAncestorLevels(){
        return ancestorLevels;
    }

    protected final int getDescendantLevels(){
        return descendantLevels;
    }

    public final O getFocus() {
        return focus;
    }

    public final boolean isEmpty() {
        if (refreshRequired){
            refresh();
        }
        return nodeMap.isEmpty();
    }

    public Set<O> getRoots() {
        if (refreshRequired){
            refresh();
        }
        return Collections.unmodifiableSet(roots);
    }

    public List<O> getChildren(O parent) {
        if (refreshRequired){
            refresh();
        }
        toldNodes.add(parent);
        final Set<O> subs = nodeMap.get(parent);
        if (subs != null){
            if (subs.removeAll(toldNodes)){
            logger.debug("Preventing cycle in hierarchy for parent: " + parent);
            }
            List<O> children = new ArrayList<O>(subs);
            if (comparator != null){
                Collections.sort(children, comparator);
            }
            return Collections.unmodifiableList(children);
        }
        return Collections.emptyList();
    }

    public boolean isLeaf(O node) {
        if (refreshRequired){
            refresh();
        }
        return nodeMap.get(node) == null || nodeMap.get(node).isEmpty();
    }

    public void addChild(O child, O parent){
//        if (child.equals(parent) || nodeMap.get(child) != null){ // prevent cycles
//            logger.debug("Detected cycle in hierarchy for child/parent: " + child + "/" + parent);
//        }
//        else{
            // get node for given class out of the map (or add if none exists)
            Set<O> subs = nodeMap.get(parent);
            if (subs == null){
                subs = new HashSet<O>();
                nodeMap.put(parent, subs);
            }

            // add the sub to the node
            if (child != null){
                subs.add(child);
            }
//        }
    }

    public void addRoot(O root) {
        roots.add(root);
    }

    public void clear(){
        roots.clear();
        nodeMap.clear();
        toldNodes.clear();
    }

    public void setComparator(Comparator<OWLObject> comparator){
        this.comparator = comparator;
    }

    protected void refresh() {
        clear();
        try {
            generateAncestorHierarchy(getFocus(), 0);
            generateDescendantHierarchy(getFocus(), 0);
        }
        catch (OWLReasonerException e) {
            logger.error("Error generating mini hierarchy", e);
        }
        refreshRequired = false;
    }

    protected abstract void generateAncestorHierarchy(O focus, int i) throws OWLReasonerException;
    protected abstract void generateDescendantHierarchy(O focus, int i) throws OWLReasonerException;
}
