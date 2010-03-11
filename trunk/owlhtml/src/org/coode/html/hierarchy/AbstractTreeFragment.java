/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.hierarchy;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.log4j.Logger;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.reasoner.OWLReasonerException;

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

    private final Map<O, Set<O>> parent2ChildMap = new HashMap<O, Set<O>>();
    private final Map<O, Set<O>> child2ParentMap = new HashMap<O, Set<O>>();
    private final Map<O, Set<O>> synonymMap = new HashMap<O, Set<O>>();

    private Comparator<OWLObject> comparator;

    protected boolean refreshRequired = true;

    private O focus;

    private int ancestorLevels = 3;
    private int descendantLevels = 2; // actually needs one more layer than displayed, as this is required for determining if leaf

//    private Set<O> toldChildren = new HashSet<O>();
//    private Set<O> toldParents = new HashSet<O>();


    public final void setFocus(O focus){
        if (this.focus != focus){
            this.focus = focus;
            refreshRequired = true;
        }
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


    public boolean contains(O node) {
        return parent2ChildMap.keySet().contains(node) || child2ParentMap.keySet().contains(node);
    }


    public boolean pathContainsNode(O root, O searchNode) {
        if (searchNode.equals(root)){
            return true;
        }
        for (O child : getChildren(root)){
            if (pathContainsNode(child, searchNode)){
                return true;
            }
        }
        return false;
    }


    public final boolean isEmpty() {
        if (refreshRequired){
            refresh();
        }
        return parent2ChildMap.isEmpty();
    }

    public Set<O> getRoots() {
        if (refreshRequired){
            refresh();
        }
        return Collections.unmodifiableSet(roots);
    }

    
    public List<O> getChildren(O parent) {
        return getResults(parent, parent2ChildMap);
    }


    public List<O> getParents(O child) {
        return getResults(child, child2ParentMap);
    }


    public List<O> getSynonyms(O node) {
        return getResults(node, synonymMap);
    }


    private List<O> getResults(O child, Map<O, Set<O>> map) {
        if (refreshRequired){
            refresh();
        }
        final Set<O> resultSet = map.get(child);
        if (resultSet != null){
            List<O> parents = new ArrayList<O>(resultSet);
            if (comparator != null){
                Collections.sort(parents, comparator);
            }
            return parents;
        }
        return Collections.emptyList();
    }


    public boolean isLeaf(O node) {
        if (refreshRequired){
            refresh();
        }
        return parent2ChildMap.get(node) == null || parent2ChildMap.get(node).isEmpty();
    }


    protected void addChild(O child, O parent){
        addToMap(parent, child, parent2ChildMap);
        addToMap(child, parent, child2ParentMap);
    }


    protected void addSynonym(O o1, O o2){
        addToMap(o1, o2, synonymMap);
        addToMap(o2, o1, synonymMap);
    }


    private void addToMap(O key, O value, Map<O, Set<O>> map) {
        Set<O> values = map.get(key);
        if (values == null){
            values = new HashSet<O>();
            map.put(key, values);
        }

        // add the sub to the node
        if (value != null){
            values.add(value);
        }
    }


    public void clear(){
        roots.clear();
        parent2ChildMap.clear();
        child2ParentMap.clear();
    }

    public void setComparator(Comparator<OWLObject> comparator){
        this.comparator = comparator;
    }

    protected void refresh() {
        clear();
        try {
            generateAncestorHierarchy(getFocus(), 0);
            generateDescendantHierarchy(getFocus(), 0);

            roots.addAll(parent2ChildMap.keySet());
            roots.removeAll(child2ParentMap.keySet());

            if (roots.isEmpty()){
                roots.add(focus); // this can happen for properties as there is currently no top property
            }
        }
        catch (OWLReasonerException e) {
            logger.error("Error generating mini hierarchy", e);
        }
        refreshRequired = false;
    }

    protected abstract void generateAncestorHierarchy(O focus, int i) throws OWLReasonerException;
    protected abstract void generateDescendantHierarchy(O focus, int i) throws OWLReasonerException;
}
