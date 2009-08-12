/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.hierarchy;

import org.semanticweb.owlapi.model.OWLClass;

import java.util.List;
import java.util.Set;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jan 23, 2008<br><br>
 *
 * @@TODO should implement TreeModel
 */
public interface TreeFragment<O> {

    String getTitle();

    boolean isEmpty();

    Set<O> getRoots();

    List<O> getChildren(O node);

    List<O> getParents(O node);

    List<O> getSynonyms(O node);

    boolean isLeaf(O node);

    void setFocus(O focus);

    O getFocus();

    boolean contains(O node);

    boolean pathContainsNode(O root, O searchNode);
}
