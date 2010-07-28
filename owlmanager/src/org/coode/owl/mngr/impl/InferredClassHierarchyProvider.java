package org.coode.owl.mngr.impl;

import org.coode.owl.mngr.OWLServer;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.reasoner.OWLReasoner;

/**
 * Author: drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Aug 6, 2009<br><br>
 */
public class InferredClassHierarchyProvider extends ClassHierarchyProvider{


    public InferredClassHierarchyProvider(OWLServer server) {
        super(server);
    }

    protected OWLReasoner getReasoner() {
        return getServer().getOWLReasoner();
    }

    public boolean isRoot(OWLClass node) {
        return super.isRoot(node);
    }
}