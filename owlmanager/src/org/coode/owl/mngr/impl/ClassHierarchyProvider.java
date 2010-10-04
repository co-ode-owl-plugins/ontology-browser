package org.coode.owl.mngr.impl;

import java.util.Collections;
import java.util.Set;

import org.apache.log4j.Logger;
import org.coode.owl.mngr.HierarchyProvider;
import org.coode.owl.mngr.OWLServer;
import org.coode.owl.mngr.OWLServerListener;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.reasoner.OWLReasonerRuntimeException;
import org.semanticweb.owlapi.reasoner.structural.StructuralReasoner;
import org.semanticweb.owlapi.reasoner.structural.StructuralReasonerFactory;

/**
 * Author: drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Aug 6, 2009<br><br>
 */
public class ClassHierarchyProvider implements HierarchyProvider<OWLClass>{

    private Logger logger = Logger.getLogger(ClassHierarchyProvider.class);

    private OWLServer server;

    private StructuralReasoner r;

    private OWLServerListener serverListener = new OWLServerListener(){

        public void activeOntologyChanged(OWLOntology ont) {
            reset();
        }
    };


    public ClassHierarchyProvider(OWLServer server) {
        this.server = server;
        server.addServerListener(serverListener);
    }


    public OWLClass getRoot() {
        return getServer().getOWLOntologyManager().getOWLDataFactory().getOWLThing();
    }


    public Set<OWLClass> getParents(OWLClass node) {
        try {
            return getReasoner().getSuperClasses(node,false).getFlattened();
        }
        catch (OWLReasonerRuntimeException e) {
            logger.error(e);
        }
        return Collections.emptySet();
    }


    public Set<OWLClass> getChildren(OWLClass node) {
        try {
            return getReasoner().getSubClasses(node, true).getFlattened();
        }
        catch (OWLReasonerRuntimeException e) {
            logger.error(e);
        }
        return Collections.emptySet();
    }


    public Set<OWLClass> getEquivalents(OWLClass node) {
        try{
            return getReasoner().getEquivalentClasses(node).getEntities();
        }
        catch (OWLReasonerRuntimeException e) {
            logger.error(e);
        }
        return Collections.emptySet();
    }


    public Set<OWLClass> getDescendants(OWLClass node) {
        try {
            return getReasoner().getSubClasses(node, false).getFlattened();
        }
        catch (OWLReasonerRuntimeException e) {
            logger.error(e);
        }
        return Collections.emptySet();
    }


    public Set<OWLClass> getAncestors(OWLClass node) {
        try{
            return getReasoner().getSuperClasses(node, false).getFlattened();
        }
        catch (OWLReasonerRuntimeException e) {
            logger.error(e);
        }
        return Collections.emptySet();
    }


    public void dispose() {
        getServer().removeServerListener(serverListener);
    }


    protected final OWLServer getServer() {
        return server;
    }


    protected Set<OWLOntology> getOntologies() {
        return getServer().getActiveOntologies();
    }


    protected OWLReasoner getReasoner() {
        if (r == null){
            StructuralReasonerFactory factory = new StructuralReasonerFactory();
            factory.createReasoner(getServer().getActiveOntology());
            r.prepareReasoner();
        }
        return r;
    }

    private void reset() {
        if (r != null){
            r.dispose();
            r = null;
        }
    }
}
