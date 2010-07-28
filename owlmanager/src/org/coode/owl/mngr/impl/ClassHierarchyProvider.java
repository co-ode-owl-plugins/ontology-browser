package org.coode.owl.mngr.impl;

import org.apache.log4j.Logger;
import org.coode.owl.mngr.HierarchyProvider;
import org.coode.owl.mngr.OWLServer;
import org.coode.owl.mngr.OWLServerListener;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.reasoner.*;
import org.semanticweb.owlapi.reasoner.structural.StructuralReasoner;
import org.semanticweb.owlapi.reasoner.structural.StructuralReasonerFactory;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

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

    private OWLReasoner r;

    private OWLServerListener serverListener = new OWLServerListener(){

        public void activeOntologyChanged(OWLOntology ont) {
            reset();
        }
    };

    public ClassHierarchyProvider(OWLServer server) {
        this.server = server;
        server.addServerListener(serverListener);
    }


    public Set<OWLClass> getRoots() {
        return Collections.singleton(getOWLThing());
    }

    public boolean isRoot(OWLClass node) {
        return node.equals(getOWLThing());
    }

    public boolean isLeaf(OWLClass node) {
        return getChildren(node).isEmpty();
    }

    public Set<OWLClass> getParents(OWLClass node) {
        try {
            return getReasoner().getSuperClasses(node, true).getFlattened();
        }
        catch (OWLReasonerRuntimeException e) {
            logger.error(e);
        }
        return Collections.emptySet();
    }


    public Set<OWLClass> getChildren(OWLClass node) {
        try {
            NodeSet<OWLClass> subsets = getReasoner().getSubClasses(node, true);
            if (node.equals(getServer().getOWLOntologyManager().getOWLDataFactory().getOWLThing())){
                if (getReasoner() instanceof StructuralReasoner){
                // TODO fix orphans - they don't show up when Thing asserted as supercls
//                ((StructuralReasoner)getReasoner()).dumpClassHierarchy(false);
                }
            }
            Set<OWLClass> children = new HashSet<OWLClass>();
            for (Node<OWLClass> synset : subsets.getNodes()){
                if (!synset.isBottomNode()){
                    children.addAll(synset.getEntities());
                }
            }
            return children;
        }
        catch (OWLReasonerRuntimeException e) {
            logger.error(e);
        }
        return Collections.emptySet();
    }


    public Set<OWLClass> getEquivalents(OWLClass node) {
        try{
            return getReasoner().getEquivalentClasses(node).getEntitiesMinus(node);
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

    public boolean hasAncestor(OWLClass node, OWLClass ancestor) {
        return getAncestors(node).contains(ancestor);
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
            OWLReasonerFactory factory = new StructuralReasonerFactory();
            r = factory.createReasoner(getServer().getActiveOntology());

            // OWLAPI v3.1
//            r.precomputeInferences(InferenceType.CLASS_HIERARCHY);

            // OWLAPI v3.0
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

    private OWLClass getOWLThing() {
        return server.getOWLOntologyManager().getOWLDataFactory().getOWLThing();
    }
}
