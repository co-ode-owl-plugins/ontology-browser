package org.coode.owl.mngr.impl;

import org.apache.log4j.Logger;
import org.coode.owl.mngr.HierarchyProvider;
import org.coode.owl.mngr.OWLServer;
import org.coode.owl.mngr.OWLServerListener;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.reasoner.Node;
import org.semanticweb.owlapi.reasoner.NodeSet;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.reasoner.OWLReasonerRuntimeException;

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

    // TODO: should we also reset on an ontology load/remove?

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
        logger.debug("getChildren(" + node + ")");
        try {
            Set<OWLClass> children = new HashSet<OWLClass>();

            NodeSet<OWLClass> subsets = getReasoner().getSubClasses(node, true);
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
        logger.debug("getEquivalents(" + node + ")");
        try{
            return getReasoner().getEquivalentClasses(node).getEntitiesMinus(node);
        }
        catch (OWLReasonerRuntimeException e) {
            logger.error(e);
        }
        return Collections.emptySet();
    }


    public Set<OWLClass> getDescendants(OWLClass node) {
        logger.debug("getDescendants(" + node + ")");
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
            r = getServer().getOWLReasoner();
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


//    // OWL API bug fixed
//    // see https://sourceforge.net/tracker/?func=detail&aid=3037035&group_id=90989&atid=595534
//    private Set<OWLClass> getImplicitRoots() {
//        if (implicitRoots == null){
//            implicitRoots = new HashSet<OWLClass>();
//            for (OWLOntology ont : getOntologies()){
//                implicitRoots.addAll(ont.getClassesInSignature());
//            }
//            implicitRoots.remove(getOWLThing());
//            for (OWLOntology ont : getOntologies()){
//                for (OWLSubClassOfAxiom ax : ont.getAxioms(AxiomType.SUBCLASS_OF)){
//                    if (!ax.getSubClass().isAnonymous()){
//                        if (isImplicitNamedClass(ax.getSuperClass())){
//                            implicitRoots.remove(ax.getSubClass().asOWLClass());
//                        }
//                    }
//                }
//                for (OWLEquivalentClassesAxiom ax : ont.getAxioms(AxiomType.EQUIVALENT_CLASSES)){
//                    Set<OWLClass> names = new HashSet<OWLClass>();
//                    boolean remove = false;
//                    for (OWLClassExpression cls : ax.getClassExpressions()){
//                        if (!cls.isAnonymous()){
//                            names.add(cls.asOWLClass());
//                        }
//                        else if (isImplicitNamedClass(cls)){
//                            remove = true;
//                        }
//                    }
//                    if (remove){
//                        implicitRoots.removeAll(names);
//                    }
//                }
//            }
//        }
//        return new HashSet<OWLClass>(implicitRoots);
//    }
//
//    private boolean isImplicitNamedClass(OWLClassExpression superClass) {
//        for (OWLClassExpression op : superClass.asConjunctSet()){
//            if (!op.isAnonymous()){
//                return true;
//            }
//        }
//        return false;
//    }
}
