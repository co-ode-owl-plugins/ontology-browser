/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.owl.mngr.impl;

import org.semanticweb.owlapi.inference.OWLPropertyReasoner;
import org.semanticweb.owlapi.inference.OWLReasonerException;
import org.semanticweb.owlapi.model.*;
import org.apache.log4j.Logger;
import org.coode.owl.mngr.HierarchyProvider;
import org.coode.owl.mngr.OWLServer;
import org.coode.owl.mngr.OWLServerListener;
import sun.reflect.generics.reflectiveObjects.NotImplementedException;

import java.util.*;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jan 23, 2008<br><br>
 */
public class OWLObjectPropertyHierarchyProvider implements HierarchyProvider<OWLObjectProperty> {

    private OWLServer server;

    private Set<OWLObjectProperty> implicitRoots;

    private OWLServerListener serverListener = new OWLServerListener(){
        public void activeOntologyChanged(OWLOntology ont) {
            reset();
        }
    };

    private OWLOntologyChangeListener ontologyListener = new OWLOntologyChangeListener(){

        public void ontologiesChanged(List<? extends OWLOntologyChange> changes) throws OWLException {
            for (OWLOntologyChange change : changes){
                if (change.isAxiomChange()){
                    if (change.getAxiom() instanceof OWLSubPropertyAxiom){
                        reset();
                        return;
                    }
                }
            }
        }
    };


    public OWLObjectPropertyHierarchyProvider(OWLServer server) {
        this.server = server;
        server.addServerListener(serverListener);
        server.getOWLOntologyManager().addOntologyChangeListener(ontologyListener);
    }


    public OWLObjectProperty getRoot() {
        return server.getOWLOntologyManager().getOWLDataFactory().getOWLTopObjectProperty();
    }


    public Set<OWLObjectProperty> getParents(OWLObjectProperty node) {
        Set<OWLObjectProperty> supers;
        if (node.equals(getRoot())){
            supers = Collections.emptySet();
        }
        else{
            supers = new HashSet<OWLObjectProperty>();
            for (OWLObjectPropertyExpression pe : node.getSuperProperties(getOntologies())){
                if (!pe.isAnonymous()){
                    supers.add(pe.asOWLObjectProperty());
                }
            }
            if (supers.isEmpty()){
                supers.add(getRoot());
            }
        }
        return supers;
    }


    public Set<OWLObjectProperty> getChildren(OWLObjectProperty node) {
        Set<OWLObjectProperty> subs = new HashSet<OWLObjectProperty>();
        if (node.equals(getRoot())){
            subs.addAll(getImplicitRoots());
        }

        for (OWLObjectPropertyExpression pe : node.getSubProperties(getOntologies())){
            if (!pe.isAnonymous()){
                subs.add(pe.asOWLObjectProperty());
            }
        }

        return subs;
    }


    public Set<OWLObjectProperty> getEquivalents(OWLObjectProperty node) {
        Set<OWLObjectProperty> equivs = new HashSet<OWLObjectProperty>();
        for (OWLObjectPropertyExpression pe : node.getEquivalentProperties(getOntologies())){
            if (!pe.isAnonymous()){
                equivs.add(pe.asOWLObjectProperty());
            }
        }
        return equivs;
    }


    public Set<OWLObjectProperty> getDescendants(OWLObjectProperty node) {
        Set<OWLObjectProperty> descendants = new HashSet<OWLObjectProperty>();
        if (node.equals(getRoot())){
            for (OWLOntology ont : getOntologies()){
                descendants.addAll(ont.getReferencedObjectProperties());
            }
        }
        else{
            List<OWLObjectProperty> nodes = new ArrayList<OWLObjectProperty>();
            nodes.add(node);
            for(int i=0; i<nodes.size(); i++){
                for (OWLObjectProperty child : getChildren(nodes.get(i))){
                    if (!nodes.contains(child)){
                        nodes.add(child);
                    }
                }
            }
            descendants.addAll(nodes);
            descendants.remove(node);
        }
        return descendants;
    }


    public Set<OWLObjectProperty> getAncestors(OWLObjectProperty node) {
        Set<OWLObjectProperty> ancestors;
        if (node.equals(getRoot())){
            ancestors = Collections.emptySet();
        }
        else{
            List<OWLObjectProperty> nodes = new ArrayList<OWLObjectProperty>();
            nodes.add(node);
            for(int i=0; i<nodes.size(); i++){
                for (OWLObjectProperty child : getParents(nodes.get(i))){
                    if (!nodes.contains(child)){
                        nodes.add(child);
                    }
                }
            }
            if (nodes.size() == 1){
                ancestors = Collections.singleton(getRoot());
            }
            else{
                ancestors = new HashSet<OWLObjectProperty>(nodes);
                ancestors.remove(node);
            }
        }
        return ancestors;
    }


    protected Set<OWLOntology> getOntologies() {
        return server.getActiveOntologies();
    }


    public void dispose() {
        server.getOWLOntologyManager().removeOntologyChangeListener(ontologyListener);
        server.removeServerListener(serverListener);
        server = null;
    }


    private void reset() {
        implicitRoots = null;
    }


    private Set<OWLObjectProperty> getImplicitRoots() {
        if (implicitRoots == null){
            implicitRoots = new HashSet<OWLObjectProperty>();
            for (OWLOntology ont : getOntologies()){
                implicitRoots.addAll(ont.getReferencedObjectProperties());
            }
            for (Iterator i=implicitRoots.iterator(); i.hasNext();){
                OWLObjectProperty p = (OWLObjectProperty)i.next();
                if (!p.getSuperProperties(getOntologies()).isEmpty()){
                    i.remove();
                }
            }
        }
        return implicitRoots;
    }
}
