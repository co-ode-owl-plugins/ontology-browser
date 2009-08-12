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
public class OWLDataPropertyHierarchyProvider implements HierarchyProvider<OWLDataProperty> {

    private OWLServer server;

    private Set<OWLDataProperty> implicitRoots;

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


    public OWLDataPropertyHierarchyProvider(OWLServer server) {
        this.server = server;
        server.addServerListener(serverListener);
        server.getOWLOntologyManager().addOntologyChangeListener(ontologyListener);
    }


    public OWLDataProperty getRoot() {
        return server.getOWLOntologyManager().getOWLDataFactory().getOWLTopDataProperty();
    }


    public Set<OWLDataProperty> getParents(OWLDataProperty node) {
        Set<OWLDataProperty> supers;
        if (node.equals(getRoot())){
            supers = Collections.emptySet();
        }
        else{
            supers = new HashSet<OWLDataProperty>();
            for (OWLDataPropertyExpression pe : node.getSuperProperties(getOntologies())){
                if (!pe.isAnonymous()){
                    supers.add(pe.asOWLDataProperty());
                }
            }
            if (supers.isEmpty()){
                supers.add(getRoot());
            }
        }
        return supers;
    }


    public Set<OWLDataProperty> getChildren(OWLDataProperty node) {
        Set<OWLDataProperty> subs = new HashSet<OWLDataProperty>();
        if (node.equals(getRoot())){
            subs.addAll(getImplicitRoots());
        }

        for (OWLDataPropertyExpression pe : node.getSubProperties(getOntologies())){
            if (!pe.isAnonymous()){
                subs.add(pe.asOWLDataProperty());
            }
        }

        return subs;
    }


    public Set<OWLDataProperty> getEquivalents(OWLDataProperty node) {
        Set<OWLDataProperty> equivs = new HashSet<OWLDataProperty>();
        for (OWLDataPropertyExpression pe : node.getEquivalentProperties(getOntologies())){
            if (!pe.isAnonymous()){
                equivs.add(pe.asOWLDataProperty());
            }
        }
        return equivs;
    }


    public Set<OWLDataProperty> getDescendants(OWLDataProperty node) {
        Set<OWLDataProperty> descendants = new HashSet<OWLDataProperty>();
        if (node.equals(getRoot())){
            for (OWLOntology ont : getOntologies()){
                descendants.addAll(ont.getReferencedDataProperties());
            }
        }
        else{
            List<OWLDataProperty> nodes = new ArrayList<OWLDataProperty>();
            nodes.add(node);
            for(int i=0; i<nodes.size(); i++){
                for (OWLDataProperty child : getChildren(nodes.get(i))){
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


    public Set<OWLDataProperty> getAncestors(OWLDataProperty node) {
        Set<OWLDataProperty> ancestors;
        if (node.equals(getRoot())){
            ancestors = Collections.emptySet();
        }
        else{
            List<OWLDataProperty> nodes = new ArrayList<OWLDataProperty>();
            nodes.add(node);
            for(int i=0; i<nodes.size(); i++){
                for (OWLDataProperty child : getParents(nodes.get(i))){
                    if (!nodes.contains(child)){
                        nodes.add(child);
                    }
                }
            }
            if (nodes.size() == 1){
                ancestors = Collections.singleton(getRoot());
            }
            else{
                ancestors = new HashSet<OWLDataProperty>(nodes);
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


    private Set<OWLDataProperty> getImplicitRoots() {
        if (implicitRoots == null){
            implicitRoots = new HashSet<OWLDataProperty>();
            for (OWLOntology ont : getOntologies()){
                implicitRoots.addAll(ont.getReferencedDataProperties());
            }
            for (Iterator i=implicitRoots.iterator(); i.hasNext();){
                OWLDataProperty p = (OWLDataProperty)i.next();
                if (!p.getSuperProperties(getOntologies()).isEmpty()){
                    i.remove();
                }
            }
        }
        return implicitRoots;
    }
}