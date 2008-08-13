/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.owl.mngr.impl;

import org.semanticweb.owl.inference.OWLPropertyReasoner;
import org.semanticweb.owl.inference.OWLReasonerException;
import org.semanticweb.owl.model.*;
import org.apache.log4j.Logger;
import sun.reflect.generics.reflectiveObjects.NotImplementedException;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import com.sun.org.apache.xpath.internal.compiler.XPathParser;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jan 23, 2008<br><br>
 */
public class ToldPropertyHierarchyReasoner implements OWLPropertyReasoner {

    private static final Logger logger = Logger.getLogger(ToldPropertyHierarchyReasoner.class);

    private Set<OWLOntology> onts = new HashSet<OWLOntology>();


    public ToldPropertyHierarchyReasoner(OWLOntologyManager mngr) {
        logger.warn("MUST IMPLEMENT ONTOLOGY MODEL LISTENER");
    }

    public Set<Set<OWLObjectProperty>> getSuperProperties(OWLObjectProperty property) throws OWLReasonerException {
        Set<Set<OWLObjectProperty>> results = new HashSet<Set<OWLObjectProperty>>();
        for (OWLObjectPropertyExpression superProp : property.getSuperProperties(onts)){
            if (superProp instanceof OWLObjectProperty){
                results.add(Collections.singleton((OWLObjectProperty)superProp));
            }
        }
        return results;
    }

    public Set<Set<OWLObjectProperty>> getSubProperties(OWLObjectProperty property) throws OWLReasonerException {
        Set<Set<OWLObjectProperty>> results = new HashSet<Set<OWLObjectProperty>>();
        for (OWLObjectPropertyExpression subProp : property.getSubProperties(onts)){
            if (subProp instanceof OWLObjectProperty){
                results.add(Collections.singleton((OWLObjectProperty)subProp));
            }
        }
        return results;
    }

    public Set<Set<OWLDataProperty>> getSuperProperties(OWLDataProperty property) throws OWLReasonerException {
        Set<Set<OWLDataProperty>> results = new HashSet<Set<OWLDataProperty>>();
        for (OWLDataPropertyExpression superProp : property.getSuperProperties(onts)){
            if (superProp instanceof OWLDataProperty){
                results.add(Collections.singleton((OWLDataProperty)superProp));
            }
        }
        return results;
    }

    public Set<Set<OWLDataProperty>> getSubProperties(OWLDataProperty property) throws OWLReasonerException {
        Set<Set<OWLDataProperty>> results = new HashSet<Set<OWLDataProperty>>();
        for (OWLDataPropertyExpression subProp : property.getSubProperties(onts)){
            if (subProp instanceof OWLDataProperty){
                results.add(Collections.singleton((OWLDataProperty)subProp));
            }
        }
        return results;
    }


    public Set<Set<OWLObjectProperty>> getAncestorProperties(OWLObjectProperty property) throws OWLReasonerException {
        throw new NotImplementedException();
    }

    public Set<Set<OWLObjectProperty>> getDescendantProperties(OWLObjectProperty property) throws OWLReasonerException {
        throw new NotImplementedException();
    }

    public Set<Set<OWLObjectProperty>> getInverseProperties(OWLObjectProperty property) throws OWLReasonerException {
        throw new NotImplementedException();
    }

    public Set<OWLObjectProperty> getEquivalentProperties(OWLObjectProperty property) throws OWLReasonerException {
        throw new NotImplementedException();
    }

    public Set<Set<OWLDescription>> getDomains(OWLObjectProperty property) throws OWLReasonerException {
        throw new NotImplementedException();
    }

    public Set<OWLDescription> getRanges(OWLObjectProperty property) throws OWLReasonerException {
        throw new NotImplementedException();
    }

    public boolean isFunctional(OWLObjectProperty property) throws OWLReasonerException {
        throw new NotImplementedException();
    }

    public boolean isInverseFunctional(OWLObjectProperty property) throws OWLReasonerException {
        throw new NotImplementedException();
    }

    public boolean isSymmetric(OWLObjectProperty property) throws OWLReasonerException {
        throw new NotImplementedException();
    }

    public boolean isTransitive(OWLObjectProperty property) throws OWLReasonerException {
        throw new NotImplementedException();
    }

    public boolean isReflexive(OWLObjectProperty property) throws OWLReasonerException {
        throw new NotImplementedException();
    }

    public boolean isIrreflexive(OWLObjectProperty property) throws OWLReasonerException {
        throw new NotImplementedException();
    }

    public boolean isAntiSymmetric(OWLObjectProperty property) throws OWLReasonerException {
        throw new NotImplementedException();
    }

    public Set<Set<OWLDataProperty>> getAncestorProperties(OWLDataProperty property) throws OWLReasonerException {
        throw new NotImplementedException();
    }

    public Set<Set<OWLDataProperty>> getDescendantProperties(OWLDataProperty property) throws OWLReasonerException {
        throw new NotImplementedException();
    }

    public Set<OWLDataProperty> getEquivalentProperties(OWLDataProperty property) throws OWLReasonerException {
        throw new NotImplementedException();
    }

    public Set<Set<OWLDescription>> getDomains(OWLDataProperty property) throws OWLReasonerException {
        throw new NotImplementedException();
    }

    public Set<OWLDataRange> getRanges(OWLDataProperty property) throws OWLReasonerException {
        throw new NotImplementedException();
    }

    public boolean isFunctional(OWLDataProperty property) throws OWLReasonerException {
        throw new NotImplementedException();
    }

    public void loadOntologies(Set<OWLOntology> ontologies) throws OWLReasonerException {
        onts.addAll(ontologies);
    }

    public boolean isClassified() throws OWLReasonerException {
        return true;
    }

    public void classify() throws OWLReasonerException {
        // do nothing
    }

    public boolean isRealised() throws OWLReasonerException {
        throw new NotImplementedException();
    }

    public void realise() throws OWLReasonerException {
        throw new NotImplementedException();
    }

    public boolean isDefined(OWLClass cls) throws OWLReasonerException {
        throw new NotImplementedException();
    }

    public boolean isDefined(OWLObjectProperty prop) throws OWLReasonerException {
        throw new NotImplementedException();
    }

    public boolean isDefined(OWLDataProperty prop) throws OWLReasonerException {
        throw new NotImplementedException();
    }

    public boolean isDefined(OWLIndividual ind) throws OWLReasonerException {
        throw new NotImplementedException();
    }

    public Set<OWLOntology> getLoadedOntologies() {
        return Collections.unmodifiableSet(onts);
    }

    public void unloadOntologies(Set<OWLOntology> ontologies) throws OWLReasonerException {
        onts.removeAll(ontologies);
    }

    public void clearOntologies() throws OWLReasonerException {
        onts.clear();
    }

    public void dispose() throws OWLReasonerException {
        onts.clear();
    }
}
