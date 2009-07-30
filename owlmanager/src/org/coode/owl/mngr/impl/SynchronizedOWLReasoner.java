package org.coode.owl.mngr.impl;

import org.semanticweb.owl.inference.OWLReasoner;
import org.semanticweb.owl.inference.OWLReasonerException;
import org.semanticweb.owl.model.*;

import java.util.Set;
import java.util.Map;
/*
* Copyright (C) 2007, University of Manchester
*
* Modifications to the initial code base are copyright of their
* respective authors, or their employers as appropriate.  Authorship
* of the modifications may be determined from the ChangeLog placed at
* the end of this file.
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.

* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.

* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*/

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jul 24, 2007<br><br>
 *
 * Wrapper for any OWLReasoner that implements methods as synchronized to allow safe multithread access
 */
public class SynchronizedOWLReasoner implements OWLReasoner {

    private final OWLReasoner r;

    public SynchronizedOWLReasoner(OWLReasoner r) {
        this.r = r;
    }

    public synchronized boolean isConsistent(OWLOntology ontology) throws OWLReasonerException {
        return r.isConsistent(ontology);
    }

    public synchronized void loadOntologies(Set<OWLOntology> ontologies) throws OWLReasonerException {
        r.loadOntologies(ontologies);
    }

    public synchronized boolean isClassified() throws OWLReasonerException {
        return r.isClassified();
    }

    public synchronized void classify() throws OWLReasonerException {
        r.classify();
    }

    public synchronized boolean isRealised() throws OWLReasonerException {
        return r.isRealised();
    }

    public synchronized void realise() throws OWLReasonerException {
        r.realise();
    }

    public synchronized boolean isDefined(OWLClass cls) throws OWLReasonerException {
        return r.isDefined(cls);
    }

    public synchronized boolean isDefined(OWLObjectProperty prop) throws OWLReasonerException {
        return r.isDefined(prop);
    }

    public synchronized boolean isDefined(OWLDataProperty prop) throws OWLReasonerException {
        return r.isDefined(prop);
    }

    public synchronized boolean isDefined(OWLIndividual ind) throws OWLReasonerException {
        return r.isDefined(ind);
    }

    public synchronized Set<OWLOntology> getLoadedOntologies() {
        return r.getLoadedOntologies();
    }

    public synchronized void unloadOntologies(Set<OWLOntology> ontologies) throws OWLReasonerException {
        r.unloadOntologies(ontologies);
    }

    public synchronized void clearOntologies() throws OWLReasonerException {
        r.clearOntologies();
    }

    public synchronized void dispose() throws OWLReasonerException {
        r.dispose();
    }

    public synchronized boolean isSubClassOf(OWLDescription clsC, OWLDescription clsD) throws OWLReasonerException {
        return r.isSubClassOf(clsC, clsD);
    }

    public synchronized boolean isEquivalentClass(OWLDescription clsC, OWLDescription clsD) throws OWLReasonerException {
        return r.isEquivalentClass(clsC, clsD);
    }

    public synchronized Set<Set<OWLClass>> getSuperClasses(OWLDescription clsC) throws OWLReasonerException {
        return r.getSuperClasses(clsC);
    }

    public synchronized Set<Set<OWLClass>> getAncestorClasses(OWLDescription clsC) throws OWLReasonerException {
        return r.getAncestorClasses(clsC);
    }

    public synchronized Set<Set<OWLClass>> getSubClasses(OWLDescription clsC) throws OWLReasonerException {
        return r.getSubClasses(clsC);
    }

    public synchronized Set<Set<OWLClass>> getDescendantClasses(OWLDescription clsC) throws OWLReasonerException {
        return r.getDescendantClasses(clsC);
    }

    public synchronized Set<OWLClass> getEquivalentClasses(OWLDescription clsC) throws OWLReasonerException {
        return r.getEquivalentClasses(clsC);
    }

    public synchronized Set<OWLClass> getInconsistentClasses() throws OWLReasonerException {
        return r.getInconsistentClasses();
    }

    public synchronized boolean isSatisfiable(OWLDescription description) throws OWLReasonerException {
        return r.isSatisfiable(description);
    }

    public synchronized Set<Set<OWLClass>> getTypes(OWLIndividual individual, boolean direct) throws OWLReasonerException {
        return r.getTypes(individual, direct);
    }

    public synchronized Set<OWLIndividual> getIndividuals(OWLDescription clsC, boolean direct) throws OWLReasonerException {
        return r.getIndividuals(clsC, direct);
    }

    public synchronized Map<OWLObjectProperty, Set<OWLIndividual>> getObjectPropertyRelationships(OWLIndividual individual) throws OWLReasonerException {
        return r.getObjectPropertyRelationships(individual);
    }

    public synchronized Map<OWLDataProperty, Set<OWLConstant>> getDataPropertyRelationships(OWLIndividual individual) throws OWLReasonerException {
        return r.getDataPropertyRelationships(individual);
    }

    public synchronized boolean hasType(OWLIndividual individual, OWLDescription type, boolean direct) throws OWLReasonerException {
        return r.hasType(individual, type, direct);
    }

    public synchronized boolean hasObjectPropertyRelationship(OWLIndividual subject, OWLObjectPropertyExpression property, OWLIndividual object) throws OWLReasonerException {
        return r.hasObjectPropertyRelationship(subject, property, object);
    }

    public synchronized boolean hasDataPropertyRelationship(OWLIndividual subject, OWLDataPropertyExpression property, OWLConstant object) throws OWLReasonerException {
        return r.hasDataPropertyRelationship(subject, property, object);
    }

    public synchronized Set<OWLIndividual> getRelatedIndividuals(OWLIndividual subject, OWLObjectPropertyExpression property) throws OWLReasonerException {
        return r.getRelatedIndividuals(subject, property);
    }

    public synchronized Set<OWLConstant> getRelatedValues(OWLIndividual subject, OWLDataPropertyExpression property) throws OWLReasonerException {
        return r.getRelatedValues(subject, property);
    }

    public synchronized Set<Set<OWLObjectProperty>> getSuperProperties(OWLObjectProperty property) throws OWLReasonerException {
        return r.getSuperProperties(property);
    }

    public synchronized Set<Set<OWLObjectProperty>> getSubProperties(OWLObjectProperty property) throws OWLReasonerException {
        return r.getSubProperties(property);
    }

    public synchronized Set<Set<OWLObjectProperty>> getAncestorProperties(OWLObjectProperty property) throws OWLReasonerException {
        return r.getAncestorProperties(property);
    }

    public synchronized Set<Set<OWLObjectProperty>> getDescendantProperties(OWLObjectProperty property) throws OWLReasonerException {
        return r.getDescendantProperties(property);
    }

    public synchronized Set<Set<OWLObjectProperty>> getInverseProperties(OWLObjectProperty property) throws OWLReasonerException {
        return r.getInverseProperties(property);
    }

    public synchronized Set<OWLObjectProperty> getEquivalentProperties(OWLObjectProperty property) throws OWLReasonerException {
        return r.getEquivalentProperties(property);
    }

    public synchronized Set<Set<OWLDescription>> getDomains(OWLObjectProperty property) throws OWLReasonerException {
        return r.getDomains(property);
    }

    public synchronized Set<OWLDescription> getRanges(OWLObjectProperty property) throws OWLReasonerException {
        return r.getRanges(property);
    }

    public synchronized boolean isFunctional(OWLObjectProperty property) throws OWLReasonerException {
        return r.isFunctional(property);
    }

    public synchronized boolean isInverseFunctional(OWLObjectProperty property) throws OWLReasonerException {
        return r.isInverseFunctional(property);
    }

    public synchronized boolean isSymmetric(OWLObjectProperty property) throws OWLReasonerException {
        return r.isSymmetric(property);
    }

    public synchronized boolean isTransitive(OWLObjectProperty property) throws OWLReasonerException {
        return r.isTransitive(property);
    }

    public synchronized boolean isReflexive(OWLObjectProperty property) throws OWLReasonerException {
        return r.isReflexive(property);
    }

    public synchronized boolean isIrreflexive(OWLObjectProperty property) throws OWLReasonerException {
        return r.isIrreflexive(property);
    }

    public synchronized boolean isAntiSymmetric(OWLObjectProperty property) throws OWLReasonerException {
        return r.isAntiSymmetric(property);
    }

    public synchronized Set<Set<OWLDataProperty>> getSuperProperties(OWLDataProperty property) throws OWLReasonerException {
        return r.getSuperProperties(property);
    }

    public synchronized Set<Set<OWLDataProperty>> getSubProperties(OWLDataProperty property) throws OWLReasonerException {
        return r.getSubProperties(property);
    }

    public synchronized Set<Set<OWLDataProperty>> getAncestorProperties(OWLDataProperty property) throws OWLReasonerException {
        return r.getAncestorProperties(property);
    }

    public synchronized Set<Set<OWLDataProperty>> getDescendantProperties(OWLDataProperty property) throws OWLReasonerException {
        return r.getDescendantProperties(property);
    }

    public synchronized Set<OWLDataProperty> getEquivalentProperties(OWLDataProperty property) throws OWLReasonerException {
        return r.getEquivalentProperties(property);
    }

    public synchronized Set<Set<OWLDescription>> getDomains(OWLDataProperty property) throws OWLReasonerException {
        return r.getDomains(property);
    }

    public synchronized Set<OWLDataRange> getRanges(OWLDataProperty property) throws OWLReasonerException {
        return r.getRanges(property);
    }

    public synchronized boolean isFunctional(OWLDataProperty property) throws OWLReasonerException {
        return isFunctional(property);
    }
}
