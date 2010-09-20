package org.coode.owl.mngr;

import java.net.URI;
import java.util.Comparator;
import java.util.Set;

import org.semanticweb.owlapi.expression.OWLEntityChecker;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.util.OntologyIRIShortFormProvider;
import org.semanticweb.owlapi.util.ShortFormProvider;

/**
 * Author: Nick Drummond<br>
 * nick.drummond@cs.manchester.ac.uk<br>
 * http://www.cs.man.ac.uk/~drummond<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jun 7, 2007<br><br>
 * <p/>
 * code made available under Mozilla Public License (http://www.mozilla.org/MPL/MPL-1.1.html)<br>
 * copyright 2006, The University of Manchester<br>
 */
public interface OWLServer {

    OWLOntology getActiveOntology();
    /**
     * Get the ontologies used for reasoning
     * @return imports closure of the current active ontology (plus meta ontology if it exists)
     */
    Set<OWLOntology> getActiveOntologies();

    Set<OWLOntology> getOntologies();

    void setActiveOntology(OWLOntology ont);

    void loadOntology(URI ontPhysicalURI) throws OWLOntologyCreationException;

    void removeOntology(OWLOntology ont);

    void clearOntologies();

    void addServerListener(OWLServerListener l);

    void removeServerListener(OWLServerListener l);

    OWLOntologyManager getOWLOntologyManager();

    OWLReasoner getOWLReasoner();

    HierarchyProvider<OWLClass> getClassHierarchyProvider();
    HierarchyProvider<OWLObjectProperty> getOWLObjectPropertyHierarchyProvider();
    HierarchyProvider<OWLDataProperty> getOWLDataPropertyHierarchyProvider();
//    HierarchyProvider<OWLAnnotationProperty> getAnnotationPropertyHierarchyProvider();

    Comparator<OWLObject> getComparator();

    OWLEntityFinder getFinder();

    OWLEntityChecker getOWLEntityChecker();

    ShortFormProvider getShortFormProvider();

    OntologyIRIShortFormProvider getOntologyShortFormProvider();


    /**
     *
     * @param type one of
     * @return
     */
    OWLClassExpressionParser getClassExpressionParser(String type);

    void registerDescriptionParser(String syntax, OWLClassExpressionParser parser);

    Set<String> getSupportedSyntaxes();

    ServerPropertiesAdapter<ServerProperty> getProperties();


    /**
     * Get rid of all caches (such as renderings) and clear the reasoner.
     * Do not clear the loaded ontologies - this is done with clearOntologies
     */
    void clear();

    void dispose();

    boolean isDead();
}