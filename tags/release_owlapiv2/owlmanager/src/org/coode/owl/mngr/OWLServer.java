package org.coode.owl.mngr;

import org.coode.owl.mngr.impl.ToldPropertyHierarchyReasoner;
import org.semanticweb.owl.inference.OWLReasoner;
import org.semanticweb.owl.inference.OWLPropertyReasoner;
import org.semanticweb.owl.model.OWLObject;
import org.semanticweb.owl.model.OWLOntology;
import org.semanticweb.owl.model.OWLOntologyCreationException;
import org.semanticweb.owl.model.OWLOntologyManager;
import org.semanticweb.owl.util.ToldClassHierarchyReasoner;

import java.net.URI;
import java.util.Comparator;
import java.util.Set;

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

    String getID();

    OWLOntology getActiveOntology();

    OWLOntology getOntology(URI uri);

    Set<OWLOntology> getOntologies();

    /**
     * Get the ontologies used for reasoning
     * @return imports closure of the current active ontology (plus meta ontology if it exists)
     */
    Set<OWLOntology> getActiveOntologies();

    void setActiveOntology(OWLOntology ont);

    void loadOntology(URI ontPhysicalURI) throws OWLOntologyCreationException;

    void removeOntology(URI ontURI);

    void clearOntologies();

    void addServerListener(OWLServerListener l);

    void removeServerListener(OWLServerListener l);

    OWLOntologyManager getOWLOntologyManager();

    OWLReasoner getOWLReasoner();

    ToldClassHierarchyReasoner getClassHierarchyProvider();

    ToldPropertyHierarchyReasoner getPropertyHierarchyProvider();

    Comparator<OWLObject> getComparator();

    OWLNamedObjectFinder getFinder();

    NamedObjectShortFormProvider getNameRenderer();


    /**
     *
     * @param type one of
     * @return
     */
    OWLDescriptionParser getDescriptionParser(String type);

    void registerDescriptionParser(String syntax, OWLDescriptionParser parser);

    Set<String> getSupportedSyntaxes();

    ServerProperties getProperties();


    /**
     * Get rid of all caches (such as renderings) and clear the reasoner.
     * Do not clear the loaded ontologies - this is done with clearOntologies
     */
    void clear();

    void dispose();
}