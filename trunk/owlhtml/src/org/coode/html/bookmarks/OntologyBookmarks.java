/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.bookmarks;

import org.apache.log4j.Logger;
import org.semanticweb.owlapi.model.*;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.*;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Feb 11, 2008<br><br>
 */
public class OntologyBookmarks {

    private static final String BOOKMARK_PROP = "http://www.co-ode.org/ontologies/meta.owl#bookmark";

    private OWLAnnotationProperty property;

    private Set<OWLEntity> bookmarks = new HashSet<OWLEntity>();

    private OWLOntologyManager mngr;

    private OWLOntology ont;


    public OntologyBookmarks(OWLOntologyManager mngr, OWLOntology ont) {
        this.mngr = mngr;
        this.ont = ont;

        property = mngr.getOWLDataFactory().getOWLAnnotationProperty(IRI.create(BOOKMARK_PROP));
        loadAnnotations();
    }

    public OWLOntology getOntology(){
        return ont;
    }

    public Set<OWLEntity> getBookmarks(){
        return Collections.unmodifiableSet(bookmarks);
    }

    public int getSize() {
        return bookmarks.size();
    }

    public List<OWLOntologyChange> add(OWLEntity obj) throws OWLException {
        bookmarks.add(obj);
        return getUpdateChanges();
    }

    public List<OWLOntologyChange> remove(OWLEntity obj) throws OWLException {
        bookmarks.remove(obj);
        return getUpdateChanges();
    }

    public List<OWLOntologyChange> clear() throws OWLException {
        List<OWLOntologyChange> changes = new ArrayList<OWLOntologyChange>();
        for (OWLAnnotation annot : ont.getAnnotations()){
            if (annot.getProperty().equals(property)){
                changes.add(new RemoveOntologyAnnotation(ont, annot));
            }
        }
        return changes;
    }

    private List<OWLOntologyChange> getUpdateChanges() throws OWLException {
        List<OWLOntologyChange> changes = new ArrayList<OWLOntologyChange>();
        String annotationValue = "";
        for (OWLObject bookmark : bookmarks){
            if (bookmark instanceof OWLEntity){
                annotationValue += ((OWLEntity)bookmark).getURI() + "\n";
            }
        }
        changes.addAll(clear());

        if (annotationValue.length() > 0){
            OWLLiteral value = mngr.getOWLDataFactory().getOWLStringLiteral(annotationValue);
            OWLAnnotation annot = mngr.getOWLDataFactory().getOWLAnnotation(property, value);
            changes.add(new AddOntologyAnnotation(ont, annot));
        }

        return changes;
    }

    private void loadAnnotations() {
        // load the bookmark from the ontology annotations
        for (OWLAnnotation annot : ont.getAnnotations()){
            if (annot.getProperty().equals(property)){
                OWLAnnotationValue content = annot.getValue();
                if (content instanceof OWLLiteral){
                    parseAnnotation(((OWLLiteral)content).getLiteral());
                }
            }
        }
    }

    private void parseAnnotation(String s) {
        for (String value : s.split("\n")){
            try {
                URI uri = new URI(value);
                OWLEntity e = getEntityFromURI(uri);
                if (e != null){
                    bookmarks.add(e);
                }
            }
            catch (URISyntaxException e) {
                Logger.getLogger(OntologyBookmarks.class).error(e);
            }
        }
    }

    private OWLEntity getEntityFromURI(URI uri) {
        for (OWLOntology ont : mngr.getOntologies()){
            if (ont.containsClassReference(uri)){
                return mngr.getOWLDataFactory().getOWLClass(uri);
            }

            if (ont.containsObjectPropertyReference(uri)){
                return mngr.getOWLDataFactory().getOWLObjectProperty(uri);
            }

            if (ont.containsDataPropertyReference(uri)){
                return mngr.getOWLDataFactory().getOWLDataProperty(uri);
            }

            if (ont.containsAnnotationPropertyReference(uri)){
                return mngr.getOWLDataFactory().getOWLAnnotationProperty(uri);
            }

            if (ont.containsIndividualReference(uri)){
                return mngr.getOWLDataFactory().getOWLNamedIndividual(uri);
            }

            if (ont.containsDatatypeReference(uri)){
                return mngr.getOWLDataFactory().getOWLDatatype(uri);
            }
        }

        return null;
    }
}
