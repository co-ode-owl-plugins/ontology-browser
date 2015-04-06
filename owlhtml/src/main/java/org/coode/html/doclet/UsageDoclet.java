/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.doclet;

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

import org.coode.html.OWLHTMLKit;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLAnnotation;
import org.semanticweb.owlapi.model.OWLAnnotationAssertionAxiom;
import org.semanticweb.owlapi.model.OWLAnnotationProperty;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLClassAssertionAxiom;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLDataPropertyAssertionAxiom;
import org.semanticweb.owlapi.model.OWLDeclarationAxiom;
import org.semanticweb.owlapi.model.OWLDifferentIndividualsAxiom;
import org.semanticweb.owlapi.model.OWLDisjointClassesAxiom;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLEquivalentClassesAxiom;
import org.semanticweb.owlapi.model.OWLIndividual;
import org.semanticweb.owlapi.model.OWLNaryIndividualAxiom;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLObjectPropertyAssertionAxiom;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLSameIndividualAxiom;
import org.semanticweb.owlapi.model.OWLSubClassOfAxiom;
import org.semanticweb.owlapi.model.OWLUnaryPropertyAxiom;
import org.semanticweb.owlapi.util.OWLAxiomVisitorAdapter;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jan 25, 2008<br><br>
 */
public class UsageDoclet<O extends OWLEntity> extends AbstractOWLElementsDoclet<O, OWLObject>{

    private UsageVisibilityVisitor usageVisibilityVisitor = new UsageVisibilityVisitor();

    public UsageDoclet(OWLHTMLKit kit) {
        super("Usage", ElementsDoclet.Format.list, kit);
    }

    @Override
    protected Collection<OWLObject> getAssertedElements(Set<OWLOntology> onts) {
        OWLEntity entity = getUserObject();
        Collection<OWLObject> usage = new HashSet<>();
        for (OWLOntology ont : onts){
            for (OWLAxiom ax : ont.getReferencingAxioms(entity)){
                if (usageVisibilityVisitor.getShowUsage(ax, entity)){
                    usage.add(ax);
                }
            }

        }

        if (entity instanceof OWLAnnotationProperty){
            for (OWLOntology ont : onts){
                for (OWLAnnotation annot : ont.getAnnotations()){
                    if (annot.getProperty().equals(entity)){
                        usage.add(annot);
                    }
                }
            }
        }
        return usage;
    }


    private class UsageVisibilityVisitor extends OWLAxiomVisitorAdapter {

        private boolean showUsage;
        private OWLEntity currentEntity;

        public UsageVisibilityVisitor() {}

        @Override
        public void visit(OWLDeclarationAxiom axiom) {
            showUsage = false;
        }

        @Override
        public void visit(OWLAnnotationAssertionAxiom axiom) {
            if (axiom.getSubject() instanceof IRI && axiom.getSubject().equals(currentEntity.getIRI())){
                showUsage = false;
            }
        }

        @Override
        public void visit(OWLSubClassOfAxiom ax) {
            if (ax.getSubClass() instanceof OWLClass){
                if (ax.getSubClass().equals(currentEntity)){
                    showUsage = false; // we'll already be showing it as superclasses
                }
                else if (ax.getSuperClass().equals(currentEntity)){
                    showUsage = false; // we'll already be showing it as subclasses
                }
            }
        }

        @Override
        public void visit(OWLDisjointClassesAxiom ax) {
            for (OWLClassExpression d : ax.getClassExpressions()){
                if (!(d instanceof OWLClass)){
                    return;
                }
            }
            showUsage = false;
        }


        @Override
        public void visit(OWLEquivalentClassesAxiom ax) {
            for (OWLClassExpression d : ax.getClassExpressions()){
                if (d.equals(currentEntity)){
                    showUsage = false;
                    return;
                }
            }
        }

        @Override
        public void visit(OWLClassAssertionAxiom ax) {
            if (ax.getIndividual().equals(currentEntity) ||
                ax.getClassExpression().equals(currentEntity)){
                showUsage = false; // we'll already be showing it as type/member
            }
        }


        @Override
        public void visit(OWLObjectPropertyAssertionAxiom axiom) {
            if (axiom.getSubject().equals(currentEntity)){
                showUsage = false;
            }
        }

        @Override
        public void visit(OWLDataPropertyAssertionAxiom axiom) {
            if (axiom.getSubject().equals(currentEntity)){
                showUsage = false;
            }
        }

        @Override
        public void visit(OWLDifferentIndividualsAxiom ax) {
            visitNaryIndAxiom(ax);
        }


        @Override
        public void visit(OWLSameIndividualAxiom ax) {
            visitNaryIndAxiom(ax);
        }

        public void visitNaryIndAxiom(OWLNaryIndividualAxiom ax) {
            for (OWLIndividual d : ax.getIndividuals()){
                if (d.equals(currentEntity)){
                    showUsage = false; // we'll already be showing it as same or different
                    return;
                }
            }
        }

        public boolean getShowUsage(OWLAxiom ax, OWLEntity entity) {
            this.currentEntity = entity;
            showUsage = true;
            if (ax instanceof OWLUnaryPropertyAxiom){ // too expensive to do each by hand
                if (((OWLUnaryPropertyAxiom<?>)ax).getProperty().equals(currentEntity)){
                    showUsage = false;
                }
            }
            else{
                ax.accept(this);
            }
            return showUsage;
        }
    }
}
