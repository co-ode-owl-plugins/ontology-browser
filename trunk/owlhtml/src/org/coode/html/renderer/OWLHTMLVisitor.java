/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.renderer;

import org.apache.log4j.Logger;
import org.coode.html.impl.OWLHTMLConstants;
import org.coode.html.url.NamedObjectURLRenderer;
import org.coode.html.util.URLUtils;
import org.coode.owl.mngr.NamedObjectShortFormProvider;
import org.coode.owl.util.ModelUtil;
import org.semanticweb.owl.model.*;
import org.semanticweb.owl.util.OWLObjectVisitorAdapter;
import org.semanticweb.owl.vocab.Namespaces;
import org.semanticweb.owl.vocab.OWLRestrictedDataRangeFacetVocabulary;

import java.io.IOException;
import java.io.Writer;
import java.net.URI;
import java.net.URL;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.util.*;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Feb 12, 2008<br><br>
 */
public class OWLHTMLVisitor  extends OWLObjectVisitorAdapter {

    private Logger logger = Logger.getLogger(getClass().getName());

    // These should match the css class names
    private static final String CSS_DEPRECATED = "deprecated";
    private static final String CSS_ACTIVE_ENTITY = "active-entity";
    private static final String CSS_KEYWORD = "keyword";
    private static final String CSS_ONTOLOGY_URI = "ontology-uri";
    private static final String CSS_ACTIVE_ONTOLOGY_URI = "active-ontology-uri";
    private static final String CSS_SOME = "some";
    private static final String CSS_ONLY = "only";
    private static final String CSS_VALUE = "value";
    private static final String CSS_LITERAL = "literal";
    private static final String CSS_ANNOTATION_URI = "annotation-uri";


    private Writer out;

    private URL pageURL = null;

    private NamedObjectURLRenderer urlRenderer;

    private NamedObjectShortFormProvider sfProvider;

    private Set<OWLOntology> ontologies = new HashSet<OWLOntology>();

    private OWLOntology activeOntology = null;

    private int indent = 0;

    private OWLHTMLConstants.LinkTarget targetWindow = null;


    public OWLHTMLVisitor(NamedObjectURLRenderer urlRenderer, NamedObjectShortFormProvider sfProvider, Writer out) {
        this.urlRenderer = urlRenderer;
        this.sfProvider = sfProvider;
        this.out = out;
    }

    /**
     * Sets the current page URL. All links will be rendered relative to this page
     * @param pageURL
     */
    public void setPageURL(URL pageURL){
        this.pageURL = pageURL;
    }

    public void setOntologies(Set<OWLOntology> ontologies){
        this.ontologies = ontologies;
    }

    public void setActiveOntology(OWLOntology activeOnt){
        this.activeOntology = activeOnt;
    }

    /**
     * Sets the indentation level of subsequent lines used when wrapping some expressions
     * @param indent
     */
    public void setIndentation(int indent){
        this.indent = indent;
    }

    /**
     * Sets the target window of any links - in case frames or popups are required
     * @param targetWindow
     */
    public void setContentTargetWindow(OWLHTMLConstants.LinkTarget targetWindow){
        this.targetWindow = targetWindow;
    }

    private void write(String s) {
        try {
            out.write(s);
        }
        catch (IOException e) {
            logger.error(e);
        }
    }

    ////////// Ontology
    public void visit(OWLOntology ontology) {
        final URL urlForOntology = urlRenderer.getURLForNamedObject(ontology);
        String link = urlForOntology.toString();
        String cssClass = CSS_ONTOLOGY_URI;
        if (activeOntology != null && ontology.equals(activeOntology)){
            cssClass = CSS_ACTIVE_ONTOLOGY_URI;
        }

        boolean writeLink = false;

        if (pageURL == null){
            writeLink = true;
        }
        else{
            if (!pageURL.equals(urlForOntology)){
                link = URLUtils.createRelativeURL(pageURL, urlForOntology);
                writeLink = true;
            }
            else{
                write("<span class='" + cssClass + "'>");
                writeOntologyURIWithBoldFragment(ontology);
                write("</span>");
            }
        }

        if (writeLink){
            write("<a class='" + cssClass + "'");
            write(" href=\"" + link + "\" title='" + ontology.getURI() + "'");
            if (targetWindow != null){
                write(" target=\"" + targetWindow + "\"");
            }
            write(">");
            writeOntologyURIWithBoldFragment(ontology);
            write("</a>");
        }

        write(" <span style='color:gray;'>(" +
              "c:" + ontology.getReferencedClasses().size() +
              ", op:" + ontology.getReferencedObjectProperties().size() +
              ", dp:" + ontology.getReferencedDataProperties().size() +
              ", i:" + ontology.getReferencedIndividuals().size() +
              ")</span>");
    }

    public void visit(OWLImportsDeclaration axiom) {
        writeKeyword("imports: ");
        URI uri = axiom.getImportedOntologyURI();
        OWLOntology loadedOnt = null;
        for (OWLOntology ont : ontologies){
            if (ont.getURI().equals(uri)){
                loadedOnt = ont;
                break;
            }
        }
        if (loadedOnt != null){
            loadedOnt.accept(this);
        }
        else{
            write(uri.toString());//writeOntologyURIWithBoldFragment(uri);
        }
        writeAnnotations(axiom);
    }

    ////////// Entities

    public void visit(OWLClass desc) {
        writeOWLEntity(desc);
    }

    public void visit(OWLDataProperty property) {
        writeOWLEntity(property);
    }

    public void visit(OWLObjectProperty property) {
        writeOWLEntity(property);
    }

    public void visit(OWLIndividual individual) {
        writeOWLEntity(individual);
    }

    ///////// Anonymous classes

    public void visit(OWLObjectSomeRestriction desc) {
        desc.getProperty().accept(this);
        writeKeyword(" some ", CSS_SOME);
        writeOp(desc.getFiller());
    }

    public void visit(OWLObjectAllRestriction desc) {
        desc.getProperty().accept(this);
        writeKeyword(" only ", CSS_ONLY);
        writeOp(desc.getFiller());
    }

    public void visit(OWLObjectValueRestriction desc) {
        desc.getProperty().accept(this);
        writeKeyword(" value ", CSS_VALUE);
        writeOp(desc.getValue());
    }

    public void visit(OWLObjectMinCardinalityRestriction desc) {
        writeCardinalityRestriction(desc, "min");
    }

    public void visit(OWLObjectExactCardinalityRestriction desc) {
        writeCardinalityRestriction(desc, "exactly");
    }

    public void visit(OWLObjectMaxCardinalityRestriction desc) {
        writeCardinalityRestriction(desc, "max");
    }

    public void visit(OWLObjectComplementOf desc) {
        writeKeyword("not ");
        writeOp(desc.getOperand());
    }

    public void visit(OWLObjectSelfRestriction desc) {
        writeKeyword("self");
    }

    public void visit(OWLObjectIntersectionOf desc) {
        List<OWLDescription> orderedOps = orderOps(desc.getOperands());

        for (Iterator<OWLDescription> i = orderedOps.iterator(); i.hasNext();) {
            writeOp(i.next());
            if (i.hasNext()){
                writeKeyword(" and ");
                if (indent > 0){
                    write("<br>"); // cannot use <br /> in java browser
                    writeIndent();
                }
            }
        }
    }

    public void visit(OWLObjectUnionOf desc) {
        for (Iterator<OWLDescription> i = desc.getOperands().iterator(); i.hasNext();) {
            writeOp(i.next());
            if (i.hasNext()){
                writeKeyword(" or ");
            }
        }
    }

    public void visit(OWLObjectOneOf desc) {
        write("{");
        for (Iterator<OWLIndividual> i = desc.getIndividuals().iterator(); i.hasNext();) {
            writeOp(i.next());
            if (i.hasNext()){
                write(", ");
            }
        }
        write("}");
    }

    public void visit(OWLDataOneOf desc) {
        write("{");
        for (Iterator<OWLConstant> i = desc.getValues().iterator(); i.hasNext();) {
            writeOp(i.next());
            if (i.hasNext()){
                write(", ");
            }
        }
        write("}");
    }

    public void visit(OWLDataSomeRestriction desc) {
        desc.getProperty().accept(this);
        writeKeyword(" some ", CSS_SOME);
        writeOp(desc.getFiller());
    }

    public void visit(OWLDataAllRestriction desc) {
        desc.getProperty().accept(this);
        writeKeyword(" only ", CSS_ONLY);
        writeOp(desc.getFiller());
    }

    public void visit(OWLDataValueRestriction desc) {
        desc.getProperty().accept(this);
        writeKeyword(" value ", CSS_VALUE);
        writeOp(desc.getValue());
    }

    public void visit(OWLDataMinCardinalityRestriction desc) {
        writeCardinalityRestriction(desc, "min");
    }

    public void visit(OWLDataExactCardinalityRestriction desc) {
        writeCardinalityRestriction(desc, "exactly");
    }

    public void visit(OWLDataMaxCardinalityRestriction desc) {
        writeCardinalityRestriction(desc, "max");
    }

    public void visit(OWLDataRangeRestriction node) {
        node.getDataRange().accept(this);
        write(" [");

        for (Iterator<OWLDataRangeFacetRestriction> i = node.getFacetRestrictions().iterator(); i.hasNext();) {
            i.next().accept(this);
            if (i.hasNext()){
                write(", ");
            }
        }
        write("]");
    }

    public void visit(OWLDataRangeFacetRestriction node) {
        writeKeyword(writeFacet(node.getFacet()));
        node.getFacetValue().accept(this);
    }

    public void visit(OWLDataComplementOf node) {
        writeKeyword("not ");
        writeOp(node.getDataRange());
    }

    ////////// Properties

    public void visit(OWLObjectPropertyInverse property) {
        writeKeyword("inv");
        writeOp(property.getInverse());
    }

    public void visit(OWLFunctionalObjectPropertyAxiom axiom) {
        writeUnaryPropertyAxiom(axiom, "functional");
    }

    public void visit(OWLInverseFunctionalObjectPropertyAxiom axiom) {
        writeUnaryPropertyAxiom(axiom, "inverse-functional");
    }

    public void visit(OWLSymmetricObjectPropertyAxiom axiom) {
        writeUnaryPropertyAxiom(axiom, "symmetric");
    }

    public void visit(OWLTransitiveObjectPropertyAxiom axiom) {
        writeUnaryPropertyAxiom(axiom, "transitive");
    }

    public void visit(OWLAntiSymmetricObjectPropertyAxiom axiom) {
        writeUnaryPropertyAxiom(axiom, "anti-symmetric");
    }

    public void visit(OWLReflexiveObjectPropertyAxiom axiom) {
        writeUnaryPropertyAxiom(axiom, "reflexive");
    }

    public void visit(OWLIrreflexiveObjectPropertyAxiom axiom) {
        writeUnaryPropertyAxiom(axiom, "irreflexive");
    }

    public void visit(OWLObjectPropertyDomainAxiom axiom) {
        writeOp(axiom.getProperty());
        writeKeyword(" domain ");
        writeOp(axiom.getDomain());
        writeAnnotations(axiom);
    }

    public void visit(OWLObjectPropertyRangeAxiom axiom) {
        writeOp(axiom.getProperty());
        writeKeyword(" range ");
        writeOp(axiom.getRange());
        writeAnnotations(axiom);
    }

    public void visit(OWLInverseObjectPropertiesAxiom axiom) {
        writeOp(axiom.getFirstProperty());
        writeKeyword(" inverse ");
        writeOp(axiom.getSecondProperty());
        writeAnnotations(axiom);
    }

    public void visit(OWLObjectPropertyChainSubPropertyAxiom axiom) {
        for (Iterator<OWLObjectPropertyExpression> i = axiom.getPropertyChain().iterator(); i.hasNext();) {
            writeOp(i.next());
            if (i.hasNext()){
                writeKeyword(" . ");
            }
        }
        write(" -> ");
        writeOp(axiom.getSuperProperty());
        writeAnnotations(axiom);
    }

    public void visit(OWLDataPropertyDomainAxiom axiom) {
        writeOp(axiom.getProperty());
        writeKeyword(" domain ");
        writeOp(axiom.getDomain());
        writeAnnotations(axiom);
    }

    public void visit(OWLDataPropertyRangeAxiom axiom) {
        writeOp(axiom.getProperty());
        writeKeyword(" range ");
        writeOp(axiom.getRange());
        writeAnnotations(axiom);
    }

    public void visit(OWLFunctionalDataPropertyAxiom axiom) {
        writeUnaryPropertyAxiom(axiom, "functional");
        writeAnnotations(axiom);
    }

    ////////// Annotations

    public void visit(OWLEntityAnnotationAxiom axiom) {
        writeAnnotationAxiom(axiom);
    }

    public void visit(OWLOntologyAnnotationAxiom axiom) {
        writeAnnotationAxiom(axiom);
    }

    public void visit(OWLAxiomAnnotationAxiom axiom) {
        writeAnnotationAxiom(axiom);
    }

    public void visit(OWLConstantAnnotation annotation) {
        writeAnnotation(annotation);
    }

    public void visit(OWLObjectAnnotation annotation) {
        writeAnnotation(annotation);
    }

    public void visit(OWLUntypedConstant node) {
        write("<span class='" + CSS_LITERAL + "'>\"");
        writeLiteralContents(node.getLiteral());
        write("\"");
        final String lang = node.getLang();
        if (lang != null){
            write(" <span style='color: black;'>(" + lang + ")</span>");
        }
        write("</span>");
    }

    public void visit(OWLTypedConstant node) {
        write("<span class='" + CSS_LITERAL + "'>\"");
        writeLiteralContents(node.getLiteral());
        write("\"</span> (");
        node.getDataType().accept(this);
        write(")");
    }

    public void visit(OWLDataType node) {
        URI uri = node.getURI();
        final String base = getBase(uri) + "#";
        if (base.equals(Namespaces.XSD.toString())){
            write(uri.getFragment());
        }
        else{
            write(uri.toString());
        }
    }

    /////////// Axioms

    public void visit(OWLEquivalentClassesAxiom axiom) {
        final List<OWLDescription> ops = orderOps(axiom.getDescriptions());
        for (Iterator<OWLDescription> i = ops.iterator(); i.hasNext();) {
            i.next().accept(this);
            if (i.hasNext()){
                writeKeyword(" <-> ");
            }
        }
        writeAnnotations(axiom);
    }

    public void visit(OWLEquivalentObjectPropertiesAxiom axiom) {
        final Set<OWLObjectPropertyExpression> ops = axiom.getProperties();
        for (Iterator<OWLObjectPropertyExpression> i = ops.iterator(); i.hasNext();) {
            i.next().accept(this);
            if (i.hasNext()){
                writeKeyword(" <-> ");
            }
        }
        writeAnnotations(axiom);
    }

    public void visit(OWLEquivalentDataPropertiesAxiom axiom) {
        final Set<OWLDataPropertyExpression> ops = axiom.getProperties();
        for (Iterator<OWLDataPropertyExpression> i = ops.iterator(); i.hasNext();) {
            i.next().accept(this);
            if (i.hasNext()){
                writeKeyword(" <-> ");
            }
        }
        writeAnnotations(axiom);
    }

    public void visit(OWLSameIndividualsAxiom axiom) {
        final Set<OWLIndividual> ops = axiom.getIndividuals();
        for (Iterator<OWLIndividual> i = ops.iterator(); i.hasNext();) {
            i.next().accept(this);
            if (i.hasNext()){
                writeKeyword(" <-> ");
            }
        }
        writeAnnotations(axiom);
    }

    public void visit(OWLSubClassAxiom axiom) {
        axiom.getSubClass().accept(this);
        writeKeyword(" -> ");
        axiom.getSuperClass().accept(this);
        writeAnnotations(axiom);
    }

    public void visit(OWLObjectSubPropertyAxiom axiom) {
        axiom.getSubProperty().accept(this);
        writeKeyword(" -> ");
        axiom.getSuperProperty().accept(this);
        writeAnnotations(axiom);
    }

    public void visit(OWLDataSubPropertyAxiom axiom) {
        axiom.getSubProperty().accept(this);
        writeKeyword(" -> ");
        axiom.getSuperProperty().accept(this);
        writeAnnotations(axiom);
    }

    public void visit(OWLDisjointClassesAxiom axiom) {
        final Set<OWLDescription> ops = axiom.getDescriptions();
        for (Iterator<OWLDescription> i = ops.iterator(); i.hasNext();) {
            i.next().accept(this);
            if (i.hasNext()){
                writeKeyword(" != ");
            }
        }
        writeAnnotations(axiom);
    }

    public void visit(OWLDisjointDataPropertiesAxiom axiom) {
        final Set<OWLDataPropertyExpression> ops = axiom.getProperties();
        for (Iterator<OWLDataPropertyExpression> i = ops.iterator(); i.hasNext();) {
            i.next().accept(this);
            if (i.hasNext()){
                writeKeyword(" != ");
            }
        }
        writeAnnotations(axiom);
    }

    public void visit(OWLDisjointObjectPropertiesAxiom axiom) {
        final Set<OWLObjectPropertyExpression> ops = axiom.getProperties();
        for (Iterator<OWLObjectPropertyExpression> i = ops.iterator(); i.hasNext();) {
            i.next().accept(this);
            if (i.hasNext()){
                writeKeyword(" != ");
            }
        }
        writeAnnotations(axiom);
    }

    public void visit(OWLDifferentIndividualsAxiom axiom) {
        final Set<OWLIndividual> ops = axiom.getIndividuals();
        for (Iterator<OWLIndividual> i = ops.iterator(); i.hasNext();) {
            i.next().accept(this);
            if (i.hasNext()){
                writeKeyword(" != ");
            }
        }
        writeAnnotations(axiom);
    }

    public void visit(OWLDisjointUnionAxiom axiom) {
        write(axiom.toString());
        writeAnnotations(axiom);
    }

    public void visit(OWLDeclarationAxiom axiom) {
        final OWLEntity entity = axiom.getEntity();
        if (entity instanceof OWLClass){
            write("class: ");
        }
        else if (entity instanceof OWLObjectProperty){
            write("object property: ");
        }
        else if (entity instanceof OWLDataProperty){
            write("data property: ");
        }
        else if (entity instanceof OWLIndividual){
            write("individual:");
        }
        else if (entity instanceof OWLDataType){
            write("datatype: ");
        }
        entity.accept(this);
        writeAnnotations(axiom);
    }

    /////// OWLIndividual assertions

    public void visit(OWLClassAssertionAxiom axiom) {
        axiom.getIndividual().accept(this);
        write(": ");
        axiom.getDescription().accept(this);
        writeAnnotations(axiom);
    }

    public void visit(OWLObjectPropertyAssertionAxiom axiom) {
        writeAssertionAxiom(axiom);
    }

    public void visit(OWLDataPropertyAssertionAxiom axiom) {
        writeAssertionAxiom(axiom);
    }

    public void visit(OWLNegativeObjectPropertyAssertionAxiom axiom) {
        writeAssertionAxiom(axiom);
    }

    public void visit(OWLNegativeDataPropertyAssertionAxiom axiom) {
        writeAssertionAxiom(axiom);
    }

    private String getName(OWLEntity entity){
        return sfProvider.getShortForm(entity);
    }

    private String getBase(URI uri){
        String uriStr = uri.toString();
        String fragment = uri.getFragment();
        if (fragment != null){
            return uriStr.substring(0, uriStr.length()-uri.getFragment().length()-1);
        }
        else{
            return uri.toString();
        }
    }

    // just make sure a named class is first if there is one
    private List<OWLDescription> orderOps(Set<OWLDescription> ops) {
        List<OWLDescription> orderedOps = new ArrayList<OWLDescription>(ops);
        Collections.sort(orderedOps, new Comparator<OWLDescription>(){
            public int compare(OWLDescription d1, OWLDescription d2) {
                if (d1 instanceof OWLClass){
                    return -1;
                }
                else if (d2 instanceof OWLClass){
                    return 1;
                }
                return 0;
            }
        });
        return orderedOps;
    }

    private void writeOntologyURIWithBoldFragment(OWLOntology ont) {
        write(sfProvider.getShortForm(ont));
    }

    // add a span to allow for css highlighting
    private void writeKeyword(String keyword) {
        write("<span class='" + CSS_KEYWORD + "'>" + keyword + "</span>");
    }

    // add a span to allow for css highlighting
    private void writeKeyword(String keyword, String cssClass) {
        write("<span class='" + cssClass + "'>" + keyword + "</span>");
    }

    // useful to add brackets around the anonymous operators of unions and intersections and the fillers of restrictions
    private void writeOp(OWLObject op) {
        if (op instanceof OWLEntity ||
            op instanceof OWLObjectOneOf ||
            op instanceof OWLDataOneOf ||
            op instanceof OWLDataRangeRestriction){
            op.accept(this);
        }
        else{ // provide brackets for clarity
            write("(");
            op.accept(this);
            write(")");
        }
    }

    private void writeIndent() {
        for (int i=0; i<indent; i++){
            write("&nbsp;");
        }
    }

    public void writeOWLEntity(OWLEntity entity) {
        final URI uri = entity.getURI();

        String name = getName(entity);

        Set<String> cssClasses = new HashSet<String>();
        if (ModelUtil.isDeprecated(entity, ontologies)){
            cssClasses.add(CSS_DEPRECATED);
        }

        if (pageURL == null){
            final URL urlForTarget = urlRenderer.getURLForNamedObject(entity);
            write("<a href=\"" + urlForTarget + "\"");
            if (targetWindow != null){
                write(" target=\"" + targetWindow + "\"");
            }
            writeCSSClasses(cssClasses);
            write(" title=\"" + uri + "\">" + name + "</a>");
        }
        else{
            OWLNamedObject currentTarget = urlRenderer.getNamedObjectForURL(pageURL);
            if (currentTarget != null && currentTarget.equals(entity)){
                cssClasses.add(CSS_ACTIVE_ENTITY);
                write("<span");
                writeCSSClasses(cssClasses);
                write(">" + name + "</span>");
            }
            else{
                final URL urlForTarget = urlRenderer.getURLForNamedObject(entity);
                write("<a href=\"" + URLUtils.createRelativeURL(pageURL, urlForTarget) + "\"");
                if (targetWindow != null){
                    write(" target=\"" + targetWindow + "\"");
                }
                writeCSSClasses(cssClasses);
                write(" title=\"" + uri + "\">" + name + "</a>");
            }
        }
    }


    private void writeCardinalityRestriction(OWLCardinalityRestriction desc, String cardinalityType) {
        desc.getProperty().accept(this);
        writeKeyword(" " + cardinalityType + " ", cardinalityType);
        write(desc.getCardinality() + " ");
        if (desc.getFiller() != null){
            writeOp(desc.getFiller());
        }
    }

    private void writeUnaryPropertyAxiom(OWLUnaryPropertyAxiom axiom, String keyword) {
        writeKeyword(keyword);
        write(" (");
        writeOp(axiom.getProperty());
        write(")");
        writeAnnotations(axiom);
    }

    // OWLObjectVisitor doesn't appear to support facet yet
    private String writeFacet(OWLRestrictedDataRangeFacetVocabulary facet) {
        // need to make ranges HTML safe
        if (facet.equals(OWLRestrictedDataRangeFacetVocabulary.MIN_INCLUSIVE)) return "&gt;=";
        else if (facet.equals(OWLRestrictedDataRangeFacetVocabulary.MIN_EXCLUSIVE)) return "&gt;";
        else if (facet.equals(OWLRestrictedDataRangeFacetVocabulary.MAX_INCLUSIVE)) return "&lt;=";
        else if (facet.equals(OWLRestrictedDataRangeFacetVocabulary.MAX_EXCLUSIVE)) return "&lt;";
        return facet.getSymbolicForm();
    }

    // @@TODO literal should use <pre> to make sure that fomatting inside the string doesn't disrupt the html
    // but it appears java ignores this tag so for now just disable tags completely
    private void writeLiteralContents(String literal) {
        boolean writtenExternalRef = false;
        try {
            URI uri = new URI(literal);
            if (uri.isAbsolute()){
                write("<a href='" + uri + "' target='ext_ref'>" + uri + "</a>");
                writtenExternalRef = true;
            }
        }
        catch (URISyntaxException e) {
            // do nothing
        }
        finally{
            if (!writtenExternalRef){
                literal = literal.replace("<", "&lt;");
                literal = literal.replace(">", "&gt;");
                write(literal);
            }
        }
    }

    private void writeAnnotationAxiom(OWLAnnotationAxiom axiom) {
        writeKeyword(" /*&nbsp;", "comment");
        axiom.getAnnotation().accept(this);
        writeKeyword("&nbsp;*/", "comment");
        writeAnnotations(axiom); // in theory, you could annotate the annotation axioms !!
    }

    private void writeAnnotations(OWLAxiom axiom) {
        for (OWLOntology ont : ontologies){
            for (OWLAxiomAnnotationAxiom annot : axiom.getAnnotationAxioms(ont)){
                annot.accept(this);
            }
        }
    }

    private void writeAssertionAxiom(OWLPropertyAssertionAxiom axiom) {
        axiom.getSubject().accept(this);
        write(" ");
        axiom.getProperty().accept(this);
        write(" ");
        axiom.getObject().accept(this);
        writeAnnotations(axiom);
    }

    private void writeAnnotation(OWLAnnotation annotation) {
        URI uri = annotation.getAnnotationURI();
        String annotLabel = uri.getFragment();
        if (annotLabel == null){
            annotLabel = uri.toString();
        }
        write("<span class='" + CSS_ANNOTATION_URI + "'>" + annotLabel + ":</span> ");
        annotation.getAnnotationValue().accept(this);
    }

    private void writeCSSClasses(Set<String> cssClasses) {
        if (!cssClasses.isEmpty()){
            boolean started = false;
            write(" class='");
            for (String cls : cssClasses){
                if (started){
                    write(" ");
                }
                write(cls);
                started = true;
            }
            write("'");
        }
    }
}
