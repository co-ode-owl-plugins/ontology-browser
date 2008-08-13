/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.www.doclet;

import org.apache.log4j.Logger;
import org.coode.html.OWLHTMLServer;
import org.coode.html.doclet.AbstractHTMLDoclet;
import org.coode.html.impl.OWLHTMLConstants;
import org.coode.owl.mngr.ServerConstants;
import org.coode.suggestor.api.FillerSuggestor;
import org.coode.suggestor.api.PropertySuggestor;
import org.coode.suggestor.api.SuggestorManager;
import org.semanticweb.owl.model.*;

import java.io.PrintWriter;
import java.net.URL;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Feb 5, 2008<br><br>
 */
public class SimpleQueryFormDoclet extends AbstractHTMLDoclet<OWLDescription> {

    private static final Logger logger = Logger.getLogger(SimpleQueryFormDoclet.class);

    private static final String ID = "doclet.query.form";

    private final OWLHTMLServer server;

    private SuggestorManager sm;


    public SimpleQueryFormDoclet(OWLHTMLServer server) {
        this.server = server;
    }

    public void setSuggestorManager(SuggestorManager sm){
        this.sm = sm;
    }

    protected void renderHeader(URL pageURL, PrintWriter out) {
        String rootName = "";
        Set<OWLDescription> intersectionElements = new HashSet<OWLDescription>();

        OWLDescription baseClass = getUserObject();

        if (baseClass instanceof OWLClass){
            rootName = server.getNameRenderer().getShortForm((OWLClass)baseClass);
        }
        else if (baseClass instanceof OWLObjectIntersectionOf){
            for (OWLDescription op : ((OWLObjectIntersectionOf)baseClass).getOperands()){
                if (op instanceof OWLClass){
                    rootName = server.getNameRenderer().getShortForm((OWLClass)op);
                }
                else{
                    intersectionElements.add(op);
                }
            }
        }

        renderBoxStart("Simple DL Query", out);

        AutocompleteDoclet autocomplete = new AutocompleteDoclet(server, "find", false);
        autocomplete.setParamName("name");
        autocomplete.setInitialValue(rootName);
        // @@TODO add the JS action

        // @@TODO insert the following content as doclets within the autocomplete
        if (sm != null){
            renderSuggestorContent(pageURL, out);
        }
        else{
            renderBasicContent(intersectionElements, pageURL, out);
        }

        renderBoxEnd("Simple DL Query", out);

        out.println("<div id='resultsForm'>");

        out.println("</div>");
    }


    private void renderSuggestorContent(URL pageURL, PrintWriter out) {
        try {

            OWLDescription baseClass = getUserObject();

            final PropertySuggestor ps = sm.getPropertySuggestor();
            final FillerSuggestor fs = sm.getFillerSuggestor();

            Set<OWLObjectProperty> sanctionedProps = new HashSet<OWLObjectProperty>();
            for (OWLObjectPropertyExpression sanctProp : ps.getSanctionedObjectProperties(baseClass, true)){
                if (sanctProp instanceof OWLObjectProperty){
                    sanctionedProps.add((OWLObjectProperty)sanctProp);
                }
            }

            final Set<OWLObjectPropertyExpression> currentProperties = ps.getCurrentObjectProperties(baseClass, true);
            for (OWLObjectPropertyExpression prop : sanctionedProps){
                if (prop instanceof OWLObjectProperty){
                    Set<OWLPropertyRange> sanctionedFillers = new HashSet<OWLPropertyRange>(fs.getSanctionedNamedFillers(baseClass, prop));
                    ConjunctDoclet conjunctRenderer = new ConjunctDoclet(new HashSet<OWLProperty>(Collections.singleton((OWLObjectProperty)prop)),
                                                                         sanctionedFillers,
                                                                         server);
                    conjunctRenderer.setRestrictionType(ServerConstants.SOME);
                    if (currentProperties.contains(prop)){
                        conjunctRenderer.setSelectedProperty((OWLObjectProperty)prop);
                        for (OWLClass filler : fs.getCurrentNamedFillers(baseClass, prop, true)){
                            conjunctRenderer.setSelectedFiller(filler);
                            out.println("<br />");
                            conjunctRenderer.renderAll(pageURL, out);
                        }
                    }
                    else{
                        out.println("<br />");
                        conjunctRenderer.renderAll(pageURL, out);
                    }
                }
            }
        }
        catch (Exception e) {
            logger.error("Cannot render suggested content", e);
        }
    }

    private void renderBasicContent(Set<OWLDescription> intersectionElements, URL pageURL, PrintWriter out) {
        try {
            for (OWLDescription op : intersectionElements){
                if (op instanceof OWLQuantifiedRestriction){
                    ConjunctDoclet renderer = new ConjunctDoclet((OWLQuantifiedRestriction)op, server);
                    out.println("<span><br />"); // requires a parent element (so we can remove the whole thing easily)
                    renderer.setRenderRemoveButton(true);
                    renderer.renderAll(pageURL, out);
                    out.println("</span>");
                }
            }
        }
        catch (Exception e) {
            logger.error("Cannot render simple content", e);
        }
    }

    protected void renderFooter(URL pageURL, PrintWriter out) {
        // do nothing
    }

    public Set<URL> getRequiredJS() {
        Set<URL> js = super.getRequiredJS();
        js.add(server.getURLScheme().getURLForRelativePage(OWLHTMLConstants.JS_FORM));
        return js;
    }

    public String getID() {
        return ID;
    }
}