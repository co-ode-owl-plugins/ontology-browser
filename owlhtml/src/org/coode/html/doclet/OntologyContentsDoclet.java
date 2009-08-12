/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.doclet;

import org.coode.html.OWLHTMLKit;
import org.coode.html.impl.OWLHTMLConstants;
import org.coode.owl.mngr.NamedObjectType;
import org.coode.owl.util.ModelUtil;
import org.semanticweb.owlapi.model.OWLOntology;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.net.URL;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jan 25, 2008<br><br>
 */
public class OntologyContentsDoclet extends AbstractOWLDocDoclet<OWLOntology> {

    public static final String ID = "doclet.contents";

    private String title;

    public OntologyContentsDoclet(OWLHTMLKit kit) {
        super(kit);
    }

    public void setTitle(String title){
        this.title = title;
    }

    protected void renderHeader(URL pageURL, PrintWriter out) {
        OWLOntology ont = getUserObject();

        renderBoxStart(getTitle(pageURL), getOntologyName(), out);

        out.println("<ul>");

        for (NamedObjectType type : NamedObjectType.entitySubtypes()){
            int count = ModelUtil.getOWLEntitiesFromOntology(type, ont).size();
            renderIndexLink(count, type, pageURL, out);
        }

        out.println("</ul>");
    }

    protected void renderFooter(URL pageURL, PrintWriter out) {
        renderBoxEnd(getOntologyName(), out);
    }

    private String getTitle(URL pageURL) {
        if (title != null){
            return title;
        }
        else{
            // create link text
            URL ontURL = getHTMLGenerator().getURLScheme().getURLForOWLObject(getUserObject());
            StringWriter writer = new StringWriter();
            PrintWriter out = new PrintWriter(writer);
            renderLink(getOntologyName(), ontURL,
                       OWLHTMLConstants.LinkTarget.content,
                       null, isSingleFrameNavigation(), pageURL, out);
            out.flush();
            return writer.getBuffer().toString();
        }
    }

    private String getOntologyName() {
        return getHTMLGenerator().getOWLServer().getOntologyShortFormProvider().getShortForm(getUserObject());
    }

    private void renderIndexLink(int count, NamedObjectType type, URL pageURL, PrintWriter out) {
        if (count > 0){
            URL indexURL = getHTMLGenerator().getURLScheme().getURLForOntologyIndex(getUserObject(), type);
            out.println("<li>");
            String label = type.getPluralRendering();
            renderLink(label, indexURL, OWLHTMLConstants.LinkTarget.subnav, null, isSingleFrameNavigation(), pageURL, out);
            out.println(" (" + count + ")");
            out.println("</li>");
        }
    }

    public String getID() {
        return ID;
    }
}
