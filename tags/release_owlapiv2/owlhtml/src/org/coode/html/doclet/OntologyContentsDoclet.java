/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.doclet;

import org.coode.html.OWLHTMLServer;
import org.coode.html.impl.OWLHTMLConstants;
import org.coode.owl.mngr.NamedObjectType;
import org.semanticweb.owl.model.OWLOntology;

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

    public OntologyContentsDoclet(OWLHTMLServer server) {
        super(server);
    }

    public void setTitle(String title){
        this.title = title;
    }

    protected void renderHeader(URL pageURL, PrintWriter out) {
        OWLOntology ont = getUserObject();

        int classCount = ont.getReferencedClasses().size();
        int objPropCount = ont.getReferencedObjectProperties().size();
        int dataPropCount = ont.getReferencedDataProperties().size();
        int indCount = ont.getReferencedIndividuals().size();

        renderBoxStart(getTitle(pageURL), getOntologyName(), out);

        out.println("<ul>");

        renderIndexLink(classCount, NamedObjectType.classes, pageURL, out);
        renderIndexLink(objPropCount, NamedObjectType.objectproperties, pageURL, out);
        renderIndexLink(dataPropCount, NamedObjectType.dataproperties, pageURL, out);
        renderIndexLink(indCount, NamedObjectType.individuals, pageURL, out);

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
            URL ontURL = getServer().getURLScheme().getURLForNamedObject(getUserObject());
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
        return getServer().getNameRenderer().getShortForm(getUserObject());
    }

    private void renderIndexLink(int count, NamedObjectType type, URL pageURL, PrintWriter out) {
        if (count > 0){
            URL indexURL = getServer().getURLScheme().getURLForOntologyIndex(getUserObject(), type);
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
