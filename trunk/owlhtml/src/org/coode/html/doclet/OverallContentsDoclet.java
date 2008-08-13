/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.doclet;

import org.coode.html.OWLHTMLServer;
import org.coode.html.impl.OWLHTMLConstants;
import org.coode.html.url.URLScheme;
import org.coode.owl.mngr.NamedObjectType;
import org.semanticweb.owl.model.OWLOntology;

import java.io.PrintWriter;
import java.net.URL;
import java.util.Set;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Feb 5, 2008<br><br>
 *
 * @@TODO seperate this off into doclets
 */
public class OverallContentsDoclet extends AbstractOWLDocDoclet{

    public static final String ID = "doclet.contents.extended";

    private String title = "Contents";

    public OverallContentsDoclet(OWLHTMLServer server, String title) {
        super(server);
        this.title = title;
    }

    protected void renderHeader(URL pageURL, PrintWriter out) {

        OWLHTMLServer server = getServer();

        int classTotal = 0;
        int objPropTotal = 0;
        int dataPropTotal = 0;
        int indTotal = 0;

        final Set<OWLOntology> visibleOntologies = server.getVisibleOntologies();
        final URLScheme urlScheme = server.getURLScheme();

        for (OWLOntology ont : visibleOntologies){
            classTotal += ont.getReferencedClasses().size();
            objPropTotal += ont.getReferencedObjectProperties().size();
            dataPropTotal += ont.getReferencedDataProperties().size();
            indTotal += ont.getReferencedIndividuals().size();
        }

        renderBoxStart(title, out);
        out.println("<ul>");
        if (visibleOntologies.size() > 1){
            out.println("<li>");
            renderLink("All " + NamedObjectType.ontologies.getPluralRendering(),
                       server.getURLScheme().getURLForIndex(NamedObjectType.ontologies),
                       OWLHTMLConstants.LinkTarget.subnav, null,
                    isSingleFrameNavigation(), pageURL, out);
        }
        else{
            final OWLOntology activeOnt = server.getActiveOntology();
            String ontID = server.getNameRenderer().getShortForm(activeOnt);
            final URL ontURL = urlScheme.getURLForNamedObject(activeOnt);
            out.println("<li>");
            renderLink(ontID, ontURL, OWLHTMLConstants.LinkTarget.content, null, isSingleFrameNavigation(), pageURL, out);
        }
        out.println("<li>");
        String indexAllURL = server.getProperties().get(OWLHTMLConstants.OPTION_INDEX_ALL_URL);
        renderLink(OWLHTMLConstants.ALL_ENTITIES_TITLE,
                   server.getURLScheme().getURLForRelativePage(indexAllURL),
                   OWLHTMLConstants.LinkTarget.subnav, null, isSingleFrameNavigation(), pageURL, out);

        renderIndexLink(classTotal, NamedObjectType.classes, pageURL, out);
        renderIndexLink(objPropTotal, NamedObjectType.objectproperties, pageURL, out);
        renderIndexLink(dataPropTotal, NamedObjectType.dataproperties, pageURL, out);
        renderIndexLink(indTotal, NamedObjectType.individuals, pageURL, out);

        out.println("</ul>");
        renderBoxEnd(title, out);
    }

    protected void renderFooter(URL pageURL, PrintWriter out) {
        // do nothing
    }

    private void renderIndexLink(int count, NamedObjectType type, URL pageURL, PrintWriter out) {
        if (count > 0){
            URL indexURL = getServer().getURLScheme().getURLForIndex(type);
            out.println("<li>");
            String label = "All " + type.getPluralRendering();
            renderLink(label, indexURL, OWLHTMLConstants.LinkTarget.subnav, null, isSingleFrameNavigation(), pageURL, out);
            out.println(" (" + count + ")");
            out.println("</li>");
        }
    }

    public String getID() {
        return ID;
    }

    public void setTitle(String title) {
        this.title = title;
    }
}