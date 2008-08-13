/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.www.doclet;

import org.apache.log4j.Logger;
import org.coode.html.OWLHTMLServer;
import org.coode.html.doclet.AbstractOWLDocDoclet;
import org.coode.html.impl.OWLHTMLConstants;
import org.coode.html.renderer.OWLHTMLRenderer;
import org.coode.www.ManageAction;
import org.coode.www.OntologyBrowserConstants;
import org.semanticweb.owl.model.OWLOntology;

import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.net.URI;
import java.net.URL;
import java.net.URLEncoder;
import java.util.Map;
import java.util.Set;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jan 25, 2008<br><br>
 */
public class OntologyMappingsTableDoclet extends AbstractOWLDocDoclet {

    private static final Logger logger = Logger.getLogger(OntologyMappingsTableDoclet.class);

    public static final String ID = "doclet.ontology.mappings";

    private static final String TITLE = "Ontology Locations";

    private Map<URI, URI> map;


    public OntologyMappingsTableDoclet(OWLHTMLServer server) {
        super(server);
    }

    public void setMap(Map<URI, URI> map){
        this.map = map;
    }

    protected void renderHeader(URL pageURL, PrintWriter out) {
        OWLHTMLServer server = getServer();
        final Set<OWLOntology> visibleOnts = server.getVisibleOntologies();

        renderBoxStart(TITLE, out);

        final boolean missingValues = map.containsValue(null);
        if (missingValues){
            out.println("<form method='POST' action='.'>");
        }
        out.println("<table style='margin-bottom: 10px; width: 100%;'><tr><th>Ontology (hover for full URI)</th><th>Physical Location</th><th>Action</th></tr>");

        OWLHTMLRenderer owlRen = new OWLHTMLRenderer(server);

        for (URI ontURI : map.keySet()){
            URI physicalURI = map.get(ontURI);
            if (physicalURI != null){
                String css = "";
                final OWLOntology ont = server.getOWLOntologyManager().getOntology(ontURI);
                if (!visibleOnts.contains(ont)){
                    css = " class='disabled'";
                }
                if (ont.equals(server.getActiveOntology())){
                    css = " class='active-ontology-uri'";
                }
                try {
                    String encodedURI = URLEncoder.encode(ontURI.toString(), "UTF-8");
                    out.println("<tr" + css + "><td>");
                    owlRen.render(ont, pageURL, out);
                    out.println("</td><td>" + physicalURI + "</td><td style='width: 150px;'>");

                    for (ManageAction action : ManageAction.values()){
                        if (!action.equals(ManageAction.load)){
                            String linkURL = OWLHTMLConstants.MANAGE_HTML + "?" +
                                             OntologyBrowserConstants.PARAM_ACTION + "=" + action + "&" +
                                             OntologyBrowserConstants.PARAM_URI + "=" + encodedURI;
                            renderLink(action.toString(),
                                       server.getURLScheme().getURLForRelativePage(linkURL),
                                       OWLHTMLConstants.LinkTarget._top, "", isSingleFrameNavigation(), pageURL, out);
                        }
                    }
                    out.println("</td></tr>");
                }
                catch (UnsupportedEncodingException e) {
                    logger.error("Cannot encode URL: " + ontURI, e);
                }
            }
            else{
                out.println("<tr><td>" + ontURI + "</td><td><input style='width:100%;' name='"
                            + ontURI + "' type='text' /></td><td></td></tr>");
            }
        }

        out.println("</table>");
        if (missingValues){
            out.println("<input name='action' type='submit' value='load' />");
            out.println("</form>");
        }
    }

    protected void renderFooter(URL pageURL, PrintWriter out) {
        renderBoxEnd(TITLE, out);
    }

    public String getID() {
        return ID;
    }
}
