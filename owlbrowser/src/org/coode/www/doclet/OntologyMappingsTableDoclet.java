/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.www.doclet;

import org.apache.log4j.Logger;
import org.coode.html.OWLHTMLKit;
import org.coode.html.doclet.AbstractOWLDocDoclet;
import org.coode.html.impl.OWLHTMLConstants;
import org.coode.html.impl.OWLHTMLParam;
import org.coode.html.renderer.OWLHTMLRenderer;
import org.coode.html.util.HTMLUtils;
import org.coode.owl.mngr.NamedObjectType;
import org.coode.www.OntologyAction;
import org.coode.www.OntologyBrowserConstants;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyID;

import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
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

    private static final String TITLE = "Loaded Ontologies";

    private Map<OWLOntologyID, URI> map;


    public OntologyMappingsTableDoclet(OWLHTMLKit kit) {
        super(kit);
    }

    public void setMap(Map<OWLOntologyID, URI> map){
        this.map = map;
    }

    protected void renderHeader(URL pageURL, PrintWriter out) {
        OWLHTMLKit kit = getOWLHTMLKit();

        renderBoxStart(null, out, pageURL);

        final boolean missingValues = map.containsValue(null);
        if (missingValues){
            out.println("<form method='POST' action='.'>");
        }
        else{
            out.println("<form id='specify' method='POST' action='.' target='_top' >");
        }

        out.println("<table class='ontologytable'>");

//        out.println("<tr><th>Ontology (hover for full URI)</th><th>Physical Location</th><th>Action</th></tr>");

        OWLHTMLRenderer owlRen = new OWLHTMLRenderer(kit);

        printLoadRow(out);

        Set<OWLOntology> visibleOnts = kit.getVisibleOntologies();

        for (OWLOntologyID ontURI : map.keySet()){
            URI physicalURI = map.get(ontURI);
            if (physicalURI != null){
                String css = "";
                final OWLOntology ont = kit.getOWLServer().getOWLOntologyManager().getOntology(ontURI);
                if (!visibleOnts.contains(ont)){
                    css = " class='disabled'";
                }
                if (ont.equals(kit.getOWLServer().getActiveOntology())){
                    css = " class='active-ontology-uri'";
                }
                try {
                    String encodedURI = URLEncoder.encode(ontURI.getDefaultDocumentIRI().toURI().toString(), OWLHTMLConstants.DEFAULT_ENCODING);
                    out.println("<tr" + css + ">");

                    // ontology name
                    out.print("<td class='ontologyname'>");
                    owlRen.render(ont, pageURL, out);
                    out.println("</td>");

                    // location
                    out.print("<td class='location'>");
                    out.print(physicalURI);
                    out.println("</td>");

                    out.print("<td class='actions'>");

                    for (OntologyAction action : OntologyAction.values()){
                        if (!action.equals(OntologyAction.load)){
                            try{
                                URL linkURL = new URL(kit.getURLScheme().getURLForIndex(NamedObjectType.ontologies) + "?" +
                                                      OWLHTMLParam.action + "=" + action + "&" +
                                                      OWLHTMLParam.uri + "=" + encodedURI);
                                HTMLUtils.renderLink(action.toString(),
                                                     linkURL,
                                                     OWLHTMLConstants.LinkTarget._top,
                                                     "",
                                                     isSingleFrameNavigation(),
                                                     pageURL,
                                                     out);
                            }
                            catch (MalformedURLException e) {
                                logger.error("Cannot create action URL: " + action);
                            }
                        }
                    }
                    out.println("</td>");
                    out.println("</tr>");
                }
                catch (UnsupportedEncodingException e) {
                    logger.error("Cannot encode URL: " + ontURI, e);
                }
            }
            else{
                out.println("<tr>");
                out.println("<td>" + ontURI + "</td>");
                out.println("<td><input class='location' name='" + ontURI + "' type='text' /></td>");
                out.println("<td></td>");
                out.println("</tr>");
            }
        }

        out.println("</table>");

        if (missingValues){
            out.println("<input name='action' type='submit' value='load' />");
        }
        out.println("</form>");
    }

    private void printLoadRow(PrintWriter out) {
        out.println("<tr><td></td><td>");
        out.print("        <input id='");
        out.print(OntologyBrowserConstants.LOAD_ONTOLOGIES_INPUT_ID);
        out.print("' name='");
        out.print(OWLHTMLParam.uri);
        out.println("' type='text' style='width:80%; margin-top: 0;' />");
        out.println("</td><td>");
        out.println("        <input name='action' type='submit' value='load' />");
        out.println("</td></tr>");
    }

    protected void renderFooter(URL pageURL, PrintWriter out) {
        renderBoxEnd(TITLE, out);
    }

    public String getID() {
        return ID;
    }
}
