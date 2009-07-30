/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.url;

import org.coode.html.OWLHTMLServer;
import org.coode.html.impl.OWLHTMLConstants;
import org.coode.owl.mngr.NamedObjectType;
import org.semanticweb.owl.model.OWLNamedObject;
import org.semanticweb.owl.model.OWLOntology;
import org.apache.log4j.Logger;

import java.net.MalformedURLException;
import java.net.URL;

import edu.unika.aifb.rdf.api.syntax.RDFParser;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jan 17, 2008<br><br>
 *
 * Wraps a give URLScheme, but provides an additional parameter for the permalink
 */
public class PermalinkURLScheme implements URLScheme {

    private static final Logger logger = Logger.getLogger(PermalinkURLScheme.class);

    private URLScheme baseScheme;
    private OWLHTMLServer server;


    public PermalinkURLScheme(URLScheme baseScheme, OWLHTMLServer server) {
        this.baseScheme = baseScheme;
        this.server = server;
    }

    private URL append(URL url) {
        if (server.getCurrentLabel() != null){
            String link = url.toString();
            if (!link.contains("?")){
                link += "?";
            }
            else{
                link += "&";
            }
            link += OWLHTMLConstants.PARAM_SESSION_LABEL + "=" + server.getCurrentLabel();

            try {
                return new URL(link);
            }
            catch (MalformedURLException e) {
                logger.error("Cannot create permalink URL", e);
            }
        }
        return url;
    }

    public URL getURLForNamedObject(OWLNamedObject object) {
        return append(baseScheme.getURLForNamedObject(object));
    }

    public OWLNamedObject getNamedObjectForURL(URL url) {
        return baseScheme.getNamedObjectForURL(url);
    }

    public NamedObjectType getType(URL url) {
        return baseScheme.getType(url);
    }

    public URL getBaseURL() {
        return baseScheme.getBaseURL();
    }

    public URL getURLForOntologyIndex(OWLOntology ont, NamedObjectType type) {
        return append(baseScheme.getURLForOntologyIndex(ont, type));
    }

    public String getFilenameForOntologyIndex(OWLOntology ont, NamedObjectType type) {
        return baseScheme.getFilenameForOntologyIndex(ont, type);
    }

    public void setAdditionalLinkArguments(String s) {
        baseScheme.setAdditionalLinkArguments(s);
    }

    public void clearAdditionalLinkArguments() {
        baseScheme.clearAdditionalLinkArguments();
    }

    public URL getURLForIndex(NamedObjectType type) {
        return append(baseScheme.getURLForIndex(type));
    }

    public URL getURLForRelativePage(String pageRelativeToBase) {
        return append(baseScheme.getURLForRelativePage(pageRelativeToBase));
    }

    public URL getURLForAbsolutePage(URL pageURL) {
        return append(baseScheme.getURLForAbsolutePage(pageURL));
    }
}
