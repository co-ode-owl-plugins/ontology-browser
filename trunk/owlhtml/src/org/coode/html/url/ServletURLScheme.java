package org.coode.html.url;

import org.apache.log4j.Logger;
import org.coode.html.OWLHTMLServer;
import org.coode.html.util.URLUtils;
import org.coode.owl.mngr.NamedObjectType;
import org.semanticweb.owl.model.OWLNamedObject;
import org.semanticweb.owl.model.OWLOntology;

import java.net.URL;
import java.net.URLEncoder;
import java.util.Map;

/**
 * Author: Nick Drummond<br>
 * nick.drummond@cs.manchester.ac.uk<br>
 * http://www.cs.man.ac.uk/~drummond<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jun 11, 2007<br><br>
 * <p/>
 * code made available under Mozilla Public License (http://www.mozilla.org/MPL/MPL-1.1.html)<br>
 * copyright 2006, The University of Manchester<br>
 *
 * URL scheme for dynamic server-side resolution
 *
 * Creates URLs of the form:
 * serverbase/<type>/?name=<rendering>&baseURL=<namespace>
 *
 */
public class ServletURLScheme extends AbstractURLScheme {

    private static final Logger logger = Logger.getLogger(ServletURLScheme.class.getName());

    private static final String PARAM_URI = "uri";
    private static final String PARAM_ONTOLOGY = "ontology";

    private String additionalLinkArguments;

    public ServletURLScheme(OWLHTMLServer server) {
        super(server);
    }


    public URL getURLForNamedObject(OWLNamedObject object) {
        try {
            String encodedURI = URLEncoder.encode(object.getURI().toString(), "UTF-8");
            String partialURL = NamedObjectType.getType(object) + "/?" + PARAM_URI + "=" + encodedURI;

            if (additionalLinkArguments != null){
                partialURL += additionalLinkArguments;
            }

            return new URL(getBaseURL(), partialURL);
        }
        catch (Exception e) {
            logger.error(e);
        }
        return null;
    }

    
    public OWLNamedObject getNamedObjectForURL(URL url) {
        OWLNamedObject object = null;

        Map<String, String> paramMap = URLUtils.getParams(url);
        if (!paramMap.isEmpty()){
            String uri = paramMap.get(PARAM_URI);

            NamedObjectType type = getType(url);

            if (uri != null){
                object = server.getFinder().getOWLNamedObjects(uri, type).iterator().next(); // return the first

            }
        }
        return object;
    }


    public URL getURLForOntologyIndex(OWLOntology ont, NamedObjectType type) {
        try {
            String encodedURI = URLEncoder.encode(ont.getURI().toString(), "UTF-8");

            return new URL(getBaseURL(), type + "/?" + PARAM_ONTOLOGY + "=" + encodedURI);
        }
        catch (Exception e) {
            logger.error(e);
        }
        return null;
    }

    public String getFilenameForOntologyIndex(OWLOntology ont, NamedObjectType type) {
        return null; //not used for exporter so not implemented so far
    }

    public void setAdditionalLinkArguments(String s) {
        additionalLinkArguments = s;
    }

    public void clearAdditionalLinkArguments() {
        additionalLinkArguments = null;
    }
}
