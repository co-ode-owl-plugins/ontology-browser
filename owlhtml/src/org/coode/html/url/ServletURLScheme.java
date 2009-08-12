package org.coode.html.url;

import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLObject;
import org.coode.html.impl.OWLHTMLConstants;
import org.coode.html.impl.OWLHTMLParam;
import org.coode.html.OWLHTMLKit;
import org.coode.html.util.URLUtils;
import org.coode.owl.mngr.NamedObjectType;
import org.coode.owl.mngr.ServerConstants;
import org.apache.log4j.Logger;

import java.net.*;
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

    private String additionalLinkArguments;

    public ServletURLScheme(OWLHTMLKit kit) {
        super(kit);
    }


    public URL getURLForOWLObject(OWLObject owlObject) {
        try {
            URL url = null;
            URI uri = null;
            String type = null;
            if (owlObject instanceof OWLEntity){
                uri = ((OWLEntity)owlObject).getURI();
                type = NamedObjectType.getType((OWLEntity)owlObject).toString();
            }
            else if (owlObject instanceof OWLOntology){
                uri = ((OWLOntology)owlObject).getOntologyID().getOntologyIRI().toURI();
                type = NamedObjectType.ontologies.toString();
            }

            if (uri != null && type != null){
                String encodedURI = URLEncoder.encode(uri.toString(), OWLHTMLConstants.DEFAULT_ENCODING);
                String partialURL = type + "/" + OWLHTMLConstants.START_QUERY + OWLHTMLParam.uri + OWLHTMLConstants.EQUALS + encodedURI;

                if (additionalLinkArguments != null){
                    partialURL += additionalLinkArguments;
                }

                url = new URL(getBaseURL(), partialURL);
            }
            return url;
        }
        catch (Exception e) {
            throw new RuntimeException(e);
        }
    }


    public OWLEntity getOWLObjectForURL(URL url) {
        try {
            OWLEntity object = null;

            Map<OWLHTMLParam, String> paramMap = URLUtils.getParams(url);
            if (!paramMap.isEmpty()){
                String uriStr = paramMap.get(OWLHTMLParam.uri);
                if (uriStr != null){
                    URI uri = new URI(URLDecoder.decode(uriStr, OWLHTMLConstants.DEFAULT_ENCODING));
                    NamedObjectType type = getType(url);
                    object = getOWLEntity(uri, type);
                }
            }
            return object;
        }
        catch (Exception e) {
            throw new RuntimeException(e);
        }
    }


    public URL getURLForOntologyIndex(OWLOntology ont, NamedObjectType type) {
        try {
            String encodedURI = URLEncoder.encode(ont.getOntologyID().getOntologyIRI().toString(), OWLHTMLConstants.DEFAULT_ENCODING);

            return new URL(getBaseURL(), type + "/" + OWLHTMLConstants.START_QUERY + OWLHTMLParam.ontology + OWLHTMLConstants.EQUALS + encodedURI);
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

    protected OWLEntity getOWLEntity(URI uri, NamedObjectType type){
        return kit.getOWLServer().getFinder().getOWLEntities(uri, type).iterator().next(); // return the first
    }
}
