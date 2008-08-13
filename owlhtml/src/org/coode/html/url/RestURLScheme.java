/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.url;

import org.apache.log4j.Logger;
import org.coode.html.OWLHTMLServer;
import org.coode.html.util.URLUtils;
import org.coode.owl.mngr.NamedObjectType;
import org.semanticweb.owl.model.OWLNamedObject;
import org.semanticweb.owl.model.OWLOntology;

import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLEncoder;
import java.util.HashSet;
import java.util.Set;

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
 * This will ONLY work if the hashCode() method for URIs is 1-1 mapping and completely deterministic
 * (for each vm the hash should be the same).
 * We may need to implement our own hash function if this is not the case.
 * It appears to work on a mac, but this will need to be double checked.
 *
 * Creates URLs of the form:
 * serverbase/<type>/<hash-of-object-uri>/
 *
 */
public class RestURLScheme extends AbstractURLScheme {

    private static final Logger logger = Logger.getLogger(RestURLScheme.class.getName());

    private static final String PARAM_ONTOLOGY = "ontology";

    private String additionalLinkArguments;

    public RestURLScheme(OWLHTMLServer server) {
        super(server);
    }


    public URL getURLForNamedObject(OWLNamedObject object) {
        try {
            String partialURL = NamedObjectType.getType(object) + "/" + object.getURI().hashCode() + "/";

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
        NamedObjectType type = getType(url);
        try {
            int hashCode = getID(url);
            if (type.equals(NamedObjectType.ontologies)){
                for (OWLOntology ont : server.getOWLOntologyManager().getOntologies()){
                    if (ont.getURI().hashCode() == hashCode){
                        return ont;
                    }
                }
            }
            else{
                Set<OWLNamedObject> objs = new HashSet<OWLNamedObject>();
                for (OWLOntology ont : server.getOWLOntologyManager().getOntologies()){
                    objs.addAll(type.getNamedObjectsFromOntology(ont));
                }
                for (OWLNamedObject obj : objs){
                    if (obj.getURI().hashCode() == hashCode){
                        return obj;
                    }
                }
            }
        }
        catch (MalformedURLException e) {
            // do nothing - there is no object specified
        }
        return null;
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

    private int getID(URL url) throws MalformedURLException {
        String relativeURL = URLUtils.createRelativeURL(server.getBaseURL(), url);
        String[] path = relativeURL.split("/");
        if (path.length >= 2){
            try{
                return Integer.parseInt(path[1]); // always the second element
            }
            catch (NumberFormatException e){
                throw new MalformedURLException("Cannot find a valid ID: " + path[1]);
            }
        }
        throw new MalformedURLException("The URL specified does not contain an object identifier: " + url);
    }
}
