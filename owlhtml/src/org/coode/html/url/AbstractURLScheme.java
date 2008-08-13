/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.url;

import org.coode.html.OWLHTMLServer;
import org.coode.html.util.URLUtils;
import org.coode.owl.mngr.NamedObjectType;
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
 * Date: Jan 12, 2008<br><br>
 */
public abstract class AbstractURLScheme implements URLScheme {

    private static final Logger logger = Logger.getLogger(AbstractURLScheme.class);

    protected final OWLHTMLServer server;


    public AbstractURLScheme(OWLHTMLServer server) {
        this.server = server;
    }

    public NamedObjectType getType(URL url) {
        String relativeURL = URLUtils.createRelativeURL(server.getBaseURL(), url);
        String[] path = relativeURL.split("/");
        String typeName = path[0]; // always the first element
        try{
            return NamedObjectType.valueOf(typeName);
        }
        catch(IllegalArgumentException e){
            // do nothing
        }
        return null;
    }

    public URL getURLForIndex(NamedObjectType type) {
        try {
            return new URL(server.getBaseURL(), type.toString() + "/");
        }
        catch (MalformedURLException e) {
            logger.error("Could not create URL for index: " + type, e);
        }
        return null;
    }

    public URL getURLForRelativePage(String pageRelativeToBase) {
        try {
            return new URL(getBaseURL() + pageRelativeToBase);
        }
        catch (MalformedURLException e) {
            logger.error("Could not create URL for page: " + pageRelativeToBase, e);
        }
        return null;
    }


    public URL getURLForAbsolutePage(URL pageURL) {
        return pageURL;
    }

    public URL getBaseURL(){
        return server.getBaseURL();
    }
}
