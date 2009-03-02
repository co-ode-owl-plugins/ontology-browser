package org.coode.html.url;

import org.apache.log4j.Logger;
import org.coode.html.OWLHTMLServer;
import org.coode.html.impl.OWLHTMLConstants;
import org.coode.owl.mngr.NamedObjectShortFormProvider;
import org.coode.owl.mngr.NamedObjectType;
import org.coode.owl.mngr.impl.FragmentShortFormProvider;
import org.semanticweb.owl.model.OWLNamedObject;
import org.semanticweb.owl.model.OWLOntology;

import java.net.*;
import java.io.UnsupportedEncodingException;
import java.util.Set;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;

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
 * URL scheme for static HTML pages.
 *
 * URLs are of the form:
 * serverbase/<type>/<fragment>.html
 *
 *
 */
public class StaticFilesURLScheme extends AbstractURLScheme {

    private static final Logger logger = Logger.getLogger(StaticFilesURLScheme.class.getName());

    private NamedObjectShortFormProvider shortFormProvider;

    private static final String ID_SPLITTER = "___";

    private Map<URL, OWLNamedObject> url2ObjMap = new HashMap<URL, OWLNamedObject>();
    private Map<OWLNamedObject, URL> obj2UrlMap = new HashMap<OWLNamedObject, URL>();


    public StaticFilesURLScheme(OWLHTMLServer server) {
        super(server);
        this.shortFormProvider = new FragmentShortFormProvider(); // always use the fragment as it will be safe in URLs
    }

    public URL getURLForNamedObject(OWLNamedObject object) {

        URL url = obj2UrlMap.get(object);
        if (url == null){
        // always use the fragment instead of the rendering
        String name = shortFormProvider.getShortForm(object) + ID_SPLITTER + object.getURI().hashCode();

        try {
            name = URLEncoder.encode(name, OWLHTMLConstants.DEFAULT_ENCODING);
            url = new URL(getBaseURL(), NamedObjectType.getType(object) + "/" + name + OWLHTMLConstants.DEFAULT_EXTENSION);
            obj2UrlMap.put(object, url);
            url2ObjMap.put(url, object);
        }
        catch (MalformedURLException e) {
            logger.error(e);
        }
        catch (UnsupportedEncodingException e) {
            logger.error(e);
        }
        }
        return url;
    }

    public OWLNamedObject getNamedObjectForURL(URL url) {
        return url2ObjMap.get(url);
    }

    public URL getURLForOntologyIndex(OWLOntology ont, NamedObjectType type) {
        try {
            return new URL(getBaseURL(), type + "/" + getFilenameForOntologyIndex(ont, type));
        }
        catch (MalformedURLException e) {
            logger.error(e);
        }
        return null;
    }


    /**
     * Overloaded to provide the index.html file (which is necessary if the pages are not being hosted on a server)
     * @param type
     * @return
     */
    public URL getURLForIndex(NamedObjectType type) {
        try {
            return new URL(super.getURLForIndex(type) + OWLHTMLConstants.INDEX_HTML);
        }
        catch (MalformedURLException e) {
            logger.error("Cannot create URL for index: " + type, e);
        }
        return null;
    }


    public String getFilenameForOntologyIndex(OWLOntology ont, NamedObjectType type) {
        return "index-" + shortFormProvider.getShortForm(ont) + OWLHTMLConstants.DEFAULT_EXTENSION;
    }

    public void setAdditionalLinkArguments(String s) {
        // do nothing
    }

    public void clearAdditionalLinkArguments() {
        // do nothing
    }
}
