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


    public StaticFilesURLScheme(OWLHTMLServer server) {
        super(server);
        this.shortFormProvider = new FragmentShortFormProvider(); // always use the fragment as it will be safe in URLs
    }

    public URL getURLForNamedObject(OWLNamedObject object) {

        // always use the fragment instead of the rendering
        String name = shortFormProvider.getShortForm(object) + ID_SPLITTER + object.getURI().hashCode();

        try {
            name = URLEncoder.encode(name, "UTF-8");
            return new URL(getBaseURL(), NamedObjectType.getType(object) + "/" + name + OWLHTMLConstants.DEFAULT_EXTENSION);
        }
        catch (MalformedURLException e) {
            logger.error(e);
        }
        catch (UnsupportedEncodingException e) {
            logger.error(e);
        }
        return null;
    }

    public OWLNamedObject getNamedObjectForURL(URL url) {

        NamedObjectType type = getType(url);
        if (type != null){
            String[] path = url.getPath().split("/");
            String filename = path[path.length-1].substring(0, path[path.length-1].indexOf(OWLHTMLConstants.DEFAULT_EXTENSION));

            try {
                filename = URLDecoder.decode(filename, "UTF-8");
                String[] nameIdPair = filename.split(ID_SPLITTER);

                if (nameIdPair.length > 1){ // otherwise it might be an index
                    Set<? extends OWLNamedObject> objs = getServer().getFinder().getOWLNamedObjects(nameIdPair[0], type);
                    if (objs.size() > 0){
                        for (OWLNamedObject o : objs){
                            if (o.getURI().hashCode() == Integer.parseInt(nameIdPair[1])){
                                return o;
                            }
                        }
                    }
                }
            }
            catch (UnsupportedEncodingException e) {
                logger.error(e);
            }
        }
        return null;
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
