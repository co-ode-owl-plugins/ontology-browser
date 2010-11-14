/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.doclet;

import org.coode.html.OWLHTMLKit;
import org.coode.html.impl.OWLHTMLConstants;
import org.coode.html.impl.OWLHTMLParam;
import org.coode.html.impl.OWLHTMLProperty;
import org.coode.html.url.PermalinkURLScheme;
import org.coode.owl.mngr.NamedObjectType;
import org.semanticweb.owlapi.model.OWLObject;

import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.net.URL;
import java.net.URLEncoder;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Feb 7, 2008<br><br>
 */
public abstract class AbstractTitleDoclet<O extends OWLObject> extends AbstractOWLDocDoclet<O> {

    public static final String ID = "doclet.summary.title";

    public AbstractTitleDoclet(OWLHTMLKit kit) {
        super(kit);
    }


    protected void renderHeader(URL pageURL, PrintWriter out) {

        final OWLHTMLKit kit = getOWLHTMLKit();

        out.print("<h2>");
        out.print(getTitle());
        out.println("</h2>");

        final boolean permalink = kit.getHTMLProperties().isSet(OWLHTMLProperty.optionRenderPermalink);
        if (permalink){
            renderLink(OWLHTMLConstants.PERMALINK_LABEL,
                       new PermalinkURLScheme(kit.getURLScheme()).getURLForAbsolutePage(pageURL),
                       null,
                       "permalink",
                       isSingleFrameNavigation(),
                       pageURL,
                       out);
        }

        String subtitle = getSubtitle();
        if (subtitle != null){
            out.print("<h3>");
            out.print(subtitle);
            try {
                URL url = new URL(subtitle);
                URL loadURL = new URL(kit.getURLScheme().getURLForIndex(NamedObjectType.ontologies),
                                      "?" + OWLHTMLParam.action + "=load&" +
                                      OWLHTMLParam.uri + "=" +
                                      URLEncoder.encode(url.toString(), OWLHTMLConstants.DEFAULT_ENCODING) +
                                      "&redirect=" +
                                      URLEncoder.encode(pageURL.toString(), OWLHTMLConstants.DEFAULT_ENCODING));
                out.println(" ");
                renderImageLink(kit.getURLScheme().getURLForRelativePage(OWLHTMLConstants.IMAGES_BASE + "external.png"), "Attempt to open link in another window", url, OWLHTMLConstants.LinkTarget._blank, "urlOption", true, pageURL, out);

                // if the ontology at this location has not already been loaded
                if (!kit.getOWLServer().getLocationsMap().containsValue(url.toURI())){
                    out.println(" ");
                    renderImageLink(kit.getURLScheme().getURLForRelativePage(OWLHTMLConstants.IMAGES_BASE + "download.png"), "Attempt to load owl/rdf", loadURL, null, "urlOption", true, pageURL, out);
                }
            }
            catch (MalformedURLException e) {
                // do nothing - no load URL for this entity
            }
            catch (UnsupportedEncodingException e) {
                throw new RuntimeException(e);
            }
            catch (URISyntaxException e) {
                throw new RuntimeException(e);
            }
            out.println("</h3>");
        }
    }

    public abstract String getTitle();


    public abstract String getSubtitle();


    protected void renderFooter(URL pageURL, PrintWriter out) {
        // do nothing
    }

    public String getID() {
        return ID;
    }
}