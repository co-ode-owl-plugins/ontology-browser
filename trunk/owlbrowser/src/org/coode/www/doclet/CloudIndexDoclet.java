/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.www.doclet;

import org.coode.html.OWLHTMLKit;
import org.coode.html.cloud.CloudType;
import org.coode.html.doclet.AbstractOWLDocDoclet;
import org.coode.html.impl.OWLHTMLConstants;

import java.io.PrintWriter;
import java.net.URL;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Feb 4, 2008<br><br>
 */
public class CloudIndexDoclet extends AbstractOWLDocDoclet {

    public static final String ID = "doclet.index.cloud";

    public CloudIndexDoclet(OWLHTMLKit kit) {
        super(kit);
    }

    protected void renderHeader(URL pageURL, PrintWriter out) {
        renderBoxStart("Clouds", out);
        for (CloudType cloud : CloudType.values()){
            out.print("<li>");
            renderLink(cloud.getRendering(), getOWLHTMLKit().getURLScheme().getURLForRelativePage("cloud/?type=" + cloud), OWLHTMLConstants.LinkTarget.subnav, "", isSingleFrameNavigation(), pageURL, out);
            out.print("</li>");
        }
    }

    protected void renderFooter(URL pageURL, PrintWriter out) {
        renderBoxEnd("Clouds", out);
    }

    public String getID() {
        return ID;
    }
}
