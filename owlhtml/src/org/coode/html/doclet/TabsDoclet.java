/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.doclet;

import org.coode.html.OWLHTMLServer;
import org.coode.html.impl.OWLHTMLConstants;
import org.coode.owl.mngr.NamedObjectType;

import java.io.PrintWriter;
import java.net.URL;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jan 24, 2008<br><br>
 */
public class TabsDoclet extends AbstractOWLDocDoclet {

    public static final String ID = "doclet.Tabs";

    public TabsDoclet(OWLHTMLServer server) {
        super(server);
    }

    public void renderHeader(URL pageURL, PrintWriter out) {
        out.println("<div id='tabs'>");
        
        OWLHTMLServer server = getServer();
        final boolean singleFrame = isSingleFrameNavigation();

        renderLink("All " + NamedObjectType.ontologies.getPluralRendering(),
                   server.getURLScheme().getURLForIndex(NamedObjectType.ontologies),
                   OWLHTMLConstants.LinkTarget.subnav, "", singleFrame, pageURL, out);
        out.print(" | ");

        renderLink("All " + NamedObjectType.classes.getPluralRendering(),
                   server.getURLScheme().getURLForIndex(NamedObjectType.classes),
                   OWLHTMLConstants.LinkTarget.subnav, "", singleFrame, pageURL, out);
        out.print(" | ");

        renderLink("All " + NamedObjectType.objectproperties.getPluralRendering(),
                   server.getURLScheme().getURLForIndex(NamedObjectType.objectproperties),
                   OWLHTMLConstants.LinkTarget.subnav, "", singleFrame, pageURL, out);
        out.print(" | ");

        renderLink("All " + NamedObjectType.dataproperties.getPluralRendering(),
                   server.getURLScheme().getURLForIndex(NamedObjectType.dataproperties),
                   OWLHTMLConstants.LinkTarget.subnav, "", singleFrame, pageURL, out);
        out.print(" | ");

        renderLink("All " + NamedObjectType.individuals.getPluralRendering(),
                   server.getURLScheme().getURLForIndex(NamedObjectType.individuals),
                   OWLHTMLConstants.LinkTarget.subnav, "", singleFrame, pageURL, out);
    }

    protected void renderFooter(URL pageURL, PrintWriter out) {
        out.println("</div> <!-- tabs -->");
    }

    public String getID() {
        return ID;
    }
}
