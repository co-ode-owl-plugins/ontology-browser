/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.doclet;

import org.coode.html.OWLHTMLKit;
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

    public TabsDoclet(OWLHTMLKit kit) {
        super(kit);
    }

    public void renderHeader(URL pageURL, PrintWriter out) {
        out.println("<div id='tabs'>");
        
        OWLHTMLKit kit = getHTMLGenerator();
        final boolean singleFrame = isSingleFrameNavigation();

        for (NamedObjectType type : NamedObjectType.values()){

        renderLink(type.getPluralRendering(),
                   kit.getURLScheme().getURLForIndex(type),
                   OWLHTMLConstants.LinkTarget.subnav,
                   "",
                   singleFrame,
                   pageURL,
                   out);
        out.print(" | ");
        }

        renderLink("Clouds",
                   kit.getURLScheme().getURLForRelativePage("cloud/"),
                   OWLHTMLConstants.LinkTarget.subnav,
                   "",
                   singleFrame,
                   pageURL,
                   out);
    }

    protected void renderFooter(URL pageURL, PrintWriter out) {
        out.println("</div> <!-- tabs -->");
    }

    public String getID() {
        return ID;
    }
}
