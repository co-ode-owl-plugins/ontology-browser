/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.www.page;

import org.coode.html.OWLHTMLKit;
import org.coode.html.doclet.HTMLDoclet;
import org.coode.html.impl.OWLHTMLConstants;
import org.coode.html.impl.OWLHTMLProperty;
import org.coode.html.util.URLUtils;

import java.io.PrintWriter;
import java.net.URL;
import java.util.Collections;
import java.util.Set;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jan 24, 2008<br><br>
 */
public class FramesHTMLPage implements HTMLDoclet {

    public static final String ID = "doclet.frames";

    private final URL defaultcontent;
    private final OWLHTMLKit kit;


    public FramesHTMLPage(URL defaultcontent, OWLHTMLKit kit) {
        this.defaultcontent = defaultcontent;
        this.kit = kit;
    }

    public String getID() {
        return ID;
    }

    public void renderContent(URL pageURL, PrintWriter out) {
        out.println("<frameset rows='50,90%'>\n");

        out.print("<frame src='" + OWLHTMLConstants.HEADER_HTML +
                  "' name='" + OWLHTMLConstants.LinkTarget.header + "' title='Header'/>\n");

        out.print("  <frameset cols='40%,60%'>\n" +
                  "     <frameset rows='30%,70%'>\n");

        out.print("<frame src='" + OWLHTMLConstants.CONTENTS_HTML +
                  "' name='" + OWLHTMLConstants.LinkTarget.nav + "' title='Contents'/>\n");

        out.print("<frame src='" + kit.getHTMLProperties().get(OWLHTMLProperty.optionIndexAllURL) +
                  "' name='" + OWLHTMLConstants.LinkTarget.subnav + "' title='All Entities'/>\n");

        out.print("     </frameset>\n");

        out.print("<frame src='" + URLUtils.createRelativeURL(pageURL, defaultcontent) +
                  "' name='" + OWLHTMLConstants.LinkTarget.content + "' title='Content'/>\n");

        out.print("     </frameset>\n");
        out.print("    <noframes>" + URLUtils.createRelativeURL(pageURL, defaultcontent) + "</noframes>\n" +
                  "</frameset>\n");
    }

    public void renderAll(URL pageURL, PrintWriter out) {
        out.print("<html>\n<head>\n<title>OWLDoc</title>\n</head>\n");
        renderContent(pageURL, out);
        out.print("</html>");
    }

    public void setUserObject(Object object) {
        // ignore, no user object
    }

    public Object getUserObject() {
        return null;  // ignore, no user object
    }

    public boolean isPinned() {
        return true;
    }

    public Set getRequiredCSS() {
        return Collections.emptySet();
    }

    public Set getRequiredJS() {
        return Collections.emptySet();
    }
}
