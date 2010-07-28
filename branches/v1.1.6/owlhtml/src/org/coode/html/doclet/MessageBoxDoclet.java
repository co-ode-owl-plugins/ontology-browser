/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.doclet;

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
public class MessageBoxDoclet extends AbstractHTMLDoclet {

    public static final String ID = "doclet.Message";

    private final String title;
    private String message;

    private String cssClass = "codebox";

    public MessageBoxDoclet(String title, String message) {
        this.title = title;
        this.message = message;
    }

    public void renderHeader(URL pageURL, PrintWriter out) {
        renderBoxStart(out);
        // print message if there is one
        if (message != null && message.length() > 0){
            out.println("<pre>");
            out.println(message);
            out.println("</pre>");
        }
    }

    protected void renderFooter(URL pageURL, PrintWriter out) {
        renderBoxEnd(title, out);
    }

    public String getID() {
        return ID;
    }

    protected final void renderBoxStart(PrintWriter out) {
        out.println("<h2>" + title + "</h2>");
        out.println("<div class='" + cssClass + "' id='" + title.toLowerCase().replace(" ", "_") + "'>");
    }

    public void setClass(String cssClass) {
        this.cssClass = cssClass;
    }
}
