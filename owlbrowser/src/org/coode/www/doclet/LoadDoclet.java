/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.www.doclet;

import org.coode.html.doclet.AbstractHTMLDoclet;
import org.coode.www.Bookmarks;

import java.io.PrintWriter;
import java.net.URL;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jan 25, 2008<br><br>
 */
public class LoadDoclet extends AbstractHTMLDoclet {

    private static final String ID = "doclet.load";

    public LoadDoclet() {
        LoadBookmarksDoclet bookmarks = new LoadBookmarksDoclet();
        bookmarks.setLabel("or Select a bookmark from below:");
        bookmarks.addAll(Bookmarks.getBookmarks());
        addDoclet(bookmarks);

        addDoclet(new LoadFormDoclet());
    }

    protected void renderHeader(URL pageURL, PrintWriter out) {
        renderBoxStart("Load Ontologies", out);
    }

    protected void renderFooter(URL pageURL, PrintWriter out) {
        out.println("<br style='clear: both;' />");
        renderBoxEnd("Load Ontologies", out);
    }

    public String getID() {
        return ID;
    }
}
