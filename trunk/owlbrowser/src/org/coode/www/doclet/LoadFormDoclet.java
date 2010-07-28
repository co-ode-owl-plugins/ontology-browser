/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.www.doclet;

import org.coode.html.doclet.AbstractHTMLDoclet;
import org.coode.html.impl.OWLHTMLParam;
import org.coode.www.OntologyBrowserConstants;

import java.io.PrintWriter;
import java.net.URI;
import java.net.URL;
import java.util.Map;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jan 25, 2008<br><br>
 */
public class LoadFormDoclet extends AbstractHTMLDoclet {

    private static final String ID = "doclet.load";

    protected void renderHeader(URL pageURL, PrintWriter out) {
        renderBoxStart("Load Ontologies", out);

        out.println("    <form style='float: left;  width: 50%;' id='specify' method='POST' action='.' target='_top' >");
        out.println("        <label for='uri-spec'><h3 style='margin-bottom: 0;'>Specify the physical location of your ontology:</h3></label><br />");
        out.print("        <input id='");
        out.print(OntologyBrowserConstants.LOAD_ONTOLOGIES_INPUT_ID);
        out.print("' name='");
        out.print(OWLHTMLParam.uri);
        out.println("' type='text' style='width:80%; margin-top: 0;' />");
        out.println("        <input name='action' type='submit' value='load' />");
        out.println("    </form>");
        out.println("    <!--form method='post' enctype='multipart/form-data' action='.'>");
        out.println("      <p class='instructions'>Upload an ontology:</p>");
        out.println("      <p><label title='Choose a Local File to Upload and Validate' for='uploaded_file'>File:</label>");
        out.println("        <input type='file' id='uploaded_file' name='uploaded_file' size='30' /></p>");
        out.println("        <input name='action' type='submit' value='load' />");
        out.println("    </form-->");
    }

    protected void renderFooter(URL pageURL, PrintWriter out) {
        renderBoxEnd("Load Ontologies", out);
    }

    public void addBookmarkSet(String name, Map<String, URI> bookmarkMap){
        LoadBookmarksDoclet bookmarks = new LoadBookmarksDoclet();
        bookmarks.setLabel(name);
        bookmarks.addAll(bookmarkMap);
        addDoclet(bookmarks);
    }

    public String getID() {
        return ID;
    }
}
