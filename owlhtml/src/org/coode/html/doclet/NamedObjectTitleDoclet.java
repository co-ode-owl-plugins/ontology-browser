/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.doclet;

import org.coode.html.OWLHTMLServer;
import org.coode.owl.mngr.NamedObjectType;
import org.semanticweb.owl.model.OWLNamedObject;

import java.io.PrintWriter;
import java.net.URL;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Feb 7, 2008<br><br>
 */
public class NamedObjectTitleDoclet<O extends OWLNamedObject> extends AbstractOWLDocDoclet<O> {

    public static final String ID = "doclet.summary.title";

    public NamedObjectTitleDoclet(OWLHTMLServer server) {
        super(server);
    }

    protected void renderHeader(URL pageURL, PrintWriter out) {
        final O object = getUserObject();
        out.println("<h1>" + getTitle() + "</h1>");
        out.println("<h2 class='summaryURI'>" + object.getURI() + "</h2>");
    }

    public String getTitle() {
        final O object = getUserObject();
        return NamedObjectType.getType(object).getSingularRendering() + ": " + getServer().getNameRenderer().getShortForm(object);
    }

    protected void renderFooter(URL pageURL, PrintWriter out) {
        // do nothing
    }

    public String getID() {
        return ID;
    }
}
