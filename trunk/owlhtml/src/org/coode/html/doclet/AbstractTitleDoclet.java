/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.doclet;

import org.coode.html.OWLHTMLKit;
import org.semanticweb.owlapi.model.OWLObject;

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
public abstract class AbstractTitleDoclet<O extends OWLObject> extends AbstractOWLDocDoclet<O> {

    public static final String ID = "doclet.summary.title";

    public AbstractTitleDoclet(OWLHTMLKit kit) {
        super(kit);
    }


    protected void renderHeader(URL pageURL, PrintWriter out) {
        out.println("<h1>" + getTitle() + "</h1>");
        String subtitle = getSubtitle();
        if (subtitle != null){
            out.println("<h2 class='summaryURI'>" + subtitle + "</h2>");
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