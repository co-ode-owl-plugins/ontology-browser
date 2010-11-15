package org.coode.www.doclet;

import org.coode.html.doclet.AbstractHTMLDoclet;
import org.coode.html.impl.OWLHTMLConstants;
import org.coode.html.util.HTMLUtils;
import org.coode.www.OntologyBrowserConstants;

import java.io.PrintWriter;
import java.net.URL;

/**
 * Author: drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jul 26, 2010<br><br>
 */
public class TitleDoclet extends AbstractHTMLDoclet {

    @Override
    protected void renderHeader(URL pageURL, PrintWriter out) {
        HTMLUtils.renderLink("Help", OWLHTMLConstants.HOME_PAGE, OWLHTMLConstants.LinkTarget._blank, "help", true, pageURL, out);

        out.print("<h1>");
        out.print(OntologyBrowserConstants.ONTOLOGY_SERVER_NAME);
        out.println("</h1>");
        out.print(" v");
        out.print(OntologyBrowserConstants.VERSION);
    }

    @Override
    protected void renderFooter(URL pageURL, PrintWriter out) {
        // do nothing
    }

    public String getID() {
        return "TITLE";
    }
}
