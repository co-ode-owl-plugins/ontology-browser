package org.coode.www.doclet;

import org.coode.html.doclet.AbstractHTMLDoclet;
import org.coode.html.impl.OWLHTMLConstants;
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
                renderLink("Help", OWLHTMLConstants.HOME_PAGE, OWLHTMLConstants.LinkTarget._blank, "help", true, pageURL, out);

                out.print("<h1>");
                out.print(OntologyBrowserConstants.ONTOLOGY_SERVER_NAME);
                out.print(" ");
                out.print(OntologyBrowserConstants.VERSION);
                out.println("</h1>");
            }

            @Override
            protected void renderFooter(URL pageURL, PrintWriter out) {
                // do nothing
            }

            public String getID() {
                return "TITLE";
            }

}
