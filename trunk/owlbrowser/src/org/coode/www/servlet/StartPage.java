package org.coode.www.servlet;

import org.coode.html.OWLHTMLKit;
import org.coode.html.doclet.HTMLDoclet;
import org.coode.html.impl.OWLHTMLParam;
import org.coode.html.page.OWLDocPage;
import org.coode.www.OntologyBrowserConstants;
import org.coode.www.doclet.BlurbDoclet;
import org.coode.www.doclet.LoadDoclet;
import org.coode.www.exception.OntServerException;

import java.io.PrintWriter;
import java.net.URL;
import java.util.Map;

/**
 * Author: drummond<br>
 * The University Of Manchester<br>
 * Medical Informatics Group<br>
 * Date: Jul 3, 2006<br><br>
 * <p/>
 * nick.drummond@cs.manchester.ac.uk<br>
 * www.cs.man.ac.uk/~drummond<br><br>
 *
 * Cannot subclass AbstractOntologyServerServlet as that checks if an ontology has been loaded
 */
public class StartPage extends AbstractOntologyServerServlet {

    protected void handleXMLRequest(Map<OWLHTMLParam, String> params, OWLHTMLKit kit, URL servletURL, PrintWriter out) throws OntServerException {
        // no implementation
    }

    protected HTMLDoclet handleHTMLRequest(Map<OWLHTMLParam, String> params, OWLHTMLKit kit, URL pageURL) throws OntServerException {

        OWLDocPage doclet = new OWLDocPage(kit);

        doclet.addDoclet(new BlurbDoclet());
        doclet.addDoclet(new LoadDoclet(kit));

        doclet.setAutoFocusedComponent(OntologyBrowserConstants.LOAD_ONTOLOGIES_INPUT_ID);

        return doclet;
    }
}
