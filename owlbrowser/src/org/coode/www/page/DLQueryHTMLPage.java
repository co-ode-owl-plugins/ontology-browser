/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.www.page;

import org.coode.html.OWLHTMLKit;
import org.coode.html.doclet.AbstractHTMLDoclet;
import org.coode.html.doclet.MessageBoxDoclet;
import org.coode.html.impl.OWLHTMLConstants;
import org.coode.html.page.OWLDocPage;
import org.coode.www.OntologyBrowserConstants;
import org.coode.www.doclet.AutocompleteDoclet;

import java.io.PrintWriter;
import java.net.URL;
import java.util.Set;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jan 24, 2008<br><br>
 */
public class DLQueryHTMLPage extends OWLDocPage {

    private static final String DL_QUERY_AC_ID = "dlQuery";

    private String query;

    private AutocompleteDoclet acDoclet;

    public DLQueryHTMLPage(OWLHTMLKit kit) {
        super(kit);

        setTitle(OntologyBrowserConstants.DL_QUERY_LABEL);
        
        acDoclet = new AutocompleteDoclet(kit, DL_QUERY_AC_ID, false);
        acDoclet.setSubmitURL(kit.getURLScheme().getURLForRelativePage(OWLHTMLConstants.DL_QUERY_HTML));
        acDoclet.setSubmitName("query");
        acDoclet.setMultiword(true);
        acDoclet.setWidth("400px");

        MessageBoxDoclet queryBoxDoclet = new MessageBoxDoclet(OntologyBrowserConstants.DL_QUERY_LABEL, null);
        queryBoxDoclet.addDoclet(acDoclet);

        addDoclet(queryBoxDoclet);
        addDoclet(new AbstractHTMLDoclet(){

            protected void renderHeader(URL pageURL, PrintWriter out) {
                out.println("<div id='resultsForm'>");
            }

            protected void renderFooter(URL pageURL, PrintWriter out) {
                out.println("</div>");
            }

            public String getID() {
                return "doclet.results";
            }
        });

        setAutoFocusedComponent(DL_QUERY_AC_ID);
    }

    public void setQuery(String query){
        this.query = query;

        // below is nasty because we have to know ID dlQuerySyntax (which is hardcoded in ACRenderer
        final URL servletURL = getHTMLGenerator().getURLScheme().getBaseURL();

        String jsAction = "sendQuery(getValueOfElementByID(\"" + DL_QUERY_AC_ID +
                "\"), getValueOfElementByID(\"dlQuerySyntax\"), \"" + servletURL + "\")";
        if (query != null && query.length() > 0){
            addOnLoad(jsAction);
        }
        acDoclet.setInitialValue(query);
//        acDoclet.setJsAction(jsAction);
    }

    public Set<URL> getRequiredJS() {
        Set<URL> js = super.getRequiredJS();
        js.add(getHTMLGenerator().getURLScheme().getURLForRelativePage(OWLHTMLConstants.JS_DL_QUERY));
        return js;
    }
}
