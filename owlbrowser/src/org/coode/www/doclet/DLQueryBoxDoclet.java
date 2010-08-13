package org.coode.www.doclet;

import org.coode.html.OWLHTMLKit;
import org.coode.html.doclet.AbstractTitleDoclet;
import org.coode.html.doclet.MessageBoxDoclet;
import org.coode.html.doclet.OWLSelectorDoclet;
import org.coode.html.impl.OWLHTMLConstants;
import org.coode.www.OntologyBrowserConstants;

/**
 * Author: drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Aug 13, 2010<br><br>
 */
public class DLQueryBoxDoclet extends OWLSelectorDoclet{

    private static final String DL_QUERY_AC_ID = "dlQuery";

    private AutocompleteDoclet acDoclet;

    public DLQueryBoxDoclet(OWLHTMLKit kit) {
        super(kit);

        acDoclet = new AutocompleteDoclet(kit, DL_QUERY_AC_ID, false);
        acDoclet.setSubmitURL(kit.getURLScheme().getURLForRelativePage(OWLHTMLConstants.DL_QUERY_HTML));
        acDoclet.setSubmitName("query");
        acDoclet.setMultiword(true);
        acDoclet.setWidth("300px");

        addDoclet(new AbstractTitleDoclet(kit){

            @Override
            public String getTitle() {
                return OntologyBrowserConstants.DL_QUERY_LABEL;
            }

            @Override
            public String getSubtitle() {
                return null;
            }
        });

        MessageBoxDoclet queryBoxDoclet = new MessageBoxDoclet(null, null);
        queryBoxDoclet.addDoclet(acDoclet);

        addDoclet(queryBoxDoclet);
    }

    public String getID() {
        return DL_QUERY_AC_ID;
    }

    public void setInitialValue(String query) {
        acDoclet.setInitialValue(query);
    }
}
