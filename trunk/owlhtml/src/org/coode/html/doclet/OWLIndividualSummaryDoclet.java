package org.coode.html.doclet;

import org.coode.html.OWLHTMLKit;
import org.semanticweb.owlapi.model.OWLNamedIndividual;

import java.io.PrintWriter;
import java.net.URL;

/**
 * Author: drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Aug 4, 2010<br><br>
 */
public class OWLIndividualSummaryDoclet extends AbstractOWLDocDoclet<OWLNamedIndividual> {

    public OWLIndividualSummaryDoclet(OWLHTMLKit kit) {
        super(kit);

        addDoclet(new OWLEntityTitleDoclet<OWLNamedIndividual>(kit));
        addDoclet(new AnnotationsDoclet<OWLNamedIndividual>(kit));
        addDoclet(new TypesDoclet(kit));
        addDoclet(new SameAsDoclet(kit));
        addDoclet(new DifferentFromDoclet(kit));
        addDoclet(new UsageDoclet<OWLNamedIndividual>(kit));
    }

    @Override
    protected void renderHeader(URL pageURL, PrintWriter out) {
        out.write("<div class='summary'>");
    }

    @Override
    protected void renderFooter(URL pageURL, PrintWriter out) {
        out.write("</div> <!-- summary -->");
    }

    public String getID() {
        return "doclet.summary.individual";
    }
}
