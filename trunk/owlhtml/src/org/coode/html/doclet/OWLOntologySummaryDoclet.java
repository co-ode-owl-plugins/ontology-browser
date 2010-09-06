package org.coode.html.doclet;

import org.coode.html.OWLHTMLKit;
import org.semanticweb.owlapi.model.OWLOntology;

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
public class OWLOntologySummaryDoclet extends AbstractOWLDocDoclet<OWLOntology> {

    public OWLOntologySummaryDoclet(OWLHTMLKit kit) {
        super(kit);
        addDoclet(new OntologyTitleDoclet(kit));
        addDoclet(new OntologyAnnotationsDoclet(kit));

        final OntologyContentsDoclet referencesDoclet = new OntologyContentsDoclet(kit);
        referencesDoclet.setTitle("References");
        addDoclet(referencesDoclet);

        addDoclet(new OntologyImportsDoclet(kit));

        // @@TODO reenable clouds (maybe use links instead)
//        if (kit.getHTMLProperties().isSet(OWLHTMLProperty.optionRenderOntologySummaryCloud)){
//            CloudDoclet<OWLClass> cloudDoclet = new CloudDoclet<OWLClass>(kit);
//            cloudDoclet.setComparator(kit.getOWLServer().getComparator());
//            cloudDoclet.setThreshold(8);
//            cloudDoclet.setZoom(10);
//            addDoclet(cloudDoclet);
//        }
    }


    public void setUserObject(OWLOntology object) {
        super.setUserObject(object);

//        if (getOWLHTMLKit().getHTMLProperties().isSet(OWLHTMLProperty.optionRenderOntologySummaryCloud)){
//            // only show the classes in this ontology
//            cloudModel.setOntologies(Collections.singleton(getUserObject()));
//        }
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
        return "doclet.summary.objectproperty";
    }
}
