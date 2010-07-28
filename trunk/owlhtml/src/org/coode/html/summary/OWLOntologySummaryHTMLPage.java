package org.coode.html.summary;

import org.coode.html.OWLHTMLKit;
import org.coode.html.cloud.ClassesByUsageCloud;
import org.coode.html.doclet.*;
import org.coode.html.impl.OWLHTMLProperty;
import org.coode.html.page.OWLDocPage;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLOntology;

import java.util.Collections;

/**
 * Author: Nick Drummond<br>
 * nick.drummond@cs.manchester.ac.uk<br>
 * http://www.cs.man.ac.uk/~drummond<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jun 7, 2007<br><br>
 * <p/>
 * code made available under Mozilla Public License (http://www.mozilla.org/MPL/MPL-1.1.html)<br>
 * copyright 2006, The University of Manchester<br>
 */
public class OWLOntologySummaryHTMLPage extends OWLDocPage<OWLOntology> {

    private ClassesByUsageCloud cloudModel;

    private HierarchyDoclet<OWLOntology> hierarchy = null;

    public OWLOntologySummaryHTMLPage(final OWLHTMLKit kit) {
        super(kit);

        addDoclet(new OntologyTitleDoclet(kit));
        addDoclet(new OntologyAnnotationsDoclet(kit));

        final OntologyContentsDoclet referencesDoclet = new OntologyContentsDoclet(kit);
        referencesDoclet.setTitle("References");
        addDoclet(referencesDoclet);

        addDoclet(new OntologyImportsDoclet(kit));

        if (kit.getHTMLProperties().isSet(OWLHTMLProperty.optionRenderOntologySummaryCloud)){
            cloudModel = new ClassesByUsageCloud(getHTMLGenerator());
            CloudDoclet<OWLClass> cloudDoclet = new CloudDoclet<OWLClass>(cloudModel, getHTMLGenerator());
            cloudDoclet.setComparator(getHTMLGenerator().getOWLServer().getComparator());
            cloudDoclet.setThreshold(8);
            cloudDoclet.setZoom(10);
            addDoclet(cloudDoclet);
        }
    }


    public void setUserObject(OWLOntology object) {
        super.setUserObject(object);

        if (getHTMLGenerator().getHTMLProperties().isSet(OWLHTMLProperty.optionRenderOntologySummaryCloud)){
            // only show the classes in this ontology
            cloudModel.setOntologies(Collections.singleton(getUserObject()));
        }
    }

    public void setNavigationRenderer(HierarchyDoclet<OWLOntology> doclet) {
        if (doclet == null && this.hierarchy != null){
            removeDoclet(this.hierarchy);
        }
        else{
            addDoclet(doclet, indexOf(getDoclet(OntologyTitleDoclet.ID)));
        }
        this.hierarchy = doclet;
    }
}
