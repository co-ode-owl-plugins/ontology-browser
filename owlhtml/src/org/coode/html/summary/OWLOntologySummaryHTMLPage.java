package org.coode.html.summary;

import org.coode.html.OWLHTMLServer;
import org.coode.html.impl.OWLHTMLConstants;
import org.coode.html.cloud.ClassesByUsageCloud;
import org.coode.html.doclet.*;
import org.semanticweb.owl.model.OWLClass;
import org.semanticweb.owl.model.OWLOntology;

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
public class OWLOntologySummaryHTMLPage extends AbstractSummaryHTMLPage<OWLOntology> {

    private ClassesByUsageCloud cloudModel;
    private BookmarksDoclet bookmarksDoclet;

    public OWLOntologySummaryHTMLPage(OWLHTMLServer server) {
        super(server);

        addDoclet(new OntologyAnnotationsDoclet(server));

        final OntologyContentsDoclet referencesDoclet = new OntologyContentsDoclet(server);
        referencesDoclet.setTitle("References");
        addDoclet(referencesDoclet);

        addDoclet(new OntologyImportsDoclet(server));

        bookmarksDoclet = new BookmarksDoclet("Bookmarks", ElementsDoclet.Format.list, server);
        addDoclet(bookmarksDoclet);

        if (server.getProperties().isSet(OWLHTMLConstants.OPTION_RENDER_ONTOLOGY_SUMMARY_CLOUD)){
            cloudModel = new ClassesByUsageCloud(getServer());
            CloudDoclet<OWLClass> cloudDoclet = new CloudDoclet<OWLClass>(cloudModel, getServer());
            cloudDoclet.setComparator(getServer().getComparator());
            cloudDoclet.setThreshold(8);
            cloudDoclet.setZoom(10);
            addDoclet(cloudDoclet);
        }
    }


    public void setUserObject(OWLOntology object) {
        super.setUserObject(object);

        if (getServer().getProperties().isSet(OWLHTMLConstants.OPTION_RENDER_ONTOLOGY_SUMMARY_CLOUD)){
            // only show the classes in this ontology
            cloudModel.setOntologies(Collections.singleton(getUserObject()));
        }
    }
}
