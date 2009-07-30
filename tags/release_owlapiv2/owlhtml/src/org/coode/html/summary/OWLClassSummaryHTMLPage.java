package org.coode.html.summary;

import org.coode.html.OWLHTMLServer;
import org.coode.html.doclet.*;
import org.semanticweb.owl.model.OWLClass;

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
public class OWLClassSummaryHTMLPage extends AbstractSummaryHTMLPage<OWLClass> {

    public OWLClassSummaryHTMLPage(OWLHTMLServer server) {
        super(server);
        
        addDoclet(new AnnotationsDoclet<OWLClass>(server));
        addDoclet(new AssertedEquivalentsDoclet(server));
        addDoclet(new AssertedSuperclassesDoclet(server));
        addDoclet(new DisjointsDoclet(server));
        addDoclet(new MembersDoclet(server));
        addDoclet(new UsageDoclet<OWLClass>(server));
    }
}