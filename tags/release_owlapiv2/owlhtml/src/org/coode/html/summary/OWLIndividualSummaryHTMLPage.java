package org.coode.html.summary;

import org.coode.html.OWLHTMLServer;
import org.coode.html.doclet.*;
import org.semanticweb.owl.model.OWLIndividual;

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
public class OWLIndividualSummaryHTMLPage extends AbstractSummaryHTMLPage<OWLIndividual> {

    public OWLIndividualSummaryHTMLPage(OWLHTMLServer server) {
        super(server);
        
        addDoclet(new AnnotationsDoclet<OWLIndividual>(server));
        addDoclet(new TypesDoclet(server));
        addDoclet(new SameAsDoclet(server));
        addDoclet(new DifferentFromDoclet(server));
        addDoclet(new UsageDoclet<OWLIndividual>(server));
    }
}
