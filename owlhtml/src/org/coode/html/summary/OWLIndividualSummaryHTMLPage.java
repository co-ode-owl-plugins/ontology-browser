package org.coode.html.summary;

import org.coode.html.OWLHTMLKit;
import org.coode.html.doclet.*;
import org.semanticweb.owlapi.model.OWLNamedIndividual;

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
public class OWLIndividualSummaryHTMLPage extends AbstractOWLEntitySummaryHTMLPage<OWLNamedIndividual> {

    public OWLIndividualSummaryHTMLPage(OWLHTMLKit kit) {
        super(kit);
        
        addDoclet(new AnnotationsDoclet<OWLNamedIndividual>(kit));
        addDoclet(new TypesDoclet(kit));
        addDoclet(new SameAsDoclet(kit));
        addDoclet(new DifferentFromDoclet(kit));
        addDoclet(new UsageDoclet<OWLNamedIndividual>(kit));
    }
}
