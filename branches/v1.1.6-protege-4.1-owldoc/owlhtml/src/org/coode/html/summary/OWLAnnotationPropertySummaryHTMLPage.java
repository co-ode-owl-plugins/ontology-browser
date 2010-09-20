package org.coode.html.summary;

import org.coode.html.OWLHTMLKit;
import org.coode.html.doclet.*;
import org.semanticweb.owlapi.model.*;

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
public class OWLAnnotationPropertySummaryHTMLPage extends AbstractOWLEntitySummaryHTMLPage<OWLAnnotationProperty> {

    public OWLAnnotationPropertySummaryHTMLPage(OWLHTMLKit kit) {
        super(kit);

        addDoclet(new AnnotationsDoclet<OWLAnnotationProperty>(kit));
        addDoclet(new AnnotationPropertyDomainsDoclet(kit));
        addDoclet(new AnnotationPropertyRangesDoclet(kit));
        addDoclet(new AnnotationPropertySuperPropertiesDoclet(kit));
        addDoclet(new UsageDoclet<OWLAnnotationProperty>(kit));
    }
}