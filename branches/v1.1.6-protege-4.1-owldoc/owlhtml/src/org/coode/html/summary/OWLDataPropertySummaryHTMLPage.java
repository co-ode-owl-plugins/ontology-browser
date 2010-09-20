package org.coode.html.summary;

import org.coode.html.OWLHTMLKit;
import org.coode.html.doclet.*;
import org.semanticweb.owlapi.model.OWLDataProperty;

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
public class OWLDataPropertySummaryHTMLPage extends AbstractOWLEntitySummaryHTMLPage<OWLDataProperty> {

    public OWLDataPropertySummaryHTMLPage(OWLHTMLKit kit) {
        super(kit);
        
        addDoclet(new AnnotationsDoclet<OWLDataProperty>(kit));
        addDoclet(new PropertyCharacteristicsDoclet<OWLDataProperty>(kit));
        addDoclet(new DomainsDoclet<OWLDataProperty>(kit));
        addDoclet(new RangesDoclet<OWLDataProperty>(kit));
        addDoclet(new AssertedSuperpropertiesDoclet<OWLDataProperty>(kit));
        addDoclet(new AssertedEquivpropertiesDoclet<OWLDataProperty>(kit));
        addDoclet(new DisjointPropertiesDoclet<OWLDataProperty>(kit));
        addDoclet(new UsageDoclet<OWLDataProperty>(kit));
    }
}
