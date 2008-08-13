package org.coode.html.summary;

import org.coode.html.OWLHTMLServer;
import org.coode.html.doclet.*;
import org.semanticweb.owl.model.OWLDataProperty;

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
public class OWLDataPropertySummaryHTMLPage extends AbstractSummaryHTMLPage<OWLDataProperty> {

    public OWLDataPropertySummaryHTMLPage(OWLHTMLServer server) {
        super(server);
        
        addDoclet(new AnnotationsDoclet<OWLDataProperty>(server));
        addDoclet(new PropertyCharacteristicsDoclet<OWLDataProperty>(server));
        addDoclet(new DomainsDoclet<OWLDataProperty>(server));
        addDoclet(new RangesDoclet<OWLDataProperty>(server));
        addDoclet(new AssertedSuperpropertiesDoclet<OWLDataProperty>(server));
        addDoclet(new AssertedEquivpropertiesDoclet<OWLDataProperty>(server));
        addDoclet(new DisjointPropertiesDoclet<OWLDataProperty>(server));
        addDoclet(new UsageDoclet<OWLDataProperty>(server));
    }
}
