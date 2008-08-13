package org.coode.html.summary;

import org.coode.html.OWLHTMLServer;
import org.coode.html.doclet.*;
import org.semanticweb.owl.model.OWLObjectProperty;

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
public class OWLObjectPropertySummaryHTMLPage extends AbstractSummaryHTMLPage<OWLObjectProperty> {

    public OWLObjectPropertySummaryHTMLPage(OWLHTMLServer server) {
        super(server);

        addDoclet(new AnnotationsDoclet<OWLObjectProperty>(server));
        addDoclet(new PropertyCharacteristicsDoclet<OWLObjectProperty>(server));
        addDoclet(new DomainsDoclet<OWLObjectProperty>(server));
        addDoclet(new RangesDoclet<OWLObjectProperty>(server));
        addDoclet(new InversesDoclet(server));
        addDoclet(new AssertedSuperpropertiesDoclet<OWLObjectProperty>(server));
        addDoclet(new AssertedEquivpropertiesDoclet<OWLObjectProperty>(server));
        addDoclet(new DisjointPropertiesDoclet<OWLObjectProperty>(server));
        addDoclet(new UsageDoclet<OWLObjectProperty>(server));
    }
}
