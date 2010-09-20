package org.coode.html.summary;

import org.coode.html.OWLHTMLKit;
import org.coode.html.doclet.*;
import org.semanticweb.owlapi.model.OWLObjectProperty;

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
public class OWLObjectPropertySummaryHTMLPage extends AbstractOWLEntitySummaryHTMLPage<OWLObjectProperty> {

    public OWLObjectPropertySummaryHTMLPage(OWLHTMLKit kit) {
        super(kit);

        addDoclet(new AnnotationsDoclet<OWLObjectProperty>(kit));
        addDoclet(new PropertyCharacteristicsDoclet<OWLObjectProperty>(kit));
        addDoclet(new DomainsDoclet<OWLObjectProperty>(kit));
        addDoclet(new RangesDoclet<OWLObjectProperty>(kit));
        addDoclet(new InversesDoclet(kit));
        addDoclet(new AssertedSuperpropertiesDoclet<OWLObjectProperty>(kit));
        addDoclet(new AssertedEquivpropertiesDoclet<OWLObjectProperty>(kit));
        addDoclet(new DisjointPropertiesDoclet<OWLObjectProperty>(kit));
        addDoclet(new UsageDoclet<OWLObjectProperty>(kit));
    }
}
