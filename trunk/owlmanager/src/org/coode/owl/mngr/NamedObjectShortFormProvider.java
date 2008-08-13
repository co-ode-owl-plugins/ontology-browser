package org.coode.owl.mngr;

import org.semanticweb.owl.model.OWLNamedObject;
import org.semanticweb.owl.util.ShortFormProvider;

/**
 * Author: Nick Drummond<br>
 * nick.drummond@cs.manchester.ac.uk<br>
 * http://www.cs.man.ac.uk/~drummond<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jun 18, 2007<br><br>
 * <p/>
 * code made available under Mozilla Public License (http://www.mozilla.org/MPL/MPL-1.1.html)<br>
 * copyright 2006, The University of Manchester<br>
 *
 * implementation neutral renderer interface
 */
public interface NamedObjectShortFormProvider extends ShortFormProvider {
    
    String getShortForm(OWLNamedObject obj);
}
