package org.coode.www.exception;

import org.semanticweb.owlapi.inference.OWLReasonerException;

/**
 * Author: Nick Drummond<br>
 * nick.drummond@cs.manchester.ac.uk<br>
 * http://www.cs.man.ac.uk/~drummond<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jun 28, 2007<br><br>
 * <p/>
 * code made available under Mozilla Public License (http://www.mozilla.org/MPL/MPL-1.1.html)<br>
 * copyright 2006, The University of Manchester<br>
 */
public class MyReasonerException extends OWLReasonerException {
    public MyReasonerException(String message) {
        super(message);
    }

    public MyReasonerException(String message, Throwable cause) {
        super(message, cause);
    }

    public MyReasonerException(Throwable cause) {
        super(cause);
    }
}
