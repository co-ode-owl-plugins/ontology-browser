/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.www.exception;

import java.net.URL;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jan 11, 2008<br><br>
 */
public class RedirectException extends OntServerException {

    private URL page;

    public RedirectException(URL page) {
        this.page = page;
    }

    public URL getRedirectPage(){
        return page;
    }
}
