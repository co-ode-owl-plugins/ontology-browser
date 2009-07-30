/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.doclet;

import org.coode.html.OWLHTMLServer;
import org.coode.html.impl.OWLHTMLConstants;
import org.semanticweb.owl.model.OWLObject;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Feb 5, 2008<br><br>
 *
 * Handy version for getting access to the server
 */
public abstract class AbstractOWLDocDoclet<O extends OWLObject> extends AbstractHTMLDoclet<O>{

    private OWLHTMLServer server;

    public AbstractOWLDocDoclet(OWLHTMLServer server) {
        this.server = server;
    }

    protected final OWLHTMLServer getServer(){
        return server;
    }

    protected boolean isSingleFrameNavigation() {
        return getServer().getProperties().get(OWLHTMLConstants.OPTION_CONTENT_WINDOW) == null;
    }
}
