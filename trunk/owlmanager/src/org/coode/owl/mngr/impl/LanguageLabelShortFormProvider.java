package org.coode.owl.mngr.impl;

import org.coode.owl.mngr.NamedObjectShortFormProvider;
import org.coode.owl.mngr.OWLServer;
import org.semanticweb.owl.model.*;
import org.semanticweb.owl.util.AnnotationValueShortFormProvider;

import java.net.URI;
import java.util.Collections;
/*
* Copyright (C) 2007, University of Manchester
*
* Modifications to the initial code base are copyright of their
* respective authors, or their employers as appropriate.  Authorship
* of the modifications may be determined from the ChangeLog placed at
* the end of this file.
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.

* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.

* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*/

/**
 * Author: drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Feb 18, 2009<br><br>
 */
public class LanguageLabelShortFormProvider extends FragmentShortFormProvider {

    private AnnotationValueShortFormProvider sfp;

    public LanguageLabelShortFormProvider(OWLServer server, URI annotationURI, String lang) {
        sfp = new AnnotationValueShortFormProvider(Collections.singletonList(annotationURI),
                                                   Collections.singletonMap(annotationURI, Collections.singletonList(lang)),
                                                   server.getOWLOntologyManager());
    }


    public String getShortForm(OWLEntity entity) {
        String label = sfp.getShortForm(entity);
        if (label != null){
            return label;
        }
        return super.getShortForm(entity);
    }

    public String getShortForm(OWLNamedObject obj) {
        if (obj instanceof OWLOntology){
            return super.getShortForm(obj);
        }
        else{
            return getShortForm((OWLEntity)obj);
        }
    }
}
