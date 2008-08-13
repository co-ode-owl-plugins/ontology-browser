package org.coode.www.mngr;

import org.coode.owl.mngr.OWLServer;
import org.coode.suggestor.api.DLReasoner;
import org.coode.suggestor.impl.OWLDLReasoner;
import org.coode.suggestor.impl.SuggestorManagerImpl;
import org.semanticweb.owl.inference.OWLReasoner;
import org.semanticweb.owl.model.OWLOntologyChange;

import java.util.List;
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
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Aug 15, 2007<br><br>
 */
public class SuggestorManagerAdapter extends SuggestorManagerImpl {

    private OWLServer server;

    private DLReasoner reasoner;

    public SuggestorManagerAdapter(OWLServer server) {
        super(server.getOWLOntologyManager());
        this.server = server;
    }

    public DLReasoner getReasoner() {
        if (reasoner == null){
            reasoner = new OWLDLReasoner(server.getOWLOntologyManager()){
                protected OWLReasoner getOWLReasoner() {
                    return server.getOWLReasoner();
                }

                public String getId() {
                    return "FaCT++";
                }

                public void ontologiesChanged(List<? extends OWLOntologyChange> changes) {
                    //do nothing
                }
            };
        }
        return reasoner;
    }
}
