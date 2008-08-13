package org.coode.www.servlet;

import org.coode.html.OWLHTMLServer;
import org.coode.html.cloud.*;
import org.coode.html.doclet.CloudDoclet;
import org.coode.html.doclet.HTMLDoclet;
import org.coode.html.impl.OWLHTMLConstants;
import org.coode.html.page.EmptyOWLDocPage;
import org.coode.owl.mngr.OWLServer;
import org.coode.www.exception.OntServerException;
import org.semanticweb.owl.model.OWLOntology;

import java.io.PrintWriter;
import java.net.URI;
import java.net.URL;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
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
 * Date: Jul 27, 2007<br><br>
 */
public class Cloud extends AbstractOntologyServerServlet {

    private static final String PARAM_TYPE = "type";
    private static final String PARAM_ONTOLOGY = "ont";

    protected void handleXMLRequest(Map<String, String> params, OWLHTMLServer server, URL servletURL, PrintWriter out) throws OntServerException {
        // no implementation currently - could do an xml version though
    }

    protected HTMLDoclet handleHTMLRequest(Map<String, String> params, OWLHTMLServer server, URL pageURL) throws OntServerException {

        String ontURIStr = params.get(PARAM_ONTOLOGY);
        CloudType cType = CloudType.valueOf(params.get(PARAM_TYPE));

        Set<OWLOntology> ontologies = server.getVisibleOntologies();

        if (ontURIStr != null){
            URI ontURI = URI.create(ontURIStr);
            if (ontURI != null){
                OWLOntology ont = server.getOWLOntologyManager().getOntology(ontURI);
                if (ont != null){
                    ontologies = server.getOWLOntologyManager().getImportsClosure(ont);
                }
            }
        }

        OWLCloudModel cloudModel = getModelFromType(cType, server);
        cloudModel.setOntologies(ontologies);

        CloudDoclet cloudRenderer = new CloudDoclet(cloudModel, server);
        cloudRenderer.setTarget(OWLHTMLConstants.LinkTarget.content);

        cloudRenderer.setComparator(server.getComparator());
        cloudRenderer.setThreshold(8);
        cloudRenderer.setZoom(10);

        EmptyOWLDocPage page = new EmptyOWLDocPage(server);
        page.addDoclet(cloudRenderer);

        return page;
    }

    protected Map<String, Set<String>> getRequiredParams(OWLServer server) {
        Map<String, Set<String>> required = new HashMap<String, Set<String>>();
        required.put(PARAM_TYPE, getCloudTypeRenderings());
//        required.put(PARAM_ONTOLOGY, Collections.singleton("<ontology uri>")); optional
        return required;
    }

    private Set<String> getCloudTypeRenderings() {
        Set<String> cloudTypes = new HashSet<String>();
        for (CloudType cloudType : CloudType.values()){
            cloudTypes.add(cloudType.toString());
        }
        return cloudTypes;
    }

    private OWLCloudModel getModelFromType(CloudType type, OWLHTMLServer server) {
        switch(type){
            case classusage:
                return new ClassesByUsageCloud(server);
            case objpropusage:
                return new ObjectPropsByUsageCloud(server);
            case datapropusage:
                return new DataPropsByUsageCloud(server);
            case indusage:
                return new IndividualsByUsageCloud(server);
        }
        return null;
    }
}
