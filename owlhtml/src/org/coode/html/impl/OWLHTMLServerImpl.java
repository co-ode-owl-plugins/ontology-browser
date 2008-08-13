package org.coode.html.impl;

import org.coode.html.OWLHTMLServer;
import org.coode.html.url.StaticFilesURLScheme;
import org.coode.html.url.URLScheme;
import org.coode.owl.mngr.impl.OWLServerImpl;
import org.coode.owl.mngr.impl.ToldPropertyHierarchyReasoner;
import org.semanticweb.owl.model.OWLOntology;
import org.semanticweb.owl.model.OWLOntologyManager;
import org.semanticweb.owl.apibinding.OWLManager;

import java.net.URL;
import java.util.HashSet;
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
 * Date: Oct 2, 2007<br><br>
 */
public class OWLHTMLServerImpl extends OWLServerImpl implements OWLHTMLServer {

    private URL baseURL;

    protected URLScheme urlScheme;

    private String label;

    // @@TODO save in preferences
    private Set<OWLOntology> hiddenOntologies = new HashSet<OWLOntology>();


    public OWLHTMLServerImpl(String id, URL baseURL) {
        this(id, OWLManager.createOWLOntologyManager(), baseURL);
    }


    public OWLHTMLServerImpl(String id, OWLOntologyManager mngr, URL baseURL) {
        super(id, mngr);
        this.baseURL = baseURL;

        getProperties().set(OWLHTMLConstants.OPTION_CONTENT_WINDOW, OWLHTMLConstants.LinkTarget.content.toString());
        getProperties().set(OWLHTMLConstants.OPTION_INDEX_ALL_URL, OWLHTMLConstants.DEFAULT_INDEX_ALL_URL);
        getProperties().set(OWLHTMLConstants.OPTION_DEFAULT_CSS, OWLHTMLConstants.CSS_DEFAULT);
        getProperties().set(OWLHTMLConstants.OPTION_FRAMES, null);
    }

    
    public URL getBaseURL(){
        return baseURL;
    }

    public URLScheme getURLScheme() {
        if (urlScheme == null && !serverIsDead()){
            urlScheme = new StaticFilesURLScheme(this);
        }
        return urlScheme;
    }

    public void setURLScheme(URLScheme urlScheme) {
        this.urlScheme = urlScheme;
    }

    public Set<OWLOntology> getVisibleOntologies() {
        Set<OWLOntology> visOnts = new HashSet<OWLOntology>();
        for (OWLOntology ont : getOWLOntologyManager().getImportsClosure(getActiveOntology())){
            if (!hiddenOntologies.contains(ont)){
                visOnts.add(ont);
            }
        }
        return visOnts;
    }

    public void setOntologyVisible(OWLOntology ontology, boolean visible) {
        if (visible){
            hiddenOntologies.remove(ontology);
        }
        else{
            hiddenOntologies.add(ontology);
        }
    }

    public void setCurrentLabel(String label) {
        this.label = label;
    }

    public String getCurrentLabel() {
        return label;
    }


    public void dispose() {
        super.dispose();
        urlScheme = null;
        baseURL = null;
    }
}
