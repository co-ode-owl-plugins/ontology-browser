package org.coode.html.impl;

import org.coode.html.OWLHTMLKit;
import org.coode.html.url.StaticFilesURLScheme;
import org.coode.html.url.URLScheme;
import org.coode.owl.mngr.OWLServer;
import org.coode.owl.mngr.ServerPropertiesAdapter;
import org.coode.owl.mngr.impl.OWLServerImpl;
import org.coode.owl.mngr.impl.ServerPropertiesAdapterImpl;
import org.coode.owl.util.OWLObjectComparator;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLOntology;

import java.net.URL;
import java.util.*;
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
public class OWLHTMLKitImpl implements OWLHTMLKit {

    private URL baseURL;

    protected URLScheme urlScheme;

    private String label;

    private Set<OWLOntology> hiddenOntologies = new HashSet<OWLOntology>();

    private OWLServer owlServer;

    private ServerPropertiesAdapter<OWLHTMLProperty> properties;

    private String id;

    private Comparator<OWLObject> comparator;


    public OWLHTMLKitImpl(String id, URL baseURL) {
        this(id, new OWLServerImpl(OWLManager.createOWLOntologyManager()), baseURL);
    }


    public OWLHTMLKitImpl(String id, OWLServer server, URL baseURL) {
        this.id = id;
        this.owlServer = server;
        this.baseURL = baseURL;
        this.comparator = new OWLObjectComparator<OWLObject>(server);
    }


    public ServerPropertiesAdapter<OWLHTMLProperty> getHTMLProperties() {
        if (properties == null){
            // share the same base properties
            properties = new ServerPropertiesAdapterImpl<OWLHTMLProperty>((ServerPropertiesAdapterImpl)getOWLServer().getProperties());
            properties.addDeprecatedNames(OWLHTMLProperty.generateDeprecatedNamesMap());


            properties.set(OWLHTMLProperty.optionContentWindow, OWLHTMLConstants.LinkTarget.content.toString());
            properties.set(OWLHTMLProperty.optionIndexAllURL, OWLHTMLConstants.DEFAULT_INDEX_ALL_URL);
            properties.set(OWLHTMLProperty.optionDefaultCSS, OWLHTMLConstants.CSS_DEFAULT);
            properties.set(OWLHTMLProperty.optionUseFrames, null);
            properties.setBoolean(OWLHTMLProperty.optionRenderSubs, true);


            // Allowed values
            List<String> booleanValues = Arrays.asList(Boolean.TRUE.toString(), Boolean.FALSE.toString());
            properties.setAllowedValues(OWLHTMLProperty.optionRenderSubs, booleanValues);
            properties.setAllowedValues(OWLHTMLProperty.optionRenderPermalink, booleanValues);
            properties.setAllowedValues(OWLHTMLProperty.optionShowMiniHierarchies, booleanValues);
            properties.setAllowedValues(OWLHTMLProperty.optionShowInferredHierarchies, booleanValues);
        }
        return properties;
    }


    public String getID() {
        return id;
    }


    public OWLServer getOWLServer() {
        return owlServer;
    }


    public URL getBaseURL(){
        return baseURL;
    }

    public URLScheme getURLScheme() {
        if (urlScheme == null && !owlServer.isDead()){
            urlScheme = new StaticFilesURLScheme(this);
        }
        return urlScheme;
    }

    public void setURLScheme(URLScheme urlScheme) {
        this.urlScheme = urlScheme;
    }

    public Comparator<OWLObject> getOWLObjectComparator() {
        return comparator;
    }

    public Set<OWLOntology> getVisibleOntologies() {
        Set<OWLOntology> visOnts = new HashSet<OWLOntology>();
        final OWLOntology activeOnt = owlServer.getActiveOntology();
        if (activeOnt != null){
            for (OWLOntology ont : owlServer.getOWLOntologyManager().getImportsClosure(activeOnt)){
                if (!hiddenOntologies.contains(ont)){
                    visOnts.add(ont);
                }
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
        owlServer.dispose();
        properties = null;
        urlScheme = null;
        baseURL = null;
    }

    public boolean isActive() {
        return !owlServer.isDead() && !owlServer.getOntologies().isEmpty();
    }
}
