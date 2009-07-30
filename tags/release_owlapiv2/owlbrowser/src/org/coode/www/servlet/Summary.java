package org.coode.www.servlet;

import org.coode.html.OWLHTMLServer;
import org.coode.html.doclet.HTMLDoclet;
import org.coode.html.doclet.HierarchyRootDoclet;
import org.coode.html.doclet.NamedObjectTitleDoclet;
import org.coode.html.doclet.TabsDoclet;
import org.coode.html.hierarchy.OWLClassHierarchyTreeFragment;
import org.coode.html.hierarchy.OWLPropertyHierarchyTreeFragment;
import org.coode.html.impl.OWLHTMLConstants;
import org.coode.html.index.OWLEntityIndexHTMLPage;
import org.coode.html.page.EmptyOWLDocPage;
import org.coode.html.summary.*;
import org.coode.owl.mngr.NamedObjectType;
import org.coode.owl.mngr.OWLNamedObjectFinder;
import org.coode.owl.mngr.OWLServer;
import org.coode.owl.mngr.ServerConstants;
import org.coode.www.exception.OntServerException;
import org.coode.www.exception.RedirectException;
import org.semanticweb.owl.inference.OWLClassReasoner;
import org.semanticweb.owl.model.*;
import org.semanticweb.owl.util.ToldClassHierarchyReasoner;

import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.net.URI;
import java.net.URL;
import java.net.URLEncoder;
import java.util.*;

/**
 * Author: Nick Drummond<br>
 * nick.drummond@cs.manchester.ac.uk<br>
 * http://www.cs.man.ac.uk/~drummond<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jun 6, 2007<br><br>
 * <p/>
 * code made available under Mozilla Public License (http://www.mozilla.org/MPL/MPL-1.1.html)<br>
 * copyright 2006, The University of Manchester<br>
 *
 * can take input of the form:
 *
 * <type>/?name=<name>&baseURI=<baseURI>
 * entity/?type=<type>&name=<name>&baseURI=<baseURI>
 * ontologies/?uri=<uri>
 */
public class Summary extends AbstractOntologyServerServlet {

    private static final String PARAM_ONTOLOGY = "ontology";
    private static final String PARAM_URI = "uri";
    private static final String PARAM_NAME = "name";
    private static final String PARAM_EXPAND = "expanded";

    protected void handleXMLRequest(Map<String, String> params, OWLHTMLServer server, URL servletURL, PrintWriter out) throws OntServerException {

        String uri = params.get(PARAM_URI);
        String entityName = params.get(PARAM_NAME);
        NamedObjectType type = server.getURLScheme().getType(servletURL);

        Set<OWLNamedObject> results = Collections.emptySet();
        if (uri == null && entityName == null){
            results = getIndexResults(params, server, type);
        }
        renderXMLResults(results, server, out);
        // not yet implemented for entities
    }

    protected HTMLDoclet handleHTMLRequest(Map<String, String> params, OWLHTMLServer server, URL pageURL) throws OntServerException {

        String uri = params.get(PARAM_URI);
        String entityName = params.get(PARAM_NAME);
        String ontology = params.get(PARAM_ONTOLOGY);
        String expanded = params.get(PARAM_EXPAND);

        NamedObjectType type = server.getURLScheme().getType(pageURL);
        try {

            if (uri != null){
                String findURL = "find/?format=html&type=" + type + "&uri=" + URLEncoder.encode(uri, OWLHTMLConstants.DEFAULT_ENCODING);
                if (ontology != null){
                    findURL += "&ontology=" + ontology;
                }
                throw new RedirectException(server.getURLScheme().getURLForRelativePage(findURL));
            }
            else if (entityName != null){
                String findURL = "find/?format=html&type=" + type + "&input=" + URLEncoder.encode(entityName, OWLHTMLConstants.DEFAULT_ENCODING);
                if (ontology != null){
                    findURL += "&ontology=" + ontology;
                }
                throw new RedirectException(server.getURLScheme().getURLForRelativePage(findURL));
            }
            else{
                OWLNamedObject object = server.getURLScheme().getNamedObjectForURL(pageURL);

                if (object == null){
                    final OWLOntology ont = getOntology(ontology, server);
                    String title = type.getPluralRendering();
                    if (ont != null){
                        title += " referenced in " + server.getNameRenderer().getShortForm(ont);
                    }
                    else{
                        title = "All " + title;
                    }
                    Set<OWLNamedObject> results = getIndexResults(params, server, type);
                    OWLEntityIndexHTMLPage ren = createIndexRenderer(title, results, server);
                    prepareIndex(ren, ont, type, server);
                    return ren;
                }
                else {
                    return getSummaryRenderer(object, server, ServerConstants.TRUE.equals(expanded));
                }
            }
        }
        catch (UnsupportedEncodingException e) {
            throw new OntServerException(e);
        }
    }

    protected Map<String, Set<String>> getRequiredParams(OWLServer server) {
        Map<String, Set<String>> params = new HashMap<String,  Set<String>>();
        // all optional
        return params;
    }

    private Set<OWLNamedObject> getIndexResults(Map<String, String> params, OWLHTMLServer server, NamedObjectType type) throws OntServerException {
        Set<OWLNamedObject> results = new HashSet<OWLNamedObject>();

        String ontStr = params.get(PARAM_ONTOLOGY);

        OWLOntology ont = getOntology(ontStr, server);

        if (type.equals(NamedObjectType.ontologies)){
            results.addAll(server.getVisibleOntologies());
        }
        else{
            final OWLDataFactory df = server.getOWLOntologyManager().getOWLDataFactory();
            if (ont != null){ // if ontology specified, just display that one
                results.addAll(type.getNamedObjectsFromOntology(ont));
            }
            else{
                // no ontology specified, so display all
                for (OWLOntology ontology : server.getVisibleOntologies()){
                    results.addAll(type.getNamedObjectsFromOntology(ontology));
                }
            }
            if (type.equals(NamedObjectType.classes)){
                results.add(df.getOWLThing());
            }
        }

        return results;
    }


    private EmptyOWLDocPage createNotFoundHTML(NamedObjectType type, String entityName, OWLHTMLServer server) throws OntServerException {
        final String message = "Cannot render a page for unknown " + type + ": " + entityName;
        if (entityName != null){
            OWLNamedObjectFinder finder = server.getFinder();
            Set<? extends OWLNamedObject> entities = finder.getOWLNamedObjects(entityName + ".*", type);
            return createIndexRenderer(message, entities, server);
        }
        throw new OntServerException(message);
    }

    private void prepareIndex(OWLEntityIndexHTMLPage ren, OWLOntology ont, NamedObjectType type, OWLHTMLServer server) {

        int position = ren.indexOf(ren.getDoclet(TabsDoclet.ID))+1;

        // put in the title of the ontology, if specified
        if (ont != null){
            final NamedObjectTitleDoclet<OWLOntology> titleDoclet = new NamedObjectTitleDoclet<OWLOntology>(server);
            titleDoclet.setUserObject(ont);
            titleDoclet.setPinned(true);
            ren.addDoclet(titleDoclet, position++);
        }

        // add a tiny fragment of the class tree from owl:Thing
        if (type.equals(NamedObjectType.classes) && server.getProperties().isSet(OWLHTMLConstants.OPTION_SHOW_MINI_HIERARCHIES)){
            final OWLClass owlThing = server.getOWLOntologyManager().getOWLDataFactory().getOWLThing();
            ren.setUserObject(owlThing);
            ToldClassHierarchyReasoner hp;
            if (ont != null){
                hp = new ToldClassHierarchyReasoner(server.getOWLOntologyManager());
                hp.loadOntologies(Collections.singleton(ont));
                hp.classify();
            }
            else{
                hp = server.getClassHierarchyProvider(); // @@TODO still wrong as this has all active ontologies instead of visible
            }
            OWLClassHierarchyTreeFragment model = new OWLClassHierarchyTreeFragment(server, hp, "Asserted Class Hierarchy");
            model.setAncestorLevels(3);
            model.setDescendantLevels(2);
            HierarchyRootDoclet<OWLClass> hierarchyDoclet = new HierarchyRootDoclet<OWLClass>(server, model);
            ren.addDoclet(hierarchyDoclet, position);
        }
    }

    private EmptyOWLDocPage getSummaryRenderer(OWLObject owlObject, OWLHTMLServer server, boolean expand) {
        EmptyOWLDocPage ren = null;

        if (owlObject instanceof OWLClass){
            OWLClassSummaryHTMLPage summaryRenderer = new OWLClassSummaryHTMLPage(server);

            if (server.getProperties().isSet(OWLHTMLConstants.OPTION_SHOW_MINI_HIERARCHIES)){
                final OWLClassReasoner hp;
                String title;
                if (server.getProperties().isSet(OWLHTMLConstants.OPTION_SHOW_INFERRED_HIERARCHIES)){
                    hp = server.getOWLReasoner();
                    title = "Inferred Class Hierarchy";
                }
                else{
                    hp = server.getClassHierarchyProvider();
                    title = "Asserted Class Hierarchy";
                }
                final OWLClassHierarchyTreeFragment treeModel = new OWLClassHierarchyTreeFragment(server, hp, title);
                HierarchyRootDoclet<OWLClass> hierarchyRenderer = new HierarchyRootDoclet<OWLClass>(server, treeModel);
                hierarchyRenderer.setAutoExpandEnabled(expand);
                summaryRenderer.setOWLHierarchyRenderer(hierarchyRenderer);
            }
            ren = summaryRenderer;
        }
        else if (owlObject instanceof OWLObjectProperty){
            OWLObjectPropertySummaryHTMLPage summaryRenderer = new OWLObjectPropertySummaryHTMLPage(server);

            if (server.getProperties().isSet(OWLHTMLConstants.OPTION_SHOW_MINI_HIERARCHIES)){
                final OWLPropertyHierarchyTreeFragment treeModel = new OWLPropertyHierarchyTreeFragment(server, server.getPropertyHierarchyProvider());
                HierarchyRootDoclet<OWLObjectProperty> hierarchyRenderer = new HierarchyRootDoclet<OWLObjectProperty>(server, treeModel);
                hierarchyRenderer.setAutoExpandEnabled(expand);
                summaryRenderer.setOWLHierarchyRenderer(hierarchyRenderer);
            }
            ren = summaryRenderer;
        }
        else if (owlObject instanceof OWLDataProperty){
            OWLDataPropertySummaryHTMLPage summaryRenderer = new OWLDataPropertySummaryHTMLPage(server);

            if (server.getProperties().isSet(OWLHTMLConstants.OPTION_SHOW_MINI_HIERARCHIES)){
                final OWLPropertyHierarchyTreeFragment treeModel = new OWLPropertyHierarchyTreeFragment(server, server.getPropertyHierarchyProvider());
                HierarchyRootDoclet<OWLDataProperty> hierarchyRenderer = new HierarchyRootDoclet<OWLDataProperty>(server, treeModel);
                hierarchyRenderer.setAutoExpandEnabled(expand);
                summaryRenderer.setOWLHierarchyRenderer(hierarchyRenderer);
            }
            ren = summaryRenderer;
        }
        else if (owlObject instanceof OWLIndividual){
            ren = new OWLIndividualSummaryHTMLPage(server);
        }
        else if (owlObject instanceof OWLOntology){
            ren = new OWLOntologySummaryHTMLPage(server);
        }

        if (ren != null){
            ren.setUserObject(owlObject);
        }
        return ren;
    }

    private OWLOntology getOntology(String ontStr, OWLHTMLServer server) throws OntServerException {
        if (ontStr != null){
            return server.getOWLOntologyManager().getOntology(URI.create(ontStr));
        }
        return null;
    }
}
