package org.coode.www.servlet;

import org.coode.html.OWLHTMLKit;
import org.coode.html.util.URLUtils;
import org.coode.html.doclet.*;
import org.coode.html.hierarchy.OWLClassHierarchyTreeFragment;
import org.coode.html.hierarchy.OWLPropertyHierarchyTreeFragment;
import org.coode.html.impl.OWLHTMLConstants;
import org.coode.html.impl.OWLHTMLParam;
import org.coode.html.impl.OWLHTMLProperty;
import org.coode.html.index.OWLObjectIndexHTMLPage;
import org.coode.html.page.EmptyOWLDocPage;
import org.coode.html.summary.*;
import org.coode.owl.mngr.*;
import org.coode.owl.mngr.impl.ClassHierarchyProvider;
import org.coode.owl.mngr.impl.InferredClassHierarchyProvider;
import org.coode.owl.util.ModelUtil;
import org.coode.www.exception.OntServerException;
import org.coode.www.exception.RedirectException;
import org.coode.www.OntologyBrowserConstants;
import org.semanticweb.owlapi.model.*;

import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
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

    protected void handleXMLRequest(Map<OWLHTMLParam, String> params, OWLHTMLKit kit, URL servletURL, PrintWriter out) throws OntServerException {

        String uri = params.get(OWLHTMLParam.uri);
        String entityName = params.get(OWLHTMLParam.name);
        NamedObjectType type = kit.getURLScheme().getType(servletURL);

        Set<OWLObject> results = Collections.emptySet();
        if (uri == null && entityName == null){
            results = getIndexResults(params, kit, type);
        }
        renderXMLResults(results, kit.getOWLServer(), out);
        // not yet implemented for entities
    }

    protected HTMLDoclet handleHTMLRequest(Map<OWLHTMLParam, String> params, OWLHTMLKit kit, URL pageURL) throws OntServerException {

        String uri = params.get(OWLHTMLParam.uri);
        String entityName = params.get(OWLHTMLParam.name);
        String ontology = params.get(OWLHTMLParam.ontology);
        String expanded = params.get(OWLHTMLParam.expanded);

        NamedObjectType type = kit.getURLScheme().getType(pageURL);
        try {

            if (uri != null || entityName != null){
                // if a name or uri is specified then redirect to search
                OWLHTMLParam criteria = uri != null ? OWLHTMLParam.uri : OWLHTMLParam.input;

                Map<OWLHTMLParam, String> map = new HashMap<OWLHTMLParam, String>();
                map.put(OWLHTMLParam.format, OntologyBrowserConstants.FORMAT_HTML);
                map.put(OWLHTMLParam.type, type.toString());
                map.put(criteria, URLEncoder.encode(uri, OWLHTMLConstants.DEFAULT_ENCODING));
                if (ontology != null){
                    map.put(OWLHTMLParam.ontology, ontology);
                }
                StringBuilder sb = new StringBuilder("find/");
                sb.append(URLUtils.renderParams(map));

                throw new RedirectException(kit.getURLScheme().getURLForRelativePage(sb.toString()));
            }
            else{
                OWLObject object = kit.getURLScheme().getOWLObjectForURL(pageURL);

                if (object == null){
                    final OWLOntology ont = getOntology(ontology, kit);
                    StringBuilder sb = new StringBuilder();
                    if (ont != null){
                        sb.append(type.getPluralRendering());
                        sb.append(" referenced in ");
                        sb.append(kit.getOWLServer().getOntologyShortFormProvider().getShortForm(ont));
                    }
                    else{
                        sb.append("All ");
                        sb.append(type.getPluralRendering());
                    }
                    Set<OWLObject> results = getIndexResults(params, kit, type);
                    OWLObjectIndexHTMLPage ren = createIndexRenderer(sb.toString(), results, kit);
                    prepareIndex(ren, ont, type, kit);
                    return ren;
                }
                else {
                    return getSummaryRenderer(object, kit, Boolean.getBoolean(expanded));
                }
            }
        }
        catch (UnsupportedEncodingException e) {
            throw new OntServerException(e);
        }
    }


    protected Map<OWLHTMLParam, Set<String>> getRequiredParams(OWLServer server) {
        Map<OWLHTMLParam, Set<String>> params = new HashMap<OWLHTMLParam,  Set<String>>();
        // all optional
        return params;
    }

    private Set<OWLObject> getIndexResults(Map<OWLHTMLParam, String> params, OWLHTMLKit kit, NamedObjectType type) throws OntServerException {

        Set<OWLObject> results = new HashSet<OWLObject>();

        String ontStr = params.get(OWLHTMLParam.ontology);

        OWLOntology ont = getOntology(ontStr, kit);

        if (ont != null){ // if ontology specified, just display that one
            results.addAll(ModelUtil.getOWLEntitiesFromOntology(type, ont));
        }
        else if (type.equals(NamedObjectType.ontologies)){
            results.addAll(kit.getOWLServer().getActiveOntologies());
        }
        else{
            // no ontology specified, so display all
            for (OWLOntology ontology : kit.getVisibleOntologies()){
                results.addAll(ModelUtil.getOWLEntitiesFromOntology(type, ontology));
            }
        }
        if (type.equals(NamedObjectType.classes)){
            final OWLDataFactory df = kit.getOWLServer().getOWLOntologyManager().getOWLDataFactory();
            results.add(df.getOWLThing());
        }

        return results;
    }


    private EmptyOWLDocPage createNotFoundHTML(NamedObjectType type, String entityName, OWLHTMLKit kit) throws OntServerException {
        final String message = "Cannot render a page for unknown " + type + ": " + entityName;
        if (entityName != null){
            OWLEntityFinder finder = kit.getOWLServer().getFinder();
            Set<? extends OWLEntity> entities = finder.getOWLEntities(entityName + ".*", type);
            return createIndexRenderer(message, entities, kit);
        }
        throw new OntServerException(message);
    }

    private void prepareIndex(OWLObjectIndexHTMLPage ren, final OWLOntology ont, NamedObjectType type, OWLHTMLKit kit) {

        int position = ren.indexOf(ren.getDoclet(TabsDoclet.ID))+1;

        // put in the title of the ontology, if specified
        if (ont != null){
            final OntologyTitleDoclet titleDoclet = new OntologyTitleDoclet(kit);
            titleDoclet.setUserObject(ont);
            titleDoclet.setPinned(true);
            ren.addDoclet(titleDoclet, position++);
        }

        // add a tiny fragment of the class tree from owl:Thing
        if (type.equals(NamedObjectType.classes) && isShowMiniHierarchiesEnabled(kit)){
            final OWLClass owlThing = kit.getOWLServer().getOWLOntologyManager().getOWLDataFactory().getOWLThing();
            ren.setUserObject(owlThing);
            HierarchyProvider<OWLClass> hp;
            if (ont != null){
                hp = new ClassHierarchyProvider(kit.getOWLServer()){
                    
                    protected Set<OWLOntology> getOntologies() {
                        return Collections.singleton(ont);
                    }
                };
            }
            else{
                hp = kit.getOWLServer().getClassHierarchyProvider();
            }
            OWLClassHierarchyTreeFragment model = new OWLClassHierarchyTreeFragment(kit, hp, "Asserted Class Hierarchy");
            model.setAncestorLevels(3);
            model.setDescendantLevels(2);
            HierarchyRootDoclet<OWLClass> hierarchyDoclet = new HierarchyRootDoclet<OWLClass>(kit, model);
            ren.addDoclet(hierarchyDoclet, position);
        }
    }

    private EmptyOWLDocPage getSummaryRenderer(OWLObject owlObject, OWLHTMLKit kit, boolean expand) {
        EmptyOWLDocPage ren = null;

        if (owlObject instanceof OWLClass){
            OWLClassSummaryHTMLPage summaryRenderer = new OWLClassSummaryHTMLPage(kit);

            if (isShowMiniHierarchiesEnabled(kit)){
                final HierarchyProvider<OWLClass> hp;
                String title;
                if (kit.getHTMLProperties().isSet(OWLHTMLProperty.optionShowInferredHierarchies)){
                    hp = new InferredClassHierarchyProvider(kit.getOWLServer());
                    title = "Inferred Class Hierarchy";
                }
                else{
                    hp = kit.getOWLServer().getClassHierarchyProvider();
                    title = "Asserted Class Hierarchy";
                }
                final OWLClassHierarchyTreeFragment treeModel = new OWLClassHierarchyTreeFragment(kit, hp, title);
                HierarchyRootDoclet<OWLClass> hierarchyRenderer = new HierarchyRootDoclet<OWLClass>(kit, treeModel);
                hierarchyRenderer.setAutoExpandEnabled(expand);
                summaryRenderer.setOWLHierarchyRenderer(hierarchyRenderer);
            }
            ren = summaryRenderer;
        }
        else if (owlObject instanceof OWLObjectProperty){
            OWLObjectPropertySummaryHTMLPage summaryRenderer = new OWLObjectPropertySummaryHTMLPage(kit);

            if (isShowMiniHierarchiesEnabled(kit)){
                final OWLPropertyHierarchyTreeFragment<OWLObjectProperty> treeModel =
                        new OWLPropertyHierarchyTreeFragment<OWLObjectProperty>(kit, kit.getOWLServer().getOWLObjectPropertyHierarchyProvider());
                HierarchyRootDoclet<OWLObjectProperty> hierarchyRenderer = new HierarchyRootDoclet<OWLObjectProperty>(kit, treeModel);
                hierarchyRenderer.setAutoExpandEnabled(expand);
                summaryRenderer.setOWLHierarchyRenderer(hierarchyRenderer);
            }
            ren = summaryRenderer;
        }
        else if (owlObject instanceof OWLDataProperty){
            OWLDataPropertySummaryHTMLPage summaryRenderer = new OWLDataPropertySummaryHTMLPage(kit);

            if (isShowMiniHierarchiesEnabled(kit)){
                final OWLPropertyHierarchyTreeFragment<OWLDataProperty> treeModel =
                        new OWLPropertyHierarchyTreeFragment<OWLDataProperty>(kit, kit.getOWLServer().getOWLDataPropertyHierarchyProvider());
                HierarchyRootDoclet<OWLDataProperty> hierarchyRenderer = new HierarchyRootDoclet<OWLDataProperty>(kit, treeModel);
                hierarchyRenderer.setAutoExpandEnabled(expand);
                summaryRenderer.setOWLHierarchyRenderer(hierarchyRenderer);
            }
            ren = summaryRenderer;
        }
        else if (owlObject instanceof OWLAnnotationProperty){
//            if (kit.getProperties().isSet(OWLHTMLConstants.OPTION_SHOW_MINI_HIERARCHIES)){
//                final OWLAnnotationPropertyHierarchyTreeFragment treeModel = new OWLPropertyHierarchyTreeFragment(kit, kit.getPropertyHierarchyProvider());
//                HierarchyRootDoclet<OWLDataProperty> hierarchyRenderer = new HierarchyRootDoclet<OWLDataProperty>(kit, treeModel);
//                hierarchyRenderer.setAutoExpandEnabled(expand);
//                summaryRenderer.setOWLHierarchyRenderer(hierarchyRenderer);
//            }
            ren = new OWLAnnotationPropertySummaryHTMLPage(kit);
        }
        else if (owlObject instanceof OWLNamedIndividual){
            ren = new OWLIndividualSummaryHTMLPage(kit);
        }
        else if (owlObject instanceof OWLDatatype){
            ren = new OWLDatatypeSummaryHTMLPage(kit);
        }
        else if (owlObject instanceof OWLOntology){
            ren = new OWLOntologySummaryHTMLPage(kit);
        }

        if (ren != null){
            ren.setUserObject(owlObject);
        }
        return ren;
    }


    private boolean isShowMiniHierarchiesEnabled(OWLHTMLKit kit) {
        return kit.getHTMLProperties().isSet(OWLHTMLProperty.optionShowMiniHierarchies);
    }


    private OWLOntology getOntology(String ontStr, OWLHTMLKit kit) throws OntServerException {
        if (ontStr != null){
            return kit.getOWLServer().getOWLOntologyManager().getOntology(IRI.create(ontStr));
        }
        return null;
    }
}
