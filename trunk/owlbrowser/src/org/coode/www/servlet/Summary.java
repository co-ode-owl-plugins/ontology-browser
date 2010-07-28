package org.coode.www.servlet;

import org.coode.html.OWLHTMLKit;
import org.coode.html.doclet.HTMLDoclet;
import org.coode.html.doclet.HierarchyDoclet;
import org.coode.html.doclet.OntologyTitleDoclet;
import org.coode.html.doclet.TabsDoclet;
import org.coode.html.impl.OWLHTMLConstants;
import org.coode.html.impl.OWLHTMLParam;
import org.coode.html.impl.OWLHTMLProperty;
import org.coode.html.index.OWLObjectIndexHTMLPage;
import org.coode.html.page.OWLDocPage;
import org.coode.html.summary.*;
import org.coode.html.url.URLScheme;
import org.coode.html.util.URLUtils;
import org.coode.owl.mngr.HierarchyProvider;
import org.coode.owl.mngr.NamedObjectType;
import org.coode.owl.mngr.OWLEntityFinder;
import org.coode.owl.mngr.OWLServer;
import org.coode.owl.mngr.impl.InferredClassHierarchyProvider;
import org.coode.owl.util.ModelUtil;
import org.coode.www.OntologyBrowserConstants;
import org.coode.www.exception.OntServerException;
import org.coode.www.exception.RedirectException;
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
        String ontology = params.get(OWLHTMLParam.ontology);
        NamedObjectType type = kit.getURLScheme().getType(servletURL);

        Set<OWLObject> results = Collections.emptySet();
        if (uri == null && entityName == null){
            results = getIndexResults(getOntology(ontology, kit), kit, type);
        }
        renderXMLResults(results, kit.getOWLServer(), out);
        // not yet implemented for entities
    }

    protected HTMLDoclet handleHTMLRequest(Map<OWLHTMLParam, String> params, OWLHTMLKit kit, URL pageURL) throws OntServerException {

        String uri = params.get(OWLHTMLParam.uri);
        String entityName = params.get(OWLHTMLParam.name);
        String ontology = params.get(OWLHTMLParam.ontology);
//        String expanded = params.get(OWLHTMLParam.expanded);

        final URLScheme urlScheme = kit.getURLScheme();

        NamedObjectType type = urlScheme.getType(pageURL);

        // if a name or uri is specified then redirect to search
        if (uri != null || entityName != null){
            performSearch(type, uri, entityName, ontology, kit);
        }
        else{
            OWLObject object = urlScheme.getOWLObjectForURL(pageURL);

            // @@TODO handle summary pages when ontology specified

            if (object == null && ontology == null && isShowMiniHierarchiesEnabled(kit)){
                final OWLDataFactory df = kit.getOWLServer().getOWLOntologyManager().getOWLDataFactory();
                switch(urlScheme.getType(pageURL)){
                    case classes:
                        throw new RedirectException(urlScheme.getURLForOWLObject(df.getOWLThing()));
                    case objectproperties:
                        throw new RedirectException(urlScheme.getURLForOWLObject(df.getOWLTopObjectProperty()));
                    case dataproperties:
                        throw new RedirectException(urlScheme.getURLForOWLObject(df.getOWLTopDataProperty()));
                    case annotationproperties:
                        Set<OWLAnnotationProperty> annotationProperties = kit.getOWLServer().getOWLAnnotationPropertyHierarchyProvider().getRoots();
                        if (!annotationProperties.isEmpty()){
                            List<OWLAnnotationProperty> aps = new ArrayList<OWLAnnotationProperty>(annotationProperties);
                            Collections.sort(aps, kit.getOWLObjectComparator());
                            throw new RedirectException(urlScheme.getURLForOWLObject(aps.get(0)));
                        }
                        break;
                    case datatypes:
                        throw new RedirectException(urlScheme.getURLForOWLObject(df.getTopDatatype()));
                }
            }

            if (object == null){
                return getIndexRenderer(type, kit, getOntology(ontology, kit));
            }
            else {
                return getSummaryPage(object, kit, true);
            }
        }
        throw new RuntimeException("Cannot get here");
    }

    private void performSearch(NamedObjectType type, String uri, String entityName, String ontology, OWLHTMLKit kit) throws OntServerException {
        try{
            Map<OWLHTMLParam, String> map = new HashMap<OWLHTMLParam, String>();
            map.put(OWLHTMLParam.format, OntologyBrowserConstants.FORMAT_HTML);
            map.put(OWLHTMLParam.type, type.toString());
            if (uri != null){
                map.put(OWLHTMLParam.uri, URLEncoder.encode(uri, OWLHTMLConstants.DEFAULT_ENCODING));
            }
            else{
                map.put(OWLHTMLParam.input, entityName);
            }

            if (ontology != null){
                map.put(OWLHTMLParam.ontology, ontology);
            }
            StringBuilder sb = new StringBuilder("find/");
            sb.append(URLUtils.renderParams(map));

            throw new RedirectException(kit.getURLScheme().getURLForRelativePage(sb.toString()));
        }
        catch (UnsupportedEncodingException e) {
            throw new OntServerException(e);
        }
    }

    private HTMLDoclet getIndexRenderer(NamedObjectType type, OWLHTMLKit kit, OWLOntology ont) throws OntServerException {
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
        Set<OWLObject> results = getIndexResults(ont, kit, type);
        OWLObjectIndexHTMLPage ren = createIndexRenderer(sb.toString(), results, kit);
        prepareIndex(ren, ont, type, kit);
        return ren;
    }


    protected Map<OWLHTMLParam, Set<String>> getRequiredParams(OWLServer server) {
        // all optional
        return new HashMap<OWLHTMLParam,  Set<String>>();
    }

    private Set<OWLObject> getIndexResults(OWLOntology ont, OWLHTMLKit kit, NamedObjectType type) throws OntServerException {

        Set<OWLObject> results = new HashSet<OWLObject>();

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


    private OWLDocPage createNotFoundHTML(NamedObjectType type, String entityName, OWLHTMLKit kit) throws OntServerException {
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
    }

    private OWLDocPage getSummaryPage(OWLObject owlObject, OWLHTMLKit kit, boolean expand) throws OntServerException {
        OWLDocPage page = null;

        if (owlObject instanceof OWLClass){
            OWLClassSummaryHTMLPage summaryRenderer = new OWLClassSummaryHTMLPage(kit);

            if (isShowMiniHierarchiesEnabled(kit)){
                final HierarchyProvider<OWLClass> hp;
                String title;
                if (kit.getHTMLProperties().isSet(OWLHTMLProperty.optionShowInferredHierarchies)){
                    hp = new InferredClassHierarchyProvider(kit.getOWLServer());
                    title = "Classes (Inferred)";
                }
                else{
                    hp = kit.getOWLServer().getClassHierarchyProvider();
                    title = "Classes";
                }
                HierarchyDoclet<OWLClass> hierarchyRenderer = new HierarchyDoclet<OWLClass>(title, kit, hp);
                hierarchyRenderer.setAutoExpandEnabled(expand);
                summaryRenderer.setNavigationRenderer(hierarchyRenderer);
            }
            page = summaryRenderer;
        }
        else if (owlObject instanceof OWLObjectProperty){
            OWLObjectPropertySummaryHTMLPage summaryRenderer = new OWLObjectPropertySummaryHTMLPage(kit);

            if (isShowMiniHierarchiesEnabled(kit)){
                final HierarchyProvider<OWLObjectProperty> hp = kit.getOWLServer().getOWLObjectPropertyHierarchyProvider();
                HierarchyDoclet<OWLObjectProperty> hierarchyRenderer = new HierarchyDoclet<OWLObjectProperty>("Object Properties", kit, hp);
                hierarchyRenderer.setAutoExpandEnabled(expand);
                summaryRenderer.setNavigationRenderer(hierarchyRenderer);
            }
            page = summaryRenderer;
        }
        else if (owlObject instanceof OWLDataProperty){
            OWLDataPropertySummaryHTMLPage summaryRenderer = new OWLDataPropertySummaryHTMLPage(kit);

            if (isShowMiniHierarchiesEnabled(kit)){
                final HierarchyProvider<OWLDataProperty> hp = kit.getOWLServer().getOWLDataPropertyHierarchyProvider();
                HierarchyDoclet<OWLDataProperty> hierarchyRenderer = new HierarchyDoclet<OWLDataProperty>("Data Properties", kit, hp);
                hierarchyRenderer.setAutoExpandEnabled(expand);
                summaryRenderer.setNavigationRenderer(hierarchyRenderer);
            }
            page = summaryRenderer;
        }
        else if (owlObject instanceof OWLAnnotationProperty){
            OWLAnnotationPropertySummaryHTMLPage summaryRenderer = new OWLAnnotationPropertySummaryHTMLPage(kit);
            if (isShowMiniHierarchiesEnabled(kit)){
                final HierarchyProvider<OWLAnnotationProperty> hp = kit.getOWLServer().getOWLAnnotationPropertyHierarchyProvider();
                HierarchyDoclet<OWLAnnotationProperty> hierarchyRenderer = new HierarchyDoclet<OWLAnnotationProperty>("Annotation Properties", kit, hp);
                hierarchyRenderer.setAutoExpandEnabled(expand);
                summaryRenderer.setNavigationRenderer(hierarchyRenderer);
            }
            page = summaryRenderer;
        }
        else if (owlObject instanceof OWLNamedIndividual){
            OWLIndividualSummaryHTMLPage summaryRenderer = new OWLIndividualSummaryHTMLPage(kit);
            if (isShowMiniHierarchiesEnabled(kit)){
                final HierarchyProvider<OWLObject> hp = kit.getOWLServer().getOWLIndividualsHierarchyProvider();
                HierarchyDoclet<OWLObject> hierarchyRenderer = new HierarchyDoclet<OWLObject>("Individuals by type", kit, hp);
                hierarchyRenderer.setAutoExpandEnabled(expand);
                summaryRenderer.setNavigationRenderer(hierarchyRenderer);
            }
            page = summaryRenderer;
        }
        else if (owlObject instanceof OWLDatatype){
            OWLDatatypeSummaryHTMLPage summaryRenderer = new OWLDatatypeSummaryHTMLPage(kit);
            if (isShowMiniHierarchiesEnabled(kit)){
                final HierarchyProvider<OWLDatatype> hp = kit.getOWLServer().getOWLDatatypeHierarchyProvider();
                HierarchyDoclet<OWLDatatype> hierarchyRenderer = new HierarchyDoclet<OWLDatatype>("Datatypes", kit, hp);
                hierarchyRenderer.setAutoExpandEnabled(expand);
                summaryRenderer.setNavigationRenderer(hierarchyRenderer);
            }
            page = summaryRenderer;
        }
        else if (owlObject instanceof OWLOntology){
            OWLOntologySummaryHTMLPage summaryRenderer = new OWLOntologySummaryHTMLPage(kit);
            if (isShowMiniHierarchiesEnabled(kit)){
                final HierarchyProvider<OWLOntology> hp = kit.getOWLServer().getOntologyHierarchyProvider();
                HierarchyDoclet<OWLOntology> hierarchyRenderer = new HierarchyDoclet<OWLOntology>("Ontologies", kit, hp);
                hierarchyRenderer.setAutoExpandEnabled(expand);
                summaryRenderer.setNavigationRenderer(hierarchyRenderer);
            }
            page = summaryRenderer;
        }

        if (page != null){
            page.setUserObject(owlObject);
        }
        return page;
    }


    private boolean isShowMiniHierarchiesEnabled(OWLHTMLKit kit) {
        return kit.getHTMLProperties().isSet(OWLHTMLProperty.optionShowMiniHierarchies);
    }


    private OWLOntology getOntology(String ontStr, OWLHTMLKit kit) throws OntServerException {
        if (ontStr != null){
            return kit.getOWLServer().getOntologyForIRI(IRI.create(ontStr));
        }
        return null;
    }
}
