package org.coode.www.servlet;

import com.sun.org.apache.xerces.internal.parsers.DOMParser;
import org.coode.html.OWLHTMLKit;
import org.coode.html.doclet.HTMLDoclet;
import org.coode.html.doclet.NestedHTMLDoclet;
import org.coode.html.impl.OWLHTMLConstants;
import org.coode.html.impl.OWLHTMLParam;
import org.coode.html.page.EmptyOWLDocPage;
import org.coode.html.util.FileUtils;
import org.coode.html.util.URLUtils;
import org.coode.owl.mngr.OWLServer;
import org.coode.owl.mngr.ServerConstants;
import org.coode.www.ManageAction;
import org.coode.www.OntologyBrowserConstants;
import org.coode.www.doclet.BlurbDoclet;
import org.coode.www.doclet.LoadFormDoclet;
import org.coode.www.doclet.OntologyMappingsTableDoclet;
import org.coode.www.exception.OntServerException;
import org.coode.www.exception.RedirectException;
import org.coode.www.mngr.SessionManager;
import org.semanticweb.owlapi.model.OWLImportsDeclaration;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyID;
import org.semanticweb.owlapi.model.IRI;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import java.io.*;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.*;

/**
 * Author: drummond<br>
 * The University Of Manchester<br>
 * Medical Informatics Group<br>
 * Date: Jul 3, 2006<br><br>
 * <p/>
 * nick.drummond@cs.manchester.ac.uk<br>
 * www.cs.man.ac.uk/~drummond<br><br>
 *
 * Cannot subclass AbstractOntologyServerServlet as that checks if an ontology has been loaded
 */
public class ManageOntologies extends AbstractOntologyServerServlet {

    protected void handleXMLRequest(Map<OWLHTMLParam, String> params, OWLHTMLKit kit, URL servletURL, PrintWriter out) throws OntServerException {
        // no implementation
    }

    protected HTMLDoclet handleHTMLRequest(Map<OWLHTMLParam, String> params, OWLHTMLKit kit, URL pageURL) throws OntServerException {
        try {

            final String actionValue = params.get(OWLHTMLParam.action);

            NestedHTMLDoclet ren = null;

            OWLServer server = kit.getOWLServer();

            if (actionValue != null){
                ManageAction action = ManageAction.valueOf(actionValue);

                switch(action){
                    case load:
                        boolean clear = Boolean.parseBoolean(params.get(OWLHTMLParam.clear));
                        ren = handleLoad(getURIFromParam(params.get(OWLHTMLParam.uri)), clear, kit, pageURL);
                        break;
                    case remove:
                        ren = handleRemove(getOntologyFromParam(params.get(OWLHTMLParam.uri), server), kit, pageURL);
                        break;
                    case reload:
                        ren = handleReload(getOntologyFromParam(params.get(OWLHTMLParam.uri), server), kit, pageURL);
                        break;
                    case browse:
                        ren = handleBrowse(getOntologyFromParam(params.get(OWLHTMLParam.uri), server), kit, pageURL);
                        break;
                    case hide:
                        handleSetVisibility(getOntologyFromParam(params.get(OWLHTMLParam.uri), server), false, kit);
                        break;
                    case unhide:
                        handleSetVisibility(getOntologyFromParam(params.get(OWLHTMLParam.uri), server), true, kit);
                        break;
                }
            }

            if (ren == null){
                final Map<OWLOntologyID, URI> map = ManageOntologies.getMap(server);
                ren = createManagePageRenderer(kit, map, null, pageURL);
            }

            return ren;
        }
        catch (URISyntaxException e) {
            throw new OntServerException(e);
        }
    }


    protected Map<OWLHTMLParam, Set<String>> getRequiredParams(OWLServer server) {
        Map<OWLHTMLParam, Set<String>> required = new HashMap<OWLHTMLParam, Set<String>>();
//        required.put(PARAM_URI, Collections.singleton("<ontology uri>")); optional
//        required.put(PARAM_ACTION, getActionRenderings());
        return required;
    }

    private Set<String> getActionRenderings() {
        Set<String> actions = new HashSet<String>();
        for (ManageAction action : ManageAction.values()){
            actions.add(action.toString());
        }
        return actions;
    }

    private NestedHTMLDoclet handleLoad(URI uri, boolean clear, OWLHTMLKit kit, URL pageURL) throws OntServerException {
        Set<URI> success = new HashSet<URI>();
        Map<URI, Throwable> fail = new HashMap<URI, Throwable>();

        String message = "";

        OWLServer server = kit.getOWLServer();

        if (clear){
            server.clearOntologies();
        }

        try{
            if (uri.isAbsolute()){
                server.loadOntology(uri);
                success.add(uri);
            }
            else{
                throw new IllegalArgumentException("Ontology URIs must be absolute");
            }
        }
        catch(Exception e){
            fail.put(uri, e);
        }
        catch (OutOfMemoryError e){
            fail.put(uri, e);
            // clear all ontologies as we are in an unpredictable state
            server.clearOntologies();
            throw new OntServerException("Out of memory trying to load ontologies");
        }


        if (!fail.isEmpty()){
            for (URI f : fail.keySet()){
                message += "failed to load: " + uri +
                           " ("  + fail.get(f).getClass().getSimpleName() +
                           ": " + fail.get(f).getMessage() + ")<br />";
            }
        }
        if (!success.isEmpty()){
            SessionManager.labelServerState(kit);
            message += "<p>loaded " + success.size() + " ontologies</p>";
            message += "<p>saved session: [" + kit.getCurrentLabel() + "]</p>";
        }

        Map<OWLOntologyID, URI> map = ManageOntologies.getMap(server);

        if (map.isEmpty() || map.containsValue(null)){ // empty or missing value in map
            return createManagePageRenderer(kit, map, message, pageURL);
        }
        else{
            throw new RedirectException(kit.getBaseURL());
        }
    }


    private NestedHTMLDoclet handleRemove(OWLOntology ontology, OWLHTMLKit kit, URL pageURL) throws OntServerException {
        StringBuilder sb = new StringBuilder();

        OWLServer server = kit.getOWLServer();

        if (ontology != null){
            server.removeOntology(ontology);

            SessionManager.labelServerState(kit);
            sb.append("<p>Removed ");
            sb.append(server.getOntologyShortFormProvider().getShortForm(ontology));
            sb.append("</p><p>saved session: [");
            sb.append(kit.getCurrentLabel());
            sb.append("]</p>");
        }
        else{
            sb.append("Could not remove ontology: ");
            sb.append(server.getOntologyShortFormProvider().getShortForm(ontology));
            sb.append("<p>Ontology not loaded</p>");
        }

        return createManagePageRenderer(kit, getMap(server), sb.toString(), pageURL);
    }


    private NestedHTMLDoclet handleReload(OWLOntology ontology, OWLHTMLKit kit, URL pageURL) throws OntServerException {
        StringBuilder sb = new StringBuilder();

        OWLServer server = kit.getOWLServer();

        try {
            URI physicalLocation = server.getOWLOntologyManager().getPhysicalURIForOntology(ontology);

            server.removeOntology(ontology);
            server.loadOntology(physicalLocation);

            sb.append("<p>Removed ");
            sb.append(server.getOntologyShortFormProvider().getShortForm(ontology));
            sb.append("</p><p>saved session: [");
            sb.append(kit.getCurrentLabel());
            sb.append("]</p>");

        }
        catch (Exception e) {
            sb.append("Could not reload ontology: ");
            sb.append(server.getOntologyShortFormProvider().getShortForm(ontology));
            sb.append("<p>");
            sb.append(e.getMessage());
            sb.append("</p>");
        }

        return createManagePageRenderer(kit, getMap(server), sb.toString(), pageURL);
    }


    private NestedHTMLDoclet handleBrowse(OWLOntology ontology, OWLHTMLKit kit, URL pageURL) throws OntServerException {
        if (ontology != null){
            kit.getOWLServer().setActiveOntology(ontology);
            throw new RedirectException(kit.getBaseURL());
        }
        throw new OntServerException("Cannot browse to unknown ontology: " + ontology);
    }


    private boolean handleSetVisibility(OWLOntology ontology, boolean visible, OWLHTMLKit kit) {
        if (ontology != null){
            kit.setOntologyVisible(ontology, visible);
            return true;
        }
        return false;
    }


    private NestedHTMLDoclet createManagePageRenderer(OWLHTMLKit kit, Map<OWLOntologyID, URI> map, String message, URL pageURL) throws OntServerException {

        EmptyOWLDocPage ren = new EmptyOWLDocPage(kit);
        ren.setTitle(OntologyBrowserConstants.MANAGE_LABEL);
        ren.setAutoFocusedComponent(OntologyBrowserConstants.LOAD_ONTOLOGIES_INPUT_ID);

        Map<String, URI> bookmarks = getBookmarks(kit.getOWLServer());
        final LoadFormDoclet loadDoclet = new LoadFormDoclet();
        loadDoclet.addBookmarkSet("or Select a bookmark from below:", bookmarks);

        if (map.isEmpty()){
            ren.addDoclet(new BlurbDoclet());
            ren.addDoclet(loadDoclet);
        }
        else{
            if (map.containsValue(null)){
                if (message == null){
                    message = "";
                }
                String contentsURL = URLUtils.createRelativeURL(pageURL, kit.getURLScheme().getURLForRelativePage(OWLHTMLConstants.CONTENTS_HTML));
                message += ("<p class='warn'>There appear to be missing imports in your ontology.</p>" +
                            "<p>You can specify a location for any that have not been loaded in the following table.<br />" +
                            "Or, you can <a style='font-weight: bolder; color: blue;' target='_top' href='" + contentsURL +
                            "'>continue to browse</a> your ontology without loading the imports.</p>");
            }

            ren.addDoclet(loadDoclet);

            OntologyMappingsTableDoclet table = new OntologyMappingsTableDoclet(kit);
            table.setMap(map);
            ren.addDoclet(table);
        }

        if (message != null){
            ren.addMessage(message);
        }

        return ren;
    }


    public static Map<OWLOntologyID, URI> getMap(OWLServer server){
        Map<OWLOntologyID, URI> ontology2locationMap = new HashMap<OWLOntologyID, URI>();
        final Set<OWLOntology> ontologies = server.getOntologies();
        for (OWLOntology ont : ontologies){
            ontology2locationMap.put(ont.getOntologyID(), server.getOWLOntologyManager().getPhysicalURIForOntology(ont));
            for (OWLImportsDeclaration importsDecl : ont.getImportsDeclarations()){
                if (server.getOWLOntologyManager().getOntology(importsDecl.getIRI()) == null){
                    ontology2locationMap.put(new OWLOntologyID(importsDecl.getIRI()), null);
                }
            }
        }
        return ontology2locationMap;
    }


    private URI getURIFromParam(String param) throws URISyntaxException {
        String ontURIStr = param.trim();
        return new URI(ontURIStr);
    }

    private OWLOntology getOntologyFromParam(String param, OWLServer server) throws URISyntaxException {
        IRI iri = IRI.create(getURIFromParam(param));
        for (OWLOntology ontology : server.getActiveOntologies()){
            if (iri.equals(ontology.getOntologyID().getDefaultDocumentIRI())){
                return ontology;
            }
        }
        return null;
    }


    public Map<String, URI> getBookmarks(OWLServer server) {
        Map<String, URI> bookmarks = Collections.emptyMap();
        File bookmarksFile = SessionManager.getFile(OntologyBrowserConstants.BOOKMARKS_XML);
        if (!bookmarksFile.exists()){
            FileUtils fileUtils = new FileUtils("resources/", OWLHTMLConstants.DEFAULT_ENCODING); // path not used
            InputStream in = getClass().getResourceAsStream("default.bookmarks.xml");
            try {
                fileUtils.saveFile(in, bookmarksFile);
            }
            catch (IOException e) {
                logger.error(e);
            }
        }

        try {
            bookmarks = loadBookmarks(new BufferedReader(new FileReader(bookmarksFile)));
        }
        catch (Exception e) {
            logger.error(e);
        }
        return bookmarks;
    }


    private Map<String, URI> loadBookmarks(Reader reader) throws IOException, SAXException {
        Map<String, URI> bookmarkMap = new HashMap<String, URI>();
        DOMParser parser = new DOMParser();
        InputSource inputSource = new InputSource(reader);
        parser.parse(inputSource);
        Document doc = parser.getDocument();
        NodeList bookmarkElements = doc.getElementsByTagName("bookmark");
        for (int i=0; i<bookmarkElements.getLength(); i++){
            Node element = bookmarkElements.item(i);
            String name = element.getAttributes().getNamedItem("name").getTextContent();
            URI uri = URI.create(element.getTextContent());
            bookmarkMap.put(name, uri);
        }
        return bookmarkMap;
    }
}
