package org.coode.www.servlet;

import org.coode.html.OWLHTMLKit;
import org.coode.html.doclet.HTMLDoclet;
import org.coode.html.doclet.NestedHTMLDoclet;
import org.coode.html.impl.OWLHTMLParam;
import org.coode.owl.mngr.NamedObjectType;
import org.coode.owl.mngr.OWLServer;
import org.coode.www.OntologyAction;
import org.coode.www.exception.OntServerException;
import org.coode.www.exception.RedirectException;
import org.coode.www.mngr.SessionManager;
import org.coode.www.page.OntologiesPage;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyID;

import java.io.PrintWriter;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

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
public class Ontologies extends AbstractOntologyServerServlet {

    protected void handleXMLRequest(Map<OWLHTMLParam, String> params, OWLHTMLKit kit, URL servletURL, PrintWriter out) throws OntServerException {
        // no implementation
    }

    protected HTMLDoclet handleHTMLRequest(Map<OWLHTMLParam, String> params, OWLHTMLKit kit, URL pageURL) throws OntServerException {

        final String actionValue = params.get(OWLHTMLParam.action);

        if (actionValue != null){
            try{
                OntologyAction action = OntologyAction.valueOf(actionValue);
                return handleAction(action, params, kit, pageURL);
            }
            catch(IllegalArgumentException e){
                throw new OntServerException("Unknown action: " + actionValue);
            }
        }

        return new OntologiesPage(kit, pageURL, null);
    }

    private NestedHTMLDoclet handleAction(OntologyAction action, Map<OWLHTMLParam, String> params, OWLHTMLKit kit, URL pageURL) throws OntServerException {

        OWLServer server = kit.getOWLServer();
        try {
            switch(action){
                case load:
                    boolean clear = Boolean.parseBoolean(params.get(OWLHTMLParam.clear));
                    return handleLoad(getURIFromParam(params.get(OWLHTMLParam.uri)), clear, kit, pageURL);
                case remove:
                    return handleRemove(getOntologyFromParam(params.get(OWLHTMLParam.uri), server), kit, pageURL);
                case reload:
                    return handleReload(getOntologyFromParam(params.get(OWLHTMLParam.uri), server), kit, pageURL);
//                    case hide:
//                        handleSetVisibility(getOntologyFromParam(params.get(OWLHTMLParam.uri), server), false, kit);
//                        break;
//                    case unhide:
//                        handleSetVisibility(getOntologyFromParam(params.get(OWLHTMLParam.uri), server), true, kit);
//                        break;
            }
        }
        catch (URISyntaxException e) {
            throw new OntServerException(e);
        }
        throw new RuntimeException("Missing action handler!!");
    }


    protected Map<OWLHTMLParam, Set<String>> getRequiredParams(OWLServer server) {
        Map<OWLHTMLParam, Set<String>> required = new HashMap<OWLHTMLParam, Set<String>>();
//        required.put(PARAM_URI, Collections.singleton("<ontology uri>")); optional
//        required.put(PARAM_ACTION, getActionRenderings());
        return required;
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
            e.printStackTrace();
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
        }

        Map<OWLOntologyID, URI> map = server.getLocationsMap();

        if (map.isEmpty() || map.containsValue(null)){ // empty or missing value in map
            return new OntologiesPage(kit, pageURL, message);
        }
        else if (map.size() == 1){
            throw new RedirectException(kit.getURLScheme().getURLForOWLObject(server.getActiveOntology()));
        }
        else{
            throw new RedirectException(kit.getURLScheme().getURLForIndex(NamedObjectType.ontologies));
        }
    }


    private NestedHTMLDoclet handleRemove(OWLOntology ontology, OWLHTMLKit kit, URL pageURL) throws OntServerException {
        StringBuilder sb = new StringBuilder();

        OWLServer server = kit.getOWLServer();

        if (ontology == null){
            sb.append("Could not remove ontology: null");
            sb.append("<p>Ontology not loaded</p>");
        }
        else{
            server.removeOntology(ontology);

            SessionManager.labelServerState(kit);
            sb.append("<p>Removed ");
            sb.append(server.getOntologyShortFormProvider().getShortForm(ontology));
            sb.append("</p><p>saved session: [");
            sb.append(kit.getCurrentLabel());
            sb.append("]</p>");
        }

        return new OntologiesPage(kit, pageURL, sb.toString());
    }


    private NestedHTMLDoclet handleReload(OWLOntology ontology, OWLHTMLKit kit, URL pageURL) throws OntServerException {
        StringBuilder sb = new StringBuilder();

        OWLServer server = kit.getOWLServer();

        final String ontName = server.getOntologyShortFormProvider().getShortForm(ontology);

        try {
            URI physicalLocation = server.getOWLOntologyManager().getOntologyDocumentIRI(ontology).toURI();

            server.removeOntology(ontology);
            server.loadOntology(physicalLocation);

            sb.append("<p>Reloaded ").append(ontName).append("</p>");

        }
        catch (Exception e) {
            sb.append("Could not reload ontology: ");
            sb.append(ontName);
            sb.append("<p>").append(e.getMessage()).append("</p>");
        }

        return new OntologiesPage(kit, pageURL, sb.toString());
    }

    private boolean handleSetVisibility(OWLOntology ontology, boolean visible, OWLHTMLKit kit) {
        if (ontology != null){
            kit.setOntologyVisible(ontology, visible);
            return true;
        }
        return false;
    }

    private URI getURIFromParam(String param) throws URISyntaxException {
        String ontURIStr = param.trim();
        return new URI(ontURIStr);
    }

    private OWLOntology getOntologyFromParam(String param, OWLServer server) throws URISyntaxException {
        IRI iri = IRI.create(getURIFromParam(param));
        return server.getOntologyForIRI(iri);
    }
}
