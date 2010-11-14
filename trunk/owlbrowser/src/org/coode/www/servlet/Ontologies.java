package org.coode.www.servlet;

import org.coode.html.OWLHTMLKit;
import org.coode.html.doclet.Doclet;
import org.coode.html.doclet.HTMLDoclet;
import org.coode.html.impl.OWLHTMLParam;
import org.coode.html.page.HTMLPage;
import org.coode.owl.mngr.OWLServer;
import org.coode.www.OntologyAction;
import org.coode.www.exception.OntServerException;
import org.coode.www.exception.RedirectException;
import org.coode.www.mngr.SessionManager;
import org.coode.www.page.OntologiesPage;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyID;

import java.net.MalformedURLException;
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

    protected Doclet handleXMLRequest(Map<OWLHTMLParam, String> params, OWLHTMLKit kit, URL pageURL) throws OntServerException {
        return null; //@@TODO implement
    }

    protected HTMLPage handleHTMLPageRequest(Map<OWLHTMLParam, String> params, OWLHTMLKit kit, URL pageURL) throws OntServerException {

        final String actionValue = params.get(OWLHTMLParam.action);

        if (actionValue == null){
            throw new RedirectException(kit.getURLScheme().getURLForOWLObject(kit.getOWLServer().getActiveOntology()));
        }

        try{
            OntologyAction action = OntologyAction.valueOf(actionValue);
            return handleAction(action, params, kit, pageURL);
        }
        catch(IllegalArgumentException e){
            throw new OntServerException("Unknown action: " + actionValue);
        }
    }

    @Override
    protected HTMLDoclet handleHTMLFragmentRequest(Map<OWLHTMLParam, String> params, OWLHTMLKit kit, URL pageURL) throws OntServerException {
        return null; //@@TODO implement
    }

    private HTMLPage handleAction(OntologyAction action, Map<OWLHTMLParam, String> params, OWLHTMLKit kit, URL pageURL) throws OntServerException {

        OWLServer server = kit.getOWLServer();
        try {
            switch(action){
                case load:
                    boolean clear = Boolean.parseBoolean(params.get(OWLHTMLParam.clear));
                    URL redirect = null;
                    if (params.get(OWLHTMLParam.redirect) != null){
                        try {
                            redirect = new URL(params.get(OWLHTMLParam.redirect));
                        }
                        catch (MalformedURLException e) {
                            logger.warn("Cannot redirect to " + params.get(OWLHTMLParam.redirect));
                        }
                    }
                    return handleLoad(getURIFromParam(params.get(OWLHTMLParam.uri)), clear, redirect, kit, pageURL);
                case remove:
                    return handleRemove(getOntologyFromParam(params.get(OWLHTMLParam.uri), server), kit, pageURL);
                case reload:
                    return handleReload(getOntologyFromParam(params.get(OWLHTMLParam.uri), server), kit, pageURL);
            }
        }
        catch (URISyntaxException e) {
            throw new OntServerException(e);
        }
        throw new RuntimeException("Missing action handler!!");
    }

    private HTMLPage handleLoad(URI uri, boolean clear, URL redirect, OWLHTMLKit kit, URL pageURL) throws OntServerException {
        Set<URI> success = new HashSet<URI>();
        Map<URI, Throwable> fail = new HashMap<URI, Throwable>();

//        String message = "";

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
                throw new IllegalArgumentException("Ontology URIs must be absolute: " + uri);
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


        for (URI f : fail.keySet()){
            kit.addUserError("failed to load: " + uri +
                             " ("  + fail.get(f).getClass().getSimpleName() +
                             ": " + fail.get(f).getMessage() + ")");
        }

        if (!success.isEmpty()){
            SessionManager.labelServerState(kit);
//            message += "<p>loaded " + success.size() + " ontologies</p>";
        }

        Map<OWLOntologyID, URI> map = server.getLocationsMap();

        if (map.isEmpty()){
            throw new RedirectException(kit.getURLScheme().getBaseURL());
        }

        if (map.containsValue(null)){ // missing value in map
            return new OntologiesPage(kit, pageURL);
        }

        throw new RedirectException(redirect != null ? redirect : kit.getURLScheme().getURLForOWLObject(server.getActiveOntology()));
    }


    private HTMLPage handleRemove(OWLOntology ontology, OWLHTMLKit kit, URL pageURL) throws OntServerException {
        StringBuilder sb = new StringBuilder();

        OWLServer server = kit.getOWLServer();

        if (ontology == null){
            sb.append("Could not remove ontology: null");
            sb.append("<p>Ontology not loaded</p>");
        }
        else{
            server.removeOntology(ontology);

            SessionManager.labelServerState(kit);
//            sb.append("<p>Removed ");
//            sb.append(server.getOntologyShortFormProvider().getShortForm(ontology));
//            sb.append("</p><p>saved session: [");
//            sb.append(kit.getCurrentLabel());
//            sb.append("]</p>");
        }

        return new OntologiesPage(kit, pageURL);
    }


    private HTMLPage handleReload(OWLOntology ontology, OWLHTMLKit kit, URL pageURL) throws OntServerException {
        StringBuilder sb = new StringBuilder();

        OWLServer server = kit.getOWLServer();

        final String ontName = server.getOntologyShortFormProvider().getShortForm(ontology);

        try {
            server.reloadOntology(ontology);

            sb.append("<p>Reloaded ").append(ontName).append("</p>");

        }
        catch (Exception e) {
            sb.append("Could not reload ontology: ").append(ontName);
            kit.addUserError(sb.toString(), e);
        }

        return new OntologiesPage(kit, pageURL);
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
