package org.coode.www.servlet;

import org.coode.html.OWLHTMLServer;
import org.coode.html.doclet.HTMLDoclet;
import org.coode.owl.mngr.NamedObjectType;
import org.coode.owl.mngr.OWLNamedObjectFinder;
import org.coode.owl.mngr.OWLServer;
import org.coode.www.exception.OntServerException;
import org.coode.www.exception.RedirectException;
import org.semanticweb.owl.model.OWLNamedObject;
import org.semanticweb.owl.model.OWLOntology;

import java.io.PrintWriter;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * Author: Nick Drummond<br>
 * nick.drummond@cs.manchester.ac.uk<br>
 * http://www.cs.man.ac.uk/~drummond<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jun 20, 2007<br><br>
 * <p/>
 * code made available under Mozilla Public License (http://www.mozilla.org/MPL/MPL-1.1.html)<br>
 * copyright 2006, The University of Manchester<br>
 *
 * Service for getting the named entities matching a certain pattern.
 * This can be hooked up to the autocompleter
 *
 * type - the type of the named thing you are looking up (class, objectproperty etc) - specified in ServerConstants
 * input - the string pattern to match against (regexp, but with * as wildcard)
 * format - xml or html results
 */
public class Find extends AbstractOntologyServerServlet {

    private static final String WILDCARD = "*";


    private static final String PARAM_TYPE = "type";
    private static final String PARAM_INPUT = "input";
    private static final String PARAM_URI = "uri";
    private static final String PARAM_ONTOLOGY = "ontology";


    protected void handleXMLRequest(Map<String, String> params, OWLHTMLServer server, URL servletURL, PrintWriter out) throws OntServerException {
        Set<OWLNamedObject> results = getResults(params, server);
        renderXMLResults(results, server, out);
    }


    protected HTMLDoclet handleHTMLRequest(Map<String, String> params, OWLHTMLServer server, URL pageURL) throws OntServerException {
        Set<OWLNamedObject> results = getResults(params, server);
        if (results.size() == 1){
            // just go directly to that page
            OWLNamedObject result = results.iterator().next();
            throw new RedirectException(server.getURLScheme().getURLForNamedObject(result));
        }
        else{
            // show a list of matches
            return createIndexRenderer("Find Results", results, server);
        }
    }


    protected Map<String, Set<String>> getRequiredParams(OWLServer server) {
        Map<String, Set<String>> required = new HashMap<String, Set<String>>();
//        required.put(PARAM_TYPE, NamedObjectType.getRenderings()); // should this be optional and default to entities
//        required.put(PARAM_INPUT, Collections.singleton("<partial name>"));
        return required;
    }


    private Set<OWLNamedObject> getResults(Map<String, String> params, OWLHTMLServer server) throws OntServerException {

        String input = params.get(PARAM_INPUT);
        String uri = params.get(PARAM_URI);
        String paramOntology = params.get(PARAM_ONTOLOGY);
        NamedObjectType type = NamedObjectType.valueOf(params.get(PARAM_TYPE));

        Set<OWLNamedObject> results = new HashSet<OWLNamedObject>();

        OWLNamedObjectFinder finder = server.getFinder();
        OWLOntology ont = getOntology(paramOntology, server);

        if (uri != null){
            try{
                final URI entityURI = new URI(uri);
                if (entityURI.isAbsolute()){
                    results = new HashSet<OWLNamedObject>(finder.getOWLNamedObjects(entityURI, type, ont));
                }
            }
            catch(URISyntaxException e){
                logger.error("Could not find anything called: " + uri);
            }
        }
        else{
            if (!input.endsWith(WILDCARD)){
                input = input + WILDCARD;
            }
            if (input.length() > 0){
                results.addAll(finder.getOWLNamedObjects("^" + input.replace(WILDCARD, ".*"), type, ont));
            }

            if (results.isEmpty()){
                results.addAll(finder.getOWLNamedObjects(".*" + input.replace(WILDCARD, ".*"), type, ont));
            }
        }
        return results;
    }

    private OWLOntology getOntology(String ontStr, OWLHTMLServer server) throws OntServerException {
        if (ontStr != null){
            try {
                return server.getOWLOntologyManager().getOntology(new URI(ontStr));
            }
            catch (URISyntaxException e) {
                logger.error("Cannot find ontology for: " + ontStr, e);
            }
        }
        return null;
    }
}
