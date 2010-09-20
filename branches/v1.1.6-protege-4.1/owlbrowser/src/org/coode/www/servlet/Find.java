package org.coode.www.servlet;

import org.coode.html.OWLHTMLKit;
import org.coode.html.impl.OWLHTMLParam;
import org.coode.html.doclet.HTMLDoclet;
import org.coode.owl.mngr.NamedObjectType;
import org.coode.owl.mngr.OWLEntityFinder;
import org.coode.owl.mngr.OWLServer;
import org.coode.www.exception.OntServerException;
import org.coode.www.exception.RedirectException;
import org.semanticweb.owlapi.model.OWLNamedObject;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.IRI;

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


    protected void handleXMLRequest(Map<OWLHTMLParam, String> params, OWLHTMLKit kit, URL servletURL, PrintWriter out) throws OntServerException {
        Set<OWLEntity> results = getResults(params, kit.getOWLServer());
        renderXMLResults(results, kit.getOWLServer(), out);
    }


    protected HTMLDoclet handleHTMLRequest(Map<OWLHTMLParam, String> params, OWLHTMLKit kit, URL pageURL) throws OntServerException {
        Set<OWLEntity> results = getResults(params, kit.getOWLServer());
        if (results.size() == 1){
            // just go directly to that page
            OWLNamedObject result = results.iterator().next();
            throw new RedirectException(kit.getURLScheme().getURLForOWLObject(result));
        }
        else{
            // show a list of matches
            return createIndexRenderer("Find Results", results, kit);
        }
    }


    protected Map<OWLHTMLParam, Set<String>> getRequiredParams(OWLServer server) {
        Map<OWLHTMLParam, Set<String>> required = new HashMap<OWLHTMLParam, Set<String>>();
//        required.put(PARAM_TYPE, NamedObjectType.getRenderings()); // should this be optional and default to entities
//        required.put(PARAM_INPUT, Collections.singleton("<partial name>"));
        return required;
    }


    private Set<OWLEntity> getResults(Map<OWLHTMLParam, String> params, OWLServer server) throws OntServerException {

        String input = params.get(OWLHTMLParam.input);
        String uri = params.get(OWLHTMLParam.uri);
        String paramOntology = params.get(OWLHTMLParam.ontology);
        NamedObjectType type = NamedObjectType.valueOf(params.get(OWLHTMLParam.type));

        Set<OWLEntity> results = new HashSet<OWLEntity>();

        OWLEntityFinder finder = server.getFinder();
        OWLOntology ont = getOntology(paramOntology, server);

        if (uri != null){
            try{
                final URI entityURI = new URI(uri);
                if (entityURI.isAbsolute()){
                    results = new HashSet<OWLEntity>(finder.getOWLEntities(entityURI, type, ont));
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
                results.addAll(finder.getOWLEntities("^" + input.replace(WILDCARD, ".*"), type, ont));
            }

            if (results.isEmpty()){
                results.addAll(finder.getOWLEntities(".*" + input.replace(WILDCARD, ".*"), type, ont));
            }
        }
        return results;
    }

    private OWLOntology getOntology(String ontStr, OWLServer server) throws OntServerException {
        if (ontStr != null){
            try {
                // @@TODO handle anonymous ontologies?
                return server.getOWLOntologyManager().getOntology(IRI.create(new URI(ontStr)));
            }
            catch (URISyntaxException e) {
                logger.error("Cannot find ontology for: " + ontStr, e);
            }
        }
        return null;
    }
}
