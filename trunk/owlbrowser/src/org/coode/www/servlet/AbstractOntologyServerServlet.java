package org.coode.www.servlet;

import org.apache.log4j.Logger;
import org.coode.html.OWLHTMLServer;
import org.coode.html.doclet.*;
import org.coode.html.impl.OWLHTMLConstants;
import org.coode.html.index.OWLEntityIndexHTMLPage;
import org.coode.html.page.DefaultHTMLPage;
import org.coode.html.page.EmptyOWLDocPage;
import org.coode.owl.mngr.NamedObjectType;
import org.coode.owl.mngr.OWLServer;
import org.coode.suggestor.api.SuggestorManager;
import org.coode.www.OntologyBrowserConstants;
import org.coode.www.doclet.AutocompleteDoclet;
import org.coode.www.exception.InvalidRequestException;
import org.coode.www.exception.OntServerException;
import org.coode.www.exception.RedirectException;
import org.coode.www.mngr.SessionManager;
import org.semanticweb.owl.model.OWLNamedObject;
import org.semanticweb.owl.model.OWLOntology;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.PrintWriter;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 * Author: Nick Drummond<br>
 * nick.drummond@cs.manchester.ac.uk<br>
 * http://www.cs.man.ac.uk/~drummond<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jun 29, 2007<br><br>
 * <p/>
 * code made available under Mozilla Public License (http://www.mozilla.org/MPL/MPL-1.1.html)<br>
 * copyright 2006, The University of Manchester<br>
 */
public abstract class AbstractOntologyServerServlet extends HttpServlet {

    protected Logger logger = Logger.getLogger(getClass().getName());

    protected abstract void handleXMLRequest(Map<String, String> params,
                                             OWLHTMLServer server, URL servletURL,
                                             PrintWriter out) throws OntServerException;


    protected abstract HTMLDoclet handleHTMLRequest(Map<String, String> params, OWLHTMLServer server, URL pageURL) throws OntServerException;


    protected final void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        doRequest(request, response);
    }

    protected final void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        doRequest(request, response);
    }

    protected abstract Map<String, Set<String>> getRequiredParams(OWLServer server);

    private void doRequest(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {

        final String sessionLabel = request.getParameter(OWLHTMLConstants.PARAM_SESSION_LABEL);
        final String format = request.getParameter(OntologyBrowserConstants.PARAM_FORMAT);

        final URL pageURL = buildRequestURL(request);

        final OWLHTMLServer server = getServer(request, sessionLabel, pageURL);

        try {
            final OWLOntology ont = server.getActiveOntology();

            if (ont == null && !pageURL.getFile().contains(OWLHTMLConstants.MANAGE_HTML)){
                response.sendRedirect(server.getURLScheme().getURLForRelativePage(OWLHTMLConstants.MANAGE_HTML).toString());
            }
            else{
                // the param map is actually multivalued <String, String[]>, but to save hassle we'll simplify it
                final Map<String, String> params = checkAndCreateParams(request, server);

                if (OntologyBrowserConstants.FORMAT_XML.equals(format)){
                    response.setContentType(OntologyBrowserConstants.MIME_XML);
                    PrintWriter out = response.getWriter();

                    handleXMLRequest(params, server, pageURL, out);
                }
                else {
                    response.setContentType(OntologyBrowserConstants.MIME_HTML);
                    HTMLDoclet ren = handleHTMLRequest(params, server, pageURL);

                    if (ren != null){
                        prepareMenuBar(ren, server, pageURL);
                        PrintWriter out = response.getWriter();
                        
                        ren.renderAll(pageURL, out);
                    }
                    else{
                        renderError("Could not get renderer for request", null, server, pageURL, format, response);
                    }
                }
            }
        }
        catch(RedirectException e){
            response.sendRedirect(e.getRedirectPage().toString());
        }
        catch (Throwable e) {
            logger.error(e);
            renderError(null, e, server, pageURL, format, response);
        }
        finally{
            response.getWriter().flush();
        }
    }


    private URL buildRequestURL(HttpServletRequest request) throws IOException {
        // requestURL on its own is not good enough - doesn't include params - need to rebuild the URL and fix AbstractSummaryHTMLPage
        StringBuilder requestURL = new StringBuilder(request.getRequestURL().toString());
        boolean appendedParams = false;

        String query = request.getQueryString();
        if (query != null){
            for (String param : query.split("&")){
                if (!param.startsWith(OWLHTMLConstants.PARAM_SESSION_LABEL)){
                    if (appendedParams){
                        requestURL.append("&");
                    }
                    else{
                        requestURL.append("?");
                        appendedParams = true;
                    }
                    requestURL.append(param);
                }
            }
        }
        return new URL(requestURL.toString());
    }


    private void prepareMenuBar(HTMLDoclet ren, OWLHTMLServer server, URL pageURL) {
        if (ren instanceof NestedHTMLDoclet){
            MenuBarDoclet menuDoclet = (MenuBarDoclet)((NestedHTMLDoclet)ren).getDoclet(MenuBarDoclet.ID);
            if (menuDoclet != null){

                // add the DL Query menuItem if the reasoner is enabled
                if (server.getProperties().isSet(OWLHTMLConstants.OPTION_REASONER_ENABLED)){
                    menuDoclet.addToMenu(new MenuItemDoclet(OWLHTMLConstants.DL_QUERY_LABEL,
                                                            server.getURLScheme().getURLForRelativePage(OWLHTMLConstants.DL_QUERY_HTML),
                                                            OWLHTMLConstants.LinkTarget.subnav,
                                                            server));
                }

                menuDoclet.addToMenu(new MenuItemDoclet("Help",
                                                        server.getURLScheme().getURLForRelativePage(OntologyBrowserConstants.DOCS_HTML),
                                                        OWLHTMLConstants.LinkTarget._blank,
                                                        server));

                AutocompleteDoclet searchboxDoclet = new AutocompleteDoclet(server, "find", true);
                searchboxDoclet.setParamName("name");
                searchboxDoclet.setSubmitName("find");
                searchboxDoclet.setSubmitURL(server.getURLScheme().getURLForIndex(NamedObjectType.entities)); // could be more direct
                searchboxDoclet.setTarget(OWLHTMLConstants.LinkTarget.content);
                menuDoclet.addDoclet(searchboxDoclet);
            }
        }
    }

    private OWLHTMLServer getServer(HttpServletRequest request, String sessionLabel, URL pageURL) throws ServletException {
        try {
            return SessionManager.getServer(request, sessionLabel);
        }
        catch (OntServerException e) {
            throw new ServletException("Severe exception initialising server session: " + pageURL, e);
        }
    }

    private Map<String, String> checkAndCreateParams(HttpServletRequest request, OWLServer server) throws InvalidRequestException {
        HashMap<String, String> params = new HashMap<String, String>();
        final Set requestParams = request.getParameterMap().keySet();
        final Map<String, Set<String>> requiredParams = getRequiredParams(server);

        for (String requiredParam : requiredParams.keySet()){
            if (!requestParams.contains(requiredParam)){
                throw new InvalidRequestException(request, requiredParams);
            }
        }
        for (Object param : requestParams){
            if (!OWLHTMLConstants.PARAM_SESSION_LABEL.equals(param) &&
                    !OntologyBrowserConstants.PARAM_FORMAT.equals(param)){
                String[] v = (String[])request.getParameterMap().get(param);
                params.put(param.toString(), v[0]);
            }
        }
        return params;
    }


    protected final SuggestorManager getSuggestorManager(OWLHTMLServer server) throws OntServerException {
        return SessionManager.getSuggestorManager(server);
    }

    private void renderError(String message, Throwable e, OWLHTMLServer server, URL servletURL, String format, HttpServletResponse response) throws IOException {
        if (OntologyBrowserConstants.FORMAT_XML.equals(format)){
            response.setContentType(OntologyBrowserConstants.MIME_XML);
            response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
            response.getWriter().println();
        }
        else if (OntologyBrowserConstants.FORMAT_HTML_FRAGMENT.equals(format)){
            DefaultHTMLPage errorRenderer = createHTMLError(message, e, server, response);
            errorRenderer.removeDoclet(errorRenderer.getDoclet(MenuBarDoclet.ID));
            errorRenderer.removeDoclet(errorRenderer.getDoclet(TabsDoclet.ID));
            errorRenderer.renderContent(servletURL, response.getWriter());
        }
        else { // default to full page HTML
            DefaultHTMLPage errorRenderer = createHTMLError(message, e, server, response);
            errorRenderer.renderAll(servletURL, response.getWriter());
        }
    }

    private DefaultHTMLPage createHTMLError(String message, Throwable e, OWLHTMLServer server, HttpServletResponse response) {
        response.setContentType(OntologyBrowserConstants.MIME_HTML);
        response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
        EmptyOWLDocPage errorRenderer = new EmptyOWLDocPage(server);
        if (message != null){
            errorRenderer.addError(message);
        }
        if (e != null){
            errorRenderer.addError(e);
        }
        return errorRenderer;
    }


    protected URL getURL(URL baseURL, String urlstr){
        try {
            return new URL(baseURL, urlstr);
        }
        catch (MalformedURLException e) {
            logger.error(e);
        }
        return null;
    }


    protected final <N extends OWLNamedObject> OWLEntityIndexHTMLPage createIndexRenderer(String title,
                                                                                          Set<N> results,
                                                                                          OWLHTMLServer server) throws OntServerException {
        try {
            OWLEntityIndexHTMLPage<N> ren = new OWLEntityIndexHTMLPage<N>(server);
            if (title != null){
                ren.setTitle(title);
            }
            ren.addAll(results);

            return ren;
        }
        catch (Exception e) {
            throw new OntServerException(e);
        }
    }


    protected final void renderXMLResults(Set<OWLNamedObject> results,
                                          OWLServer server,
                                          PrintWriter out) {
        out.println("<?xml version=\"1.0\" encoding=\"utf-8\" ?>");
        out.println("<results>");
        for (OWLNamedObject result : results){
            final String name = server.getNameRenderer().getShortForm(result);
            out.println("<rs id=\"" + result.getURI() + "\" info=\"\">" + name + "</rs>");
        }
        out.println("</results>");
    }


    // @@TODO this is a duplicate of the method in AbstractHTMLPageRenderer
    protected boolean isSingleFrameNavigation(OWLServer server) {
        return server.getProperties().get(OWLHTMLConstants.OPTION_CONTENT_WINDOW) == null;
    }


    public void destroy() {
        logger.debug("Servlet " + getClass() + " being destroyed");
        super.destroy();
    }
}
