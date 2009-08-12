package org.coode.www.servlet;

import org.apache.log4j.Logger;
import org.coode.html.OWLHTMLKit;
import org.coode.html.doclet.*;
import org.coode.html.impl.OWLHTMLConstants;
import org.coode.html.impl.OWLHTMLProperty;
import org.coode.html.impl.OWLHTMLParam;
import org.coode.html.index.OWLObjectIndexHTMLPage;
import org.coode.html.page.DefaultHTMLPage;
import org.coode.html.page.EmptyOWLDocPage;
import org.coode.owl.mngr.NamedObjectType;
import org.coode.owl.mngr.OWLServer;
import org.coode.www.OntologyBrowserConstants;
import org.coode.www.doclet.AutocompleteDoclet;
import org.coode.www.exception.InvalidRequestException;
import org.coode.www.exception.OntServerException;
import org.coode.www.exception.RedirectException;
import org.coode.www.mngr.SessionManager;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLObject;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLDecoder;
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

    private String format = null;

    protected abstract void handleXMLRequest(Map<OWLHTMLParam, String> params,
                                             OWLHTMLKit kit, URL servletURL,
                                             PrintWriter out) throws OntServerException;


    protected abstract HTMLDoclet handleHTMLRequest(Map<OWLHTMLParam, String> params, OWLHTMLKit kit, URL pageURL) throws OntServerException;


    protected final void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        doRequest(request, response);
    }

    protected final void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        doRequest(request, response);
    }

    protected abstract Map<OWLHTMLParam, Set<String>> getRequiredParams(OWLServer server);

    private void doRequest(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {

        final String sessionLabel = getParameter(request, OWLHTMLParam.session);
        format = getParameter(request, OWLHTMLParam.format);

        final URL pageURL = buildRequestURL(request);

        final OWLHTMLKit kit = getHTMLGenerator(request, sessionLabel, pageURL);

        try {
            final OWLOntology ont = kit.getOWLServer().getActiveOntology();

            if (ont == null && !pageURL.getFile().contains(OWLHTMLConstants.MANAGE_HTML)){
                response.sendRedirect(kit.getURLScheme().getURLForRelativePage(OWLHTMLConstants.MANAGE_HTML).toString());
            }
            else{
                // the param map is actually multivalued <String, String[]>, but to save hassle we'll simplify it
                final Map<OWLHTMLParam, String> params = checkAndCreateParams(request, kit);

                if (OntologyBrowserConstants.FORMAT_XML.equals(format)){
                    response.setContentType(OntologyBrowserConstants.MIME_XML);
                    handleXMLRequest(params, kit, pageURL, response.getWriter());
                }
                else {
                    response.setContentType(OntologyBrowserConstants.MIME_HTML);
                    HTMLDoclet ren = handleHTMLRequest(params, kit, pageURL);

                    if (ren != null){
                        prepareMenuBar(ren, kit, pageURL);
                        ren.renderAll(pageURL, response.getWriter());
                    }
                    else{
                        renderError("Could not get renderer for request", null, kit, pageURL, format, response);
                    }
                }
            }
        }
        catch(RedirectException e){
            response.setCharacterEncoding(OWLHTMLConstants.DEFAULT_ENCODING);
            String redir = e.getRedirectPage().toString();
            response.sendRedirect(redir);
        }
        catch (Throwable e) {
            e.printStackTrace();
//            logger.error(e);
            renderError(null, e, kit, pageURL, format, response);
        }
        finally{
            response.getWriter().flush();
        }
    }


    protected String getParameter(HttpServletRequest request, OWLHTMLParam param) {
        return request.getParameter(param.name());
    }


    protected String getReturnFormat(){
        return format;
    }


    private URL buildRequestURL(HttpServletRequest request) throws IOException {
        // requestURL on its own is not good enough - doesn't include params - need to rebuild the URL and fix AbstractOWLEntitySummaryHTMLPage
        StringBuilder requestURL = new StringBuilder(request.getRequestURL().toString());
        boolean appendedParams = false;

        String query = request.getQueryString();
        if (query != null){
            for (String param : query.split("&")){
                if (!param.startsWith(OWLHTMLParam.session.name())){
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


    private void prepareMenuBar(HTMLDoclet ren, OWLHTMLKit kit, URL pageURL) {
        if (ren instanceof NestedHTMLDoclet){
            MenuBarDoclet menuDoclet = (MenuBarDoclet)((NestedHTMLDoclet)ren).getDoclet(MenuBarDoclet.ID);
            if (menuDoclet != null){

                // add the DL Query menuItem if the reasoner is enabled
                if (kit.getHTMLProperties().isSet(OWLHTMLProperty.optionReasonerEnabled)){
                    menuDoclet.addToMenu(new MenuItemDoclet(OWLHTMLConstants.DL_QUERY_LABEL,
                                                            kit.getURLScheme().getURLForRelativePage(OWLHTMLConstants.DL_QUERY_HTML),
                                                            OWLHTMLConstants.LinkTarget.subnav,
                                                            kit));
                }

                menuDoclet.addToMenu(new MenuItemDoclet("Help",
                                                        OWLHTMLConstants.HOME_PAGE,
                                                        OWLHTMLConstants.LinkTarget._blank,
                                                        kit));

                AutocompleteDoclet searchboxDoclet = new AutocompleteDoclet(kit, "find", true);
                searchboxDoclet.setParamName("name");
                searchboxDoclet.setSubmitName("find");
                searchboxDoclet.setSubmitURL(kit.getURLScheme().getURLForIndex(NamedObjectType.entities)); // could be more direct
                searchboxDoclet.setTarget(OWLHTMLConstants.LinkTarget.content);
                menuDoclet.addDoclet(searchboxDoclet);
            }
        }
    }

    private OWLHTMLKit getHTMLGenerator(HttpServletRequest request, String sessionLabel, URL pageURL) throws ServletException {
        try {
            return SessionManager.getServer(request, sessionLabel);
        }
        catch (OntServerException e) {
            throw new ServletException("Severe exception initialising kit session: " + pageURL, e);
        }
    }

    private Map<OWLHTMLParam, String> checkAndCreateParams(HttpServletRequest request,
                                                           OWLHTMLKit kit) throws InvalidRequestException {
        Map<OWLHTMLParam, String> params = new HashMap<OWLHTMLParam, String>();

        final Map<OWLHTMLParam, Set<String>> requiredParams = getRequiredParams(kit.getOWLServer());

        // check the parameters are known
        for (Object key: request.getParameterMap().keySet()){
            try{
                params.put(OWLHTMLParam.valueOf((String)key), null);
            }
            catch (IllegalArgumentException e){
                throw new InvalidRequestException(request, requiredParams);
            }
        }

        // check that the required parameters are given
        for (OWLHTMLParam requiredParam : requiredParams.keySet()){
            if (!params.keySet().contains(requiredParam)){
                throw new InvalidRequestException(request, requiredParams);
            }
        }


        for (OWLHTMLParam param : params.keySet()){
            switch(param){
                case session: break;
                case format: break;
                default:
                String[] v = (String[])request.getParameterMap().get(param.name());
                try {
                    String value = v[0];
                    // hack to ensure that params are decoded (if not already uri escaped)
                    if (request.getCharacterEncoding() == null && !value.startsWith("%")){
                        value = new String(value.getBytes("8859_1"), OWLHTMLConstants.DEFAULT_ENCODING);
                    }
                    value = URLDecoder.decode(value, OWLHTMLConstants.DEFAULT_ENCODING);
                    params.put(param, value);
                }
                catch (UnsupportedEncodingException e) {
                    throw new RuntimeException(e);
                }
            }
        }
        return params;
    }


//    protected final SuggestorManager getSuggestorManager(OWLHTMLKit kit) throws OntServerException {
//        return SessionManager.getSuggestorManager(kit);
//    }

    private void renderError(String message, Throwable e, OWLHTMLKit kit, URL servletURL, String format, HttpServletResponse response) throws IOException {
        if (OntologyBrowserConstants.FORMAT_XML.equals(format)){
            response.setContentType(OntologyBrowserConstants.MIME_XML);
            response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
            response.getWriter().println();
        }
        else if (OntologyBrowserConstants.FORMAT_HTML_FRAGMENT.equals(format)){
            DefaultHTMLPage errorRenderer = createHTMLError(message, e, kit, response);
            errorRenderer.removeDoclet(errorRenderer.getDoclet(MenuBarDoclet.ID));
            errorRenderer.removeDoclet(errorRenderer.getDoclet(TabsDoclet.ID));
            errorRenderer.renderContent(servletURL, response.getWriter());
        }
        else { // default to full page HTML
            EmptyOWLDocPage errorRenderer = createHTMLError(message, e, kit, response);
            errorRenderer.addMessage("Error rendering page: " + servletURL);            
            errorRenderer.renderAll(servletURL, response.getWriter());
        }
    }

    private EmptyOWLDocPage createHTMLError(String message, Throwable e, OWLHTMLKit kit, HttpServletResponse response) {
        response.setContentType(OntologyBrowserConstants.MIME_HTML);
        response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
        EmptyOWLDocPage errorRenderer = new EmptyOWLDocPage(kit);
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


    protected final <N extends OWLObject> OWLObjectIndexHTMLPage createIndexRenderer(String title,
                                                                                          Set<N> results,
                                                                                          OWLHTMLKit kit) throws OntServerException {
        try {
            OWLObjectIndexHTMLPage<N> ren = new OWLObjectIndexHTMLPage<N>(kit);
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


    protected final void renderXMLResults(Set<? extends OWLObject> results,
                                          OWLServer server,
                                          PrintWriter out) {
        out.println("<?xml version=\"1.0\" encoding=\"" + OWLHTMLConstants.DEFAULT_ENCODING + "\" ?>");
        out.println("<results>");
        for (OWLObject result : results){
            if (result instanceof OWLEntity){
                OWLEntity entity = (OWLEntity)result;
                final String name = server.getShortFormProvider().getShortForm(entity);
                out.println("<rs id=\"" + entity.getURI() + "\" info=\"\">" + name + "</rs>");
            }
            else if (result instanceof OWLOntology){
                OWLOntology ont = (OWLOntology)result;
                final String name = server.getOntologyShortFormProvider().getShortForm(ont);
                out.println("<rs id=\"" + ont.getOntologyID().getOntologyIRI() + "\" info=\"\">" + name + "</rs>");
            }
        }
        out.println("</results>");
    }


    // @@TODO this is a duplicate of the method in AbstractHTMLPageRenderer
    protected boolean isSingleFrameNavigation(OWLHTMLKit kit) {
        return kit.getHTMLProperties().get(OWLHTMLProperty.optionContentWindow) == null;
    }


    public void destroy() {
        logger.debug("Servlet " + getClass() + " being destroyed");
        super.destroy();
    }
}