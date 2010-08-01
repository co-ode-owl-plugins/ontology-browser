package org.coode.www.servlet;

import org.apache.log4j.Logger;
import org.coode.html.OWLHTMLKit;
import org.coode.html.doclet.HTMLDoclet;
import org.coode.html.doclet.TabsDoclet;
import org.coode.html.impl.OWLHTMLConstants;
import org.coode.html.impl.OWLHTMLParam;
import org.coode.html.impl.OWLHTMLProperty;
import org.coode.html.index.OWLObjectIndexHTMLPage;
import org.coode.html.page.DefaultHTMLPage;
import org.coode.html.page.OWLDocPage;
import org.coode.owl.mngr.OWLServer;
import org.coode.www.OntologyBrowserConstants;
import org.coode.www.ServletUtils;
import org.coode.www.doclet.MenuBarDoclet;
import org.coode.www.exception.InvalidRequestException;
import org.coode.www.exception.OntServerException;
import org.coode.www.exception.RedirectException;
import org.coode.www.mngr.SessionManager;
import org.coode.www.page.BrowserPageAdapter;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLOntology;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
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

    private HttpSession session = null;

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
        session = request.getSession(false);

        final URL pageURL = ServletUtils.rebuildRequestURL(request);

        final OWLHTMLKit kit = getOWLHTMLKit(request, sessionLabel, pageURL);

        try {
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
                    if (ren instanceof DefaultHTMLPage){
                        ren = new BrowserPageAdapter((DefaultHTMLPage)ren, kit, pageURL);
                    }
                    ren.renderAll(pageURL, response.getWriter());
                }
                else{
                    throw new OntServerException("Could not get renderer for request");
                }
            }
        }
        catch(RedirectException e){
            response.setCharacterEncoding(OWLHTMLConstants.DEFAULT_ENCODING);
            String redir = e.getRedirectPage().toString();
            response.sendRedirect(redir);
        }
        catch (Throwable e) {
            handleError(e, kit, pageURL, response);
        }
        finally{
            response.getWriter().flush();
        }
    }

    protected void handleError(Throwable e, OWLHTMLKit kit, URL pageURL, HttpServletResponse response) throws IOException {
        e.printStackTrace();
        if (OntologyBrowserConstants.FORMAT_XML.equals(format)){
            response.setContentType(OntologyBrowserConstants.MIME_XML);
            response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
            response.getWriter().println();
        }
        else if (OntologyBrowserConstants.FORMAT_HTML_FRAGMENT.equals(format)){
            DefaultHTMLPage errorRenderer = createHTMLError(e.getMessage(), e, kit, response);
            errorRenderer.removeDoclet(errorRenderer.getDoclet(MenuBarDoclet.ID));
            errorRenderer.removeDoclet(errorRenderer.getDoclet(TabsDoclet.ID));
            errorRenderer.renderContent(pageURL, response.getWriter());
        }
        else { // default to full page HTML
            OWLDocPage errorRenderer = createHTMLError(e.getMessage(), e, kit, response);
            errorRenderer.addMessage("Error rendering page: " + pageURL);
            errorRenderer.renderAll(pageURL, response.getWriter());
        }
    }

    protected HttpSession getSession() {
        return session;
    }

    protected String getParameter(HttpServletRequest request, OWLHTMLParam param) {
        return request.getParameter(param.name());
    }


    protected String getReturnFormat(){
        return format;
    }

    private OWLHTMLKit getOWLHTMLKit(HttpServletRequest request, String sessionLabel, URL pageURL) throws ServletException {
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
                case uri:// eg people+pets.owl gets corrupted otherwise
                    String[] v1 = (String[])request.getParameterMap().get(param.name());
                    try {
                        String value = v1[0];
                        // hack to ensure that params are decoded (if not already uri escaped)
                        if (request.getCharacterEncoding() == null && !value.startsWith("%")){
                            value = new String(value.getBytes("8859_1"), OWLHTMLConstants.DEFAULT_ENCODING);
                        }
                        params.put(param, value);
                    }
                    catch (UnsupportedEncodingException e) {
                        throw new RuntimeException(e);
                    }
                    break;
                default:
                    String[] v2 = (String[])request.getParameterMap().get(param.name());
                    try {
                        String value = v2[0];
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

    private OWLDocPage createHTMLError(String message, Throwable e, OWLHTMLKit kit, HttpServletResponse response) {
        response.setContentType(OntologyBrowserConstants.MIME_HTML);
        response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
        OWLDocPage errorRenderer = new OWLDocPage(kit);
        if (message != null){
            errorRenderer.addError(message);
        }
        if (e != null){
            errorRenderer.addError(e);
        }
        return errorRenderer;
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
                out.println("<rs id=\"" + entity.getIRI() + "\" info=\"\">" + name + "</rs>");
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