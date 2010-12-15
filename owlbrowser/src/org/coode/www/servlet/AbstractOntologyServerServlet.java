package org.coode.www.servlet;

import org.apache.log4j.Logger;
import org.coode.html.OWLHTMLKit;
import org.coode.html.doclet.Doclet;
import org.coode.html.doclet.HTMLDoclet;
import org.coode.html.doclet.MessageBoxDoclet;
import org.coode.html.impl.OWLHTMLConstants;
import org.coode.html.impl.OWLHTMLParam;
import org.coode.html.page.HTMLPage;
import org.coode.html.page.OWLDocPage;
import org.coode.html.url.PermalinkURLScheme;
import org.coode.owl.mngr.OWLServer;
import org.coode.owl.mngr.ServerConstants;
import org.coode.www.OntologyBrowserConstants;
import org.coode.www.ParametersBuilder;
import org.coode.www.ServletUtils;
import org.coode.www.doclet.MenuBarDoclet;
import org.coode.www.doclet.TitleDoclet;
import org.coode.www.exception.OntServerException;
import org.coode.www.exception.RedirectException;
import org.coode.www.mngr.SessionManager;

import javax.servlet.ServletException;
import javax.servlet.http.*;
import java.io.IOException;
import java.net.URL;
import java.util.Collections;
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

    private OntologyBrowserConstants.RequestFormat format = OntologyBrowserConstants.RequestFormat.html;

    private HttpSession httpSession = null;

    protected abstract Doclet handleXMLRequest(Map<OWLHTMLParam, String> params,
                                               OWLHTMLKit kit,
                                               URL pageURL) throws OntServerException;

    protected abstract HTMLPage handleHTMLPageRequest(Map<OWLHTMLParam, String> params,
                                                      OWLHTMLKit kit,
                                                      URL pageURL) throws OntServerException;

    protected abstract HTMLDoclet handleHTMLFragmentRequest(Map<OWLHTMLParam, String> params,
                                                            OWLHTMLKit kit,
                                                            URL pageURL) throws OntServerException;

    protected final void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
//        System.out.println("AbstractOntologyServerServlet.doGet");
        doRequest(request, response);
    }

    protected final void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
//        System.out.println("AbstractOntologyServerServlet.doPost");
        doRequest(request, response);
    }

    public Map<OWLHTMLParam, Set<String>> getRequiredParams(OWLServer server){
        return Collections.emptyMap();
    }

    private void doRequest(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {

        URL redirect = null;

        handleRequestType(request, response);

        httpSession = request.getSession(false);

        final URL pageURL = ServletUtils.getPageURL(request);

        final String label = getLabel(request);

        final OWLHTMLKit kit = getOWLHTMLKit(request, label, pageURL);

        try {
            if (!kit.isActive()){
                throw new OntServerException("<p>This session has timed out.</p>" +
                                             "<p>Press back in your browser and select a permalink to restore.</p>" +
                                             "<p>TIP: Enable cookies in your browser to avoid this happening again.</p>");
            }

            // the param map is actually multivalued <String, String[]>, but to save hassle we'll simplify it
            final Map<OWLHTMLParam, String> params = new ParametersBuilder().checkAndCreateParams(request, kit, this);

            Doclet ren = getResults(params, kit, pageURL);
            if (ren != null){
                ren.renderAll(pageURL, response.getWriter());
            }
            else{
                throw new OntServerException("Could not get renderer for request: " + pageURL);
            }
        }
        catch(RedirectException e){
            redirect = e.getRedirectPage();
        }
        catch (Throwable e) {
            handleError(e, kit, pageURL, response);
        }
        finally{
            if (ServerConstants.COOKIE_SESSION_RECOVERY){
                updateCookies(request, response, kit);
            }
            if (redirect != null){
                handleRedirect(redirect, response);
            }
            response.getWriter().flush();
        }
    }

    private void updateCookies(HttpServletRequest request, HttpServletResponse response, OWLHTMLKit kit) {
        if (kit.isActive() && kit.getCurrentLabel() != null){
            Cookie cookie = new Cookie(OntologyBrowserConstants.LABEL_COOKIE_NAME, kit.getCurrentLabel());
            cookie.setPath(request.getContextPath() + "/");
            cookie.setMaxAge(-1); // until session expires
            response.addCookie(cookie);
//                    System.out.println("Setting cookie " + kit.getCurrentLabel());
        }
        else {
            final Cookie[] cookies = request.getCookies();
            if (cookies != null){
                for (Cookie cookie : cookies){
                    if (cookie.getName().equals(OntologyBrowserConstants.LABEL_COOKIE_NAME)){
//                            System.out.println("clearing cookie " + cookie.getValue());
                        cookie = new Cookie(OntologyBrowserConstants.LABEL_COOKIE_NAME, "");
                        cookie.setPath(request.getContextPath() + "/");
                        cookie.setMaxAge(0);
                        response.addCookie(cookie);
                    }
                }
            }
        }
    }

    private String getLabel(HttpServletRequest request) {
        // first check if a label is given in the URL
        String label = getParameter(request, OWLHTMLParam.session);
        if (label != null){
            return label;
        }

        if (ServerConstants.COOKIE_SESSION_RECOVERY){
            // and if not, check the cookies
            Cookie[] cookies = request.getCookies();
            if (cookies != null){
                for (Cookie cookie : cookies){
                    if (cookie.getName().equals(OntologyBrowserConstants.LABEL_COOKIE_NAME)){
//                        System.out.println("recovering session from cookie: " + cookie.getValue());
                        return cookie.getValue();
                    }
                }
            }
        }
        return null;
    }

    private void handleRedirect(URL redirectPage, HttpServletResponse response) throws IOException {
        response.setCharacterEncoding(OWLHTMLConstants.DEFAULT_ENCODING);
        response.sendRedirect(redirectPage.toString());
    }

    private void handleRequestType(HttpServletRequest request, HttpServletResponse response){

        // if the format param is specified, use it
        String str = getParameter(request, OWLHTMLParam.format);
        if (str != null){
            if (OntologyBrowserConstants.HTML_FRAG.equals(str)){ // convert old requests
                format = OntologyBrowserConstants.RequestFormat.htmlfrag;
            }
            else{
                try{
                    format = OntologyBrowserConstants.RequestFormat.valueOf(str);
                }
                catch (Exception e){
                    // do nothing
                }
            }
        }

        // get the request type from the request header
        String mime = request.getHeader(OntologyBrowserConstants.ACCEPT);
        if (mime != null){
            format = OntologyBrowserConstants.RequestFormat.get(mime);
        }

        // default to html
        if (format == null){
            format = OntologyBrowserConstants.RequestFormat.html;
        }

        response.setContentType(format.getResponseType());
    }

    private Doclet getResults(Map<OWLHTMLParam, String> params, OWLHTMLKit kit, URL pageURL) throws OntServerException {
        switch(format){
            case xml:
                return handleXMLRequest(params, kit, pageURL);
            case htmlfrag:
                return handleHTMLFragmentRequest(params, kit, pageURL);
            case html:
                HTMLPage page = handleHTMLPageRequest(params, kit, pageURL);
                if (page != null){
                    wrap(page, kit);
                }
                return page;
        }
        return null;
    }

    private void wrap(HTMLPage ren, OWLHTMLKit kit){
        ren.addDoclet(new TitleDoclet(kit), 0);
        if (kit.isActive()){
            ren.addDoclet(new MenuBarDoclet(kit), 1);
        }
        ren.addOnLoad("baseURL=\"" + kit.getURLScheme().getBaseURL() + "\";");
    }

    protected void handleError(Throwable e, OWLHTMLKit kit, URL pageURL, HttpServletResponse response) throws IOException {

        e.printStackTrace();

        response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);

        switch(format){
            case xml:
                response.getWriter().println();
                break;
            case htmlfrag:
                MessageBoxDoclet errorDoclet = new MessageBoxDoclet("Error", e.getMessage());
                errorDoclet.renderAll(pageURL, response.getWriter());
                break;
            case html:
                kit.addUserError("<p>Error rendering page</p>" +
                                 "<p>Please send the following address to the developers</p><pre>" +
                                 new PermalinkURLScheme(kit.getURLScheme()).getURLForAbsolutePage(pageURL) + "</pre>", e);
                OWLDocPage errorPage = new OWLDocPage(kit);
                wrap(errorPage, kit);
                errorPage.renderAll(pageURL, response.getWriter());
                break;
        }
    }

    protected HttpSession getHttpSession() {
        return httpSession;
    }

    protected String getParameter(HttpServletRequest request, OWLHTMLParam param) {
        return request.getParameter(param.name());
    }

    private OWLHTMLKit getOWLHTMLKit(HttpServletRequest request, String sessionLabel, URL pageURL) throws ServletException {
        try {
            return SessionManager.getHTMLKit(request, sessionLabel);
        }
        catch (OntServerException e) {
            throw new ServletException("Severe exception initialising kit session: " + pageURL, e);
        }
    }
}