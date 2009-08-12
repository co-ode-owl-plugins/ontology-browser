package org.coode.www.servlet;

import org.coode.html.OWLHTMLKit;
import org.coode.owl.mngr.ServerConstants;
import org.coode.www.exception.OntServerException;
import org.coode.www.mngr.SessionManager;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import java.io.IOException;

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
public class SignOut extends HttpServlet {

    protected void doPost(HttpServletRequest httpServletRequest, HttpServletResponse httpServletResponse) throws ServletException, IOException {
        signout(httpServletRequest, httpServletResponse);
    }

    protected void doGet(HttpServletRequest httpServletRequest, HttpServletResponse httpServletResponse) throws ServletException, IOException {
        signout(httpServletRequest, httpServletResponse);
    }

    private void signout(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {

        final String confirm = request.getParameter("confirm");

        if (confirm == null){ // no confirm
            StringBuffer requestURL = request.getRequestURL();
            response.getOutputStream().println(
                    "<html><body>" +
                    "This will clear all ontologies you are browsing. " +
                    "Are you sure you wish to restart? " +
                    "<a href='" + requestURL + "?confirm=true'>YES</a> " +
                    "<a href='" + requestURL + "?confirm=false'>No</a>" +
                    "</body></html>");
        }
        else{
            if (Boolean.getBoolean(confirm)){
                HttpSession session = request.getSession(false);
                SessionManager.closeSession(session);
            }

            try {
                OWLHTMLKit kit = SessionManager.getServer(request);
                response.sendRedirect(kit.getBaseURL().toString());
            }
            catch (OntServerException e) {
                throw new ServletException(e);
            }
        }
    }
}
