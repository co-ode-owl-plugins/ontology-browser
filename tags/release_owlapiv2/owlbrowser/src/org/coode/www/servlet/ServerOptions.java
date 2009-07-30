package org.coode.www.servlet;

import org.coode.html.OWLHTMLServer;
import org.coode.html.doclet.HTMLDoclet;
import org.coode.html.impl.OWLHTMLConstants;
import org.coode.html.page.EmptyOWLDocPage;
import org.coode.owl.mngr.OWLServer;
import org.coode.owl.mngr.ServerConstants;
import org.coode.owl.mngr.ServerProperties;
import org.coode.www.doclet.OptionsTableDoclet;
import org.coode.www.exception.OntServerException;
import org.coode.www.exception.RedirectException;
import org.coode.www.mngr.SessionManager;

import java.io.PrintWriter;
import java.net.URL;
import java.util.Collections;
import java.util.Map;
import java.util.Set;
/*
* Copyright (C) 2007, University of Manchester
*
* Modifications to the initial code base are copyright of their
* respective authors, or their employers as appropriate.  Authorship
* of the modifications may be determined from the ChangeLog placed at
* the end of this file.
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.

* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.

* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*/

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Aug 7, 2007<br><br>
 */
public class ServerOptions extends AbstractOntologyServerServlet {

    protected void handleXMLRequest(Map<String, String> params, OWLHTMLServer server, URL servletURL, PrintWriter out) throws OntServerException {
        boolean success = handleOptionsSet(params, server);

//        if (success){
//            out.println("<options><" + option + " value='" + value + "'/></options>");
//        }
    }

    protected HTMLDoclet handleHTMLRequest(Map<String, String> params, OWLHTMLServer server, URL pageURL) throws OntServerException {
        boolean success = handleOptionsSet(params, server);

        if (success){
            throw new RedirectException(server.getURLScheme().getURLForRelativePage(OWLHTMLConstants.OPTIONS_HTML));
        }
        else{
            EmptyOWLDocPage page = new EmptyOWLDocPage(server);
            page.addDoclet(new OptionsTableDoclet(params, server));
            return page;
        }
    }

    private boolean handleOptionsSet(Map<String, String> params, OWLHTMLServer server) throws OntServerException {
        boolean success = false;

        for (String option : params.keySet()){

            String value = params.get(option);

            final ServerProperties serverProperties = server.getProperties();

            if (option.equals(OWLHTMLConstants.OPTION_FRAMES)){
                if (value.equals(OWLHTMLConstants.NO_FRAMES) || value.equals(ServerConstants.FALSE)){
                    if (serverProperties.get(OWLHTMLConstants.OPTION_CONTENT_WINDOW) != null){
                        serverProperties.set(OWLHTMLConstants.OPTION_CONTENT_WINDOW, null);
                        success = true;
                    }
                }
                else if (value.equals(OWLHTMLConstants.SHOW_FRAMES) || value.equals(ServerConstants.TRUE)){
                    if (serverProperties.get(OWLHTMLConstants.OPTION_CONTENT_WINDOW) == null){
                        serverProperties.set(OWLHTMLConstants.OPTION_CONTENT_WINDOW, OWLHTMLConstants.LinkTarget.content.toString());
                        success = true;
                    }
                }
            }
            else{
                success = serverProperties.set(option, value);
            }
        }

        if (success){
            SessionManager.labelServerState(server);
        }
        return success;
    }

    protected Map<String, Set<String>> getRequiredParams(OWLServer server) {
        return Collections.emptyMap();
    }
}
