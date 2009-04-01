package org.coode.html.page;

import org.coode.html.OWLHTMLServer;
import org.coode.html.doclet.HTMLDoclet;
import org.coode.html.doclet.MenuBarDoclet;
import org.coode.html.doclet.MessageBoxDoclet;
import org.coode.html.doclet.TabsDoclet;
import org.coode.html.impl.OWLHTMLConstants;
import org.coode.html.renderer.OWLHTMLRenderer;
import org.coode.html.url.PermalinkURLScheme;
import org.coode.owl.mngr.ServerConstants;
import org.semanticweb.owl.model.OWLObject;

import java.io.PrintWriter;
import java.io.PrintStream;
import java.io.StringWriter;
import java.net.URL;

/**
 * Author: Nick Drummond<br>
 * nick.drummond@cs.manchester.ac.uk<br>
 * http://www.cs.man.ac.uk/~drummond<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jun 7, 2007<br><br>
 * <p/>
 * code made available under Mozilla Public License (http://www.mozilla.org/MPL/MPL-1.1.html)<br>
 * copyright 2006, The University of Manchester<br>
 *
 * A full HTML page for a given OWL Object. Automatically adds menu and tab doclets when appropriate.
 *
 * Subclasses should not overload header and footer except for content that lies outside of the OWLDoc page.
 *
 * Content should be provided as additional doclets.
 */
public class EmptyOWLDocPage<O extends OWLObject> extends DefaultHTMLPage<O> {

    private OWLHTMLServer server;

    public EmptyOWLDocPage(OWLHTMLServer server) {
        this.server = server;
        String css = server.getProperties().get(OWLHTMLConstants.OPTION_DEFAULT_CSS);
        if (css != null){
            addCSS(server.getURLScheme().getURLForRelativePage(css));
        }
        if (isSingleFrameNavigation()){
            addJavascript(server.getURLScheme().getURLForRelativePage(OWLHTMLConstants.JS_DEFAULT));
            if (!server.getOntologies().isEmpty()){
                addDoclet(new MenuBarDoclet(server));
                addDoclet(new TabsDoclet(server));
            }
        }
    }

    protected final void renderHeader(URL pageURL, PrintWriter out) {
        super.renderHeader(pageURL, out);
    }

    protected final void renderFooter(URL pageURL, PrintWriter out) {

        out.println("<p class='footer'>");

        if (server.getProperties().isSet(OWLHTMLConstants.OPTION_RENDER_PERMALINK)){
            renderLink(OWLHTMLConstants.PERMALINK_LABEL,
                       new PermalinkURLScheme(server.getURLScheme(), server).getURLForAbsolutePage(pageURL),
                       null, null, isSingleFrameNavigation(), pageURL, out);
            out.print(" | ");
        }

        out.println("<a href='" + OWLHTMLConstants.HOME_PAGE + "' target='_blank'>OWL HTML inside</a></p>");

        super.renderFooter(pageURL, out);
    }

    public void addMessage(String title, String message, String cssClass) {
        int pos = 0;
        final HTMLDoclet tabs = getDoclet(TabsDoclet.ID);
        if (tabs != null){
            pos = Math.max(0, indexOf(tabs)+1);
        }
        final MessageBoxDoclet messageBoxDoclet = new MessageBoxDoclet(title, message);
        messageBoxDoclet.setClass(cssClass);
        addDoclet(messageBoxDoclet, pos);
    }

    /**
     * Puts a message box at the top of the page, under the tabs (if there are any)
     * @param title
     * @param message
     */
    public void addMessage(String title, String message) {
        addMessage(title, message, "message");
    }

    public void addMessage(String message) {
        addMessage("Message", message);
    }

    public void addError(String error) {
        addMessage("Error", error, "error");
    }

    public void addError(Throwable error){
        Throwable cause = error;
        while (cause.getCause() != null){
            cause = cause.getCause();
        }
        String msg = cause.getMessage();
        if (msg == null){
            StringWriter stringWriter = new StringWriter();
            final PrintWriter printWriter = new PrintWriter(stringWriter);
            cause.printStackTrace(printWriter);
            printWriter.flush();
            msg = stringWriter.toString();
        }
        addError(msg);
    }

    protected final OWLHTMLServer getServer() {
        return server;
    }

    protected final OWLHTMLRenderer getHTMLRenderer() {
        return new OWLHTMLRenderer(server);
    }

    protected final boolean isSingleFrameNavigation() {
        return server.getProperties().get(OWLHTMLConstants.OPTION_CONTENT_WINDOW) == null;
    }

    protected final boolean isReasonerEnabled() {
        return server.getProperties().get(OWLHTMLConstants.OPTION_REASONER_ENABLED).equals(ServerConstants.TRUE);
    }
}
