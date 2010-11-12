package org.coode.html.page;

import org.coode.html.OWLHTMLKit;
import org.coode.html.doclet.HTMLDoclet;
import org.coode.html.doclet.MessageBoxDoclet;
import org.coode.html.doclet.TabsDoclet;
import org.coode.html.impl.OWLHTMLConstants;
import org.coode.html.impl.OWLHTMLProperty;
import org.semanticweb.owlapi.model.OWLObject;

import java.io.PrintWriter;
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
public class OWLDocPage<O extends OWLObject> extends DefaultHTMLPage<O> {

    private OWLHTMLKit kit;


    public OWLDocPage(OWLHTMLKit kit) {
        this.kit = kit;

        String css = kit.getHTMLProperties().get(OWLHTMLProperty.optionDefaultCSS);

        if (css != null){
            addCSS(kit.getURLScheme().getURLForRelativePage(css));
        }

        addJavascript(kit.getURLScheme().getURLForRelativePage(OWLHTMLConstants.JQUERY_JS));
        addJavascript(kit.getURLScheme().getURLForRelativePage(OWLHTMLConstants.JS_DEFAULT));
        if (!kit.getOWLServer().getOntologies().isEmpty()){
            addDoclet(new TabsDoclet(kit));
        }
    }

    protected final void renderHeader(URL pageURL, PrintWriter out) {
        super.renderHeader(pageURL, out);
    }

    protected final void renderFooter(URL pageURL, PrintWriter out) {

        out.println("\n<p class='footer'>");
        out.print("<a href='");
        out.print(OWLHTMLConstants.HOME_PAGE);
        out.println("' target='_blank'>OWL HTML inside</a>");
        out.println("</p>");

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
        addMessage(null, message);
    }

    public void addError(String error) {
        addMessage("Detailed error", error, "error");
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

    protected final OWLHTMLKit getOWLHTMLKit() {
        return kit;
    }
}
