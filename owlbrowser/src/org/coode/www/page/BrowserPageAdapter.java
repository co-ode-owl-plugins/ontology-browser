package org.coode.www.page;

import org.coode.html.OWLHTMLKit;
import org.coode.html.doclet.HTMLDoclet;
import org.coode.html.page.DefaultHTMLPage;
import org.coode.www.OntologyBrowserConstants;
import org.coode.www.doclet.MenuBarDoclet;
import org.coode.www.doclet.TitleDoclet;

import java.io.PrintWriter;
import java.net.URL;
import java.util.Set;

/**
 * Author: drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jul 26, 2010<br><br>
 *
 * Decorates a standard HTMLPage with
 */
public class BrowserPageAdapter<O> implements HTMLDoclet<O> {

    private DefaultHTMLPage<O> delegate;

    public BrowserPageAdapter(DefaultHTMLPage<O> delegate, OWLHTMLKit kit, URL pageURL) {
        this.delegate = delegate;
        delegate.addDoclet(new TitleDoclet(), 0);
        if (kit.isActive()){
            delegate.addDoclet(new MenuBarDoclet(kit), 1);
        }
        delegate.addOnLoad("optionsURL=\"" + kit.getURLScheme().getURLForRelativePage(OntologyBrowserConstants.OPTIONS_HTML) + "\";");
    }

    public String getID() {
        return delegate.getID();
    }

    public void renderContent(URL pageURL, PrintWriter out) {
        delegate.renderContent(pageURL, out);
    }

    public void renderAll(URL pageURL, PrintWriter out) {
        delegate.renderAll(pageURL, out);
    }

    public void setUserObject(O object) {
        delegate.setUserObject(object);
    }

    public O getUserObject() {
        return delegate.getUserObject();
    }

    public boolean isPinned() {
        return delegate.isPinned();
    }

    public Set<URL> getRequiredCSS() {
        return delegate.getRequiredCSS();
    }

    public Set<URL> getRequiredJS() {
        return delegate.getRequiredJS();
    }
}
