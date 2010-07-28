package org.coode.www.page;

import org.coode.html.OWLHTMLKit;
import org.coode.html.doclet.HTMLDoclet;
import org.coode.html.doclet.MenuBarDoclet;
import org.coode.html.impl.OWLHTMLConstants;
import org.coode.html.page.DefaultHTMLPage;
import org.coode.owl.mngr.NamedObjectType;
import org.coode.owl.mngr.ServerProperty;
import org.coode.www.OntologyBrowserConstants;
import org.coode.www.doclet.AutocompleteDoclet;
import org.coode.www.doclet.OptionSelectorDoclet;
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
        delegate.addOnLoad("optionsURL=\"" + kit.getURLScheme().getURLForRelativePage(OntologyBrowserConstants.OPTIONS_HTML) + "\";");
        prepareMenuBar(kit, pageURL);
    }


    private void prepareMenuBar(OWLHTMLKit kit, URL pageURL) {
        MenuBarDoclet menuDoclet = (MenuBarDoclet)delegate.getDoclet(MenuBarDoclet.ID);
        if (menuDoclet != null){

            OptionSelectorDoclet activeOntDoclet = new OptionSelectorDoclet(kit, ServerProperty.optionActiveOnt.name(),
                                                                            kit.getOWLServer().getProperties().get(ServerProperty.optionActiveOnt),
                                                                            kit.getOWLServer().getProperties().getAllowedValues(ServerProperty.optionActiveOnt));
            menuDoclet.addDoclet(activeOntDoclet, 0);

            AutocompleteDoclet searchboxDoclet = new AutocompleteDoclet(kit, "find", true);
            searchboxDoclet.setParamName("name");
            searchboxDoclet.setSubmitName("find");
            searchboxDoclet.setSubmitURL(kit.getURLScheme().getURLForIndex(NamedObjectType.entities)); // could be more direct
            searchboxDoclet.setTarget(OWLHTMLConstants.LinkTarget.content);
            menuDoclet.addDoclet(searchboxDoclet, 1);
        }
    }

    public String getID() {
        return delegate.getID();
    }

    public void renderContent(URL pageURL, PrintWriter out) {
        delegate.renderContent(pageURL, out);    }

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
