package org.coode.html.renderer;

import org.coode.html.OWLHTMLKit;
import org.coode.html.impl.OWLHTMLConstants;
import org.semanticweb.owlapi.model.*;

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
 */
public class OWLHTMLRenderer implements ElementRenderer<OWLObject>{

    private OWLHTMLKit kit;

    private OWLHTMLConstants.LinkTarget linkTarget;


    public OWLHTMLRenderer(OWLHTMLKit kit) {
        this.kit = kit;
    }

    public void render(OWLObject obj, URL pageURL, PrintWriter out){
        OWLHTMLVisitor rendererVisitor = new OWLHTMLVisitor(kit.getURLScheme(),
                                                            kit.getOWLServer().getShortFormProvider(), out);
        rendererVisitor.setOntologies(kit.getVisibleOntologies());
        rendererVisitor.setActiveOntology(kit.getOWLServer().getActiveOntology());
        rendererVisitor.setPageURL(pageURL);
        rendererVisitor.setContentTargetWindow(linkTarget);
        obj.accept(rendererVisitor);
    }

    public String render(OWLObject obj, URL pageURL){
        StringWriter writer = new StringWriter();
        render(obj, pageURL, new PrintWriter(writer));
        writer.flush();
        return writer.getBuffer().toString();
    }

    public void setContentTargetWindow(OWLHTMLConstants.LinkTarget linkTarget) {
        this.linkTarget = linkTarget;
    }
}
