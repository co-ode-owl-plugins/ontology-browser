/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.www.doclet;

import org.coode.html.OWLHTMLKit;
import org.coode.html.doclet.HTMLDoclet;
import org.coode.html.renderer.OWLHTMLRenderer;
import org.semanticweb.owlapi.model.OWLNamedObject;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.util.ShortFormProvider;

import java.io.PrintWriter;
import java.net.URL;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jan 25, 2008<br><br>
 */
public class SelectorContentsDoclet implements HTMLDoclet {

    private static final String ID = "doclet.selector.contents";

    private List<OWLObject> contents;
    private OWLObject defaultValue;
    private ShortFormProvider ren;
    private OWLHTMLKit kit;


    public SelectorContentsDoclet(ShortFormProvider renderer, OWLHTMLKit kit){
        this.ren = renderer;
        this.kit = kit;
    }

    public SelectorContentsDoclet(Set<? extends OWLObject> contents,
                                        OWLObject defaultValue,
                                        ShortFormProvider renderer,
                                        OWLHTMLKit kit) {
        this.ren = renderer;
        this.contents = new LinkedList<OWLObject>(contents);
        this.defaultValue = defaultValue;
        this.kit = kit;
        Collections.sort(this.contents, kit.getOWLServer().getComparator());
    }

    public String getID() {
        return ID;
    }

    public void renderContent(URL pageURL, PrintWriter out) {
        String defaultStr = (defaultValue == null) ? "selected='selected'" : "";
        out.println("<option value='' " + defaultStr + "> </option>");

        OWLHTMLRenderer htmlRen = new OWLHTMLRenderer(kit);

        for (OWLObject entity : contents){
            defaultStr = (defaultValue != null && entity.equals(defaultValue))? " selected='selected'" : "";
            String value;
            if (entity instanceof OWLNamedObject){
                value = ((OWLNamedObject)entity).getIRI().toString();
            }
            else{
                value = htmlRen.render(entity, pageURL);
            }

            String name;
            if (entity instanceof OWLEntity){
                name = ren.getShortForm((OWLEntity)entity);
            }
            else{
                name = value;
            }
            out.println("<option value='" + value + "'" + defaultStr +
                        ">" + name + "</option>");
        }
    }

    public void renderAll(URL pageURL, PrintWriter out) {
        renderContent(pageURL, out);
    }

    public void setUserObject(Object object) {
        //@@TODO implement
    }

    public Object getUserObject() {
        return null;  //@@TODO implement
    }

    public boolean isPinned() {
        return true;
    }

    public Set getRequiredCSS() {
        return null;  //@@TODO implement
    }

    public Set getRequiredJS() {
        return null;  //@@TODO implement
    }
}
