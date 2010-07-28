package org.coode.html.index;

import org.coode.html.OWLHTMLKit;
import org.coode.html.doclet.AbstractOWLElementsDoclet;
import org.coode.html.doclet.ElementsDoclet;
import org.coode.html.impl.OWLHTMLConstants;
import org.coode.html.impl.OWLHTMLProperty;
import org.coode.html.page.OWLDocPage;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLOntology;

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

/**
 * Author: Nick Drummond<br>
 * nick.drummond@cs.manchester.ac.uk<br>
 * http://www.cs.man.ac.uk/~drummond<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jun 12, 2007<br><br>
 * <p/>
 * code made available under Mozilla Public License (http://www.mozilla.org/MPL/MPL-1.1.html)<br>
 * copyright 2006, The University of Manchester<br>
 */
public class OWLObjectIndexHTMLPage<O extends OWLObject> extends OWLDocPage<OWLOntology> {

    private Set<O> index = new HashSet<O>();

    private AbstractOWLElementsDoclet<OWLOntology, O> indexDoclet;

    private String id = "Index";


    public OWLObjectIndexHTMLPage(OWLHTMLKit kit) {
        super(kit);
        indexDoclet = new AbstractOWLElementsDoclet<OWLOntology, O>(id, ElementsDoclet.Format.list, kit) {
            protected Collection<O> getElements(Set<OWLOntology> onts) {
                return index;
            }
            public String getID() {
                return id + " (" + index.size() + ")";
            }
        };
        String contentWindow = getHTMLGenerator().getHTMLProperties().get(OWLHTMLProperty.optionContentWindow);
        if (contentWindow != null){
            indexDoclet.setTarget(OWLHTMLConstants.LinkTarget.valueOf(contentWindow));
        }
        else{
            indexDoclet.setTarget(null);
        }
        addDoclet(indexDoclet);
    }


    public void setTitle(String title) {
        this.id = title;
        super.setTitle(title);
    }


    public void add(O obj){
        index.add(obj);
    }

    public void addAll(Set<? extends O> objs){
        index.addAll(objs);
    }

    public String getTitle() {
        return super.getTitle() + " (" + index.size() + ")";
    }
}
