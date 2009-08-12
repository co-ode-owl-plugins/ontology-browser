/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.doclet;

import org.coode.html.OWLHTMLKit;
import org.coode.html.impl.OWLHTMLConstants;
import org.coode.html.renderer.ElementRenderer;
import org.coode.html.renderer.OWLHTMLRenderer;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLOntology;

import java.util.Collection;
import java.util.Set;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jan 25, 2008<br><br>
 */
public abstract class AbstractOWLElementsDoclet<O extends OWLObject, E extends OWLObject> extends ElementsDoclet<O, E> {

    private OWLHTMLKit kit;

    private OWLHTMLConstants.LinkTarget linkTarget;

    private Set<OWLOntology> ontologies;


    public AbstractOWLElementsDoclet(String name, Format format, OWLHTMLKit kit) {
        super(name, format);
        this.kit = kit;
        setComparator(kit.getOWLServer().getComparator());
    }

    protected final OWLHTMLKit getHTMLGenerator(){
        return kit;
    }

    public void setOntologies(Set<OWLOntology> onts){
        this.ontologies = onts;
    }

    public void setTarget(OWLHTMLConstants.LinkTarget target){
        this.linkTarget = target;
    }

    protected final Collection<E> getElements(){
        if (ontologies == null){
            return getElements(getHTMLGenerator().getVisibleOntologies());
        }
        else{
            return getElements(ontologies);
        }
    }

    protected abstract Collection<E> getElements(Set<OWLOntology> ontologies);

    protected final ElementRenderer<? super E> getElementRenderer() {
        OWLHTMLRenderer ren = new OWLHTMLRenderer(kit);
        if (linkTarget != null){
            ren.setContentTargetWindow(linkTarget);
        }
        return ren;
    }

    public String getID() {
        return super.getID() + " (" + getElements().size() + ")";
    }
}
