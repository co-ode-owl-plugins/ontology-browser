/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.doclet;

import org.coode.html.OWLHTMLKit;
import org.coode.html.impl.OWLHTMLConstants;
import org.coode.owl.mngr.NamedObjectType;
import org.coode.owl.mngr.ServerProperty;
import org.semanticweb.owlapi.model.OWLOntology;

import java.io.PrintWriter;
import java.net.URL;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jan 24, 2008<br><br>
 */
public class TabsDoclet extends AbstractOWLDocDoclet {

    public static final String ID = "doclet.Tabs";

    public TabsDoclet(OWLHTMLKit kit) {
        super(kit);
    }

    public void renderHeader(URL pageURL, PrintWriter out) {
        out.println("\n<div id='tabs'>");

        OWLHTMLKit kit = getOWLHTMLKit();
        final boolean singleFrame = isSingleFrameNavigation();

        for (NamedObjectType type : NamedObjectType.values()){

            if (type != NamedObjectType.entities){ // skip entities

                if (entitiesExist(type)){

                    renderLink(type.getPluralRendering(),
                               kit.getURLScheme().getURLForIndex(type),
                               OWLHTMLConstants.LinkTarget.subnav,
                               "",
                               singleFrame,
                               pageURL,
                               out);
                }
                else{
                    out.print(type.getPluralRendering());
                }

                out.println();
            }
        }

        renderLink("Clouds",
                   kit.getURLScheme().getURLForRelativePage("cloud/"),
                   OWLHTMLConstants.LinkTarget.subnav,
                   "",
                   singleFrame,
                   pageURL,
                   out);
        out.println();


        // add the DL Query tab if the reasoner is enabled
        if (kit.getOWLServer().getProperties().isSet(ServerProperty.optionReasonerEnabled)){
            renderLink(OWLHTMLConstants.DL_QUERY_LABEL,
                       kit.getURLScheme().getURLForRelativePage(OWLHTMLConstants.DL_QUERY_HTML),
                       OWLHTMLConstants.LinkTarget.subnav,
                       null,
                       singleFrame,
                       pageURL,
                       out);
            out.println();            
        }

    }

    private boolean entitiesExist(NamedObjectType type) {
        for (OWLOntology ont : getOWLHTMLKit().getOWLServer().getActiveOntologies()){
            switch (type){
                case classes: if (!ont.getClassesInSignature().isEmpty()){
                    return true;
                }
                    break;
                case objectproperties: if (!ont.getObjectPropertiesInSignature().isEmpty()){
                    return true;
                }
                    break;
                case dataproperties: if (!ont.getDataPropertiesInSignature().isEmpty()){
                    return true;
                }
                    break;
                case individuals: if (!ont.getIndividualsInSignature().isEmpty()){
                    return true;
                }
                    break;
                case annotationproperties: if (!ont.getAnnotationPropertiesInSignature().isEmpty()){
                    return true;
                }
                    break;
                case datatypes: if (!ont.getDatatypesInSignature().isEmpty()){
                    return true;
                }
                    break;
                case entities: return true;
                case ontologies: return true;
            }
            // TODO: no more efficient way to ask this?
        }
        return false;
    }

    protected void renderFooter(URL pageURL, PrintWriter out) {
        out.println("</div> <!-- tabs -->\n\n");
    }

    public String getID() {
        return ID;
    }
}
