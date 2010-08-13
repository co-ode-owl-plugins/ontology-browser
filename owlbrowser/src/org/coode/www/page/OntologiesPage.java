package org.coode.www.page;

import org.coode.html.OWLHTMLKit;
import org.coode.html.doclet.AbstractTitleDoclet;
import org.coode.html.impl.OWLHTMLConstants;
import org.coode.html.page.OWLDocPage;
import org.coode.html.util.URLUtils;
import org.coode.owl.mngr.NamedObjectType;
import org.coode.www.OntologyBrowserConstants;
import org.coode.www.doclet.OntologyMappingsTableDoclet;
import org.semanticweb.owlapi.model.OWLOntologyID;

import java.net.URI;
import java.net.URL;
import java.util.Map;

/**
 * Author: drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jul 23, 2010<br><br>
 */
public class OntologiesPage extends OWLDocPage {

    public OntologiesPage(OWLHTMLKit kit, URL pageURL, String message) {
        super(kit);

        setTitle(OntologyBrowserConstants.LOAD_LABEL);

        setAutoFocusedComponent(OntologyBrowserConstants.LOAD_ONTOLOGIES_INPUT_ID);

        final Map<OWLOntologyID, URI> locationsMap = kit.getOWLServer().getLocationsMap();


            AbstractTitleDoclet titleDoclet = new AbstractTitleDoclet(kit){

                @Override
                public String getTitle() {
                    return NamedObjectType.ontologies.getPluralRendering();
                }

                @Override
                public String getSubtitle() {
                    return null;
                }
            };

            addDoclet(titleDoclet);

            if (locationsMap.containsValue(null)){
                if (message == null){
                    message = "";
                }
                String contentsURL = URLUtils.createRelativeURL(pageURL, kit.getURLScheme().getURLForRelativePage(OWLHTMLConstants.CONTENTS_HTML));
                message += ("<p>There appear to be missing imports in your ontology.</p>" +
                            "<p>You can specify a location for any that have not been loaded in the following table.<br />" +
                            "Or, you can <a href='" + contentsURL +
                            "'>continue to browse</a> your ontology without loading the imports.</p>");
            }

            OntologyMappingsTableDoclet table = new OntologyMappingsTableDoclet(kit);
            table.setMap(locationsMap);
            addDoclet(table);

        if (message != null){
            addMessage(message);
        }
    }
}
