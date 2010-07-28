package org.coode.www.page;

import com.sun.org.apache.xerces.internal.parsers.DOMParser;
import org.coode.html.OWLHTMLKit;
import org.coode.html.doclet.AbstractTitleDoclet;
import org.coode.html.impl.OWLHTMLConstants;
import org.coode.html.page.OWLDocPage;
import org.coode.html.util.FileUtils;
import org.coode.html.util.URLUtils;
import org.coode.owl.mngr.NamedObjectType;
import org.coode.www.OntologyBrowserConstants;
import org.coode.www.doclet.BlurbDoclet;
import org.coode.www.doclet.LoadFormDoclet;
import org.coode.www.doclet.OntologyMappingsTableDoclet;
import org.coode.www.mngr.SessionManager;
import org.semanticweb.owlapi.model.OWLOntologyID;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import java.io.*;
import java.net.URI;
import java.net.URL;
import java.util.Collections;
import java.util.HashMap;
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

        if (locationsMap.isEmpty()){
            addDoclet(new BlurbDoclet());

            final LoadFormDoclet loadDoclet = new LoadFormDoclet();
            loadDoclet.addBookmarkSet("or Select a bookmark from below:", getBookmarks());
            addDoclet(loadDoclet);
        }
        else{

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
                message += ("<p class='warn'>There appear to be missing imports in your ontology.</p>" +
                            "<p>You can specify a location for any that have not been loaded in the following table.<br />" +
                            "Or, you can <a style='font-weight: bolder; color: blue;' target='_top' href='" + contentsURL +
                            "'>continue to browse</a> your ontology without loading the imports.</p>");
            }

//            addDoclet(loadDoclet);

            OntologyMappingsTableDoclet table = new OntologyMappingsTableDoclet(kit);
            table.setMap(locationsMap);
            addDoclet(table);
        }

        if (message != null){
            addMessage(message);
        }
    }

    public Map<String, URI> getBookmarks() {
        Map<String, URI> bookmarks = Collections.emptyMap();
        File bookmarksFile = SessionManager.getFile(OntologyBrowserConstants.BOOKMARKS_XML);
        if (!bookmarksFile.exists()){
            FileUtils fileUtils = new FileUtils("resources/", OWLHTMLConstants.DEFAULT_ENCODING); // path not used
            InputStream in = getClass().getResourceAsStream(OntologyBrowserConstants.DEFAULT_BOOKMARKS_XML);
            try {
                fileUtils.saveFile(in, bookmarksFile);
            }
            catch (IOException e) {
                e.printStackTrace();
            }
        }

        try {
            bookmarks = loadBookmarks(new BufferedReader(new FileReader(bookmarksFile)));
        }
        catch (Exception e) {
            e.printStackTrace();
        }
        return bookmarks;
    }

    private Map<String, URI> loadBookmarks(Reader reader) throws IOException, SAXException {
        Map<String, URI> bookmarkMap = new HashMap<String, URI>();
        DOMParser parser = new DOMParser();
        InputSource inputSource = new InputSource(reader);
        parser.parse(inputSource);
        Document doc = parser.getDocument();
        NodeList bookmarkElements = doc.getElementsByTagName("bookmark");
        for (int i=0; i<bookmarkElements.getLength(); i++){
            Node element = bookmarkElements.item(i);
            String name = element.getAttributes().getNamedItem("name").getTextContent();
            URI uri = URI.create(element.getTextContent());
            bookmarkMap.put(name, uri);
        }
        return bookmarkMap;
    }
}
