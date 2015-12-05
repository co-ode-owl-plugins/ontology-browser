package org.coode.www;

import org.apache.commons.io.FileUtils;
import org.coode.www.mngr.SessionManager;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.net.URI;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

/**
 * Author: drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Aug 1, 2010<br><br>
 */
public class Bookmarks {

    public static Map<String, URI> getBookmarks() {
        Map<String, URI> bookmarks = Collections.emptyMap();
        File bookmarksFile = SessionManager.getFile(OntologyBrowserConstants.BOOKMARKS_XML);
        if (!bookmarksFile.exists()){
            try {
                InputStream in = Bookmarks.class.getResourceAsStream(OntologyBrowserConstants.DEFAULT_BOOKMARKS_XML);
                FileUtils.copyInputStreamToFile(in, bookmarksFile);
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

    private static Map<String, URI> loadBookmarks(Reader reader) throws IOException, SAXException, ParserConfigurationException {
        Map<String, URI> bookmarkMap = new LinkedHashMap<String, URI>();
        DocumentBuilderFactory dbFactory = DocumentBuilderFactory.newInstance();
        DocumentBuilder dBuilder = dbFactory.newDocumentBuilder();
        InputSource inputSource = new InputSource(reader);
        Document doc = dBuilder.parse(inputSource);
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
