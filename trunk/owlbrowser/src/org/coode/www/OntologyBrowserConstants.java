/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.www;

import org.coode.html.impl.OWLHTMLConstants;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jan 14, 2008<br><br>
 */
public class OntologyBrowserConstants extends OWLHTMLConstants {

    public static String VERSION = "1.2.0";

    public static final String SERVER_STATES_DIR = "caches/";
    public static final String SERVER_STATES_EXT = ".saved";

    // bookmarks
    public static final String BOOKMARKS_XML = "custom.bookmarks.xml";
    public static final String DEFAULT_BOOKMARKS_XML = "default.bookmarks.xml";

    public static final String LOAD_ONTOLOGIES_INPUT_ID = "uri-spec";

    public static final String DL_QUERY_LABEL = "DL Query";

    // supported formats
    public enum RequestFormat{
        xml(MIME_XML),
        html(MIME_HTML),
        htmlfrag(MIME_HTML);

        private String mimeType;

        RequestFormat(String mimeType) {
            this.mimeType = mimeType;
        }

        public String getMimeType(){
            return mimeType;
        }
    }

    // and their mime types
    public static final String MIME_XML = "text/xml;charset=" + OWLHTMLConstants.DEFAULT_ENCODING;
    public static final String MIME_HTML = "text/html;charset=" + OWLHTMLConstants.DEFAULT_ENCODING;

}
