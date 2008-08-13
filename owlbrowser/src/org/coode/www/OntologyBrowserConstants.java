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

    public static String VERSION = "1.1.3";

    public static final String SERVER_STATES_DIR = "caches/";
    public static final String SERVER_STATES_EXT = ".saved";
    public static final String BOOKMARKS_XML = "custom.bookmarks.xml";

    // debugging flags
    public static final boolean SIMULATE_SERVER_LATENCY = false;
    public static final boolean SHOW_MEMORY = false;
    public static final boolean LOGGING = true;

    public static final String PARAM_URI = "uri";
    public static final String LOAD_ONTOLOGIES_INPUT_ID = "uri-spec";

    public static final String PARAM_ACTION = "action";
    public static final String PARAM_CLEAR = "clear";
    public static final String PARAM_FORMAT = "format";

    public static final String DOCS_HTML = "docs/index.html";

    public static final String DL_QUERY_LABEL = "DL Query";

}
