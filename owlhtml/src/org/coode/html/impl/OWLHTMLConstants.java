package org.coode.html.impl;

import org.coode.owl.mngr.ServerConstants;

import java.net.URL;
import java.net.MalformedURLException;
/*
* Copyright (C) 2007, University of Manchester
*
* Modifications to the initial code base are copyright of their
* respective authors, or their employers as appropriate.  Authorship
* of the modifications may be determined from the ChangeLog placed at
* the end of this file.
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.

* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.

* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*/

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Oct 2, 2007<br><br>
 */
public class OWLHTMLConstants extends ServerConstants {

    public static final String DEFAULT_ENCODING = "utf8";


    public static final String JS_DEFAULT = "default.js";
    public static final String JS_FORM = "form.js";
    public static final String JS_DL_QUERY = "dlquery.js";
    public static final String JS_TREE = "tree.js";
    public static final String IMAGES_REMOVE_PNG = "images/remove.png";
    public static final String AUTO_SUGGEST_JS = "bsn.AutoSuggest_2.1_multiword.js";
    public static final String AUTO_SUGGEST_CSS = "css/autosuggest_inquisitor.css";

    // used to find the base URL of the servlets - will hunt in request URL for this string
    public static final String ONTOLOGY_SERVER = "/browser/";

    public static final String DEFAULT_EXTENSION = ".html";

    public static final String INDEX_HTML = "index" + DEFAULT_EXTENSION;

    public static final String ALL_ENTITIES_TITLE = "All Resources";

    public static final String ONTOLOGY_HTML = "ontology" + DEFAULT_EXTENSION;
    public static final String CONTENTS_HTML = "contents" + DEFAULT_EXTENSION;
    public static final String DL_QUERY_HTML = "dlquery/";
    public static final String MANAGE_HTML = "manage/";
    public static final String OPTIONS_HTML = "options/";
    public static final String HEADER_HTML = "header" + DEFAULT_EXTENSION;
    public static final String SIGNOUT_HTML = "signout" + DEFAULT_EXTENSION;

    public static final String DL_QUERY_LABEL = "DL Query";
    public static final String PERMALINK_LABEL = "Permalink";    
    public static final String MANAGE_LABEL = "Manage";
    public static final String CONTENTS_LABEL = "Contents";
    public static final String RESTART_LABEL = "Restart";
    public static final String BOOKMARKS_LABEL = "Bookmarks";

    public static final int MAX_SESSIONS = 4;
    public static final int SESSION_TIMEOUT = 5;

    public static final String PARAM_SESSION_LABEL = "session";

    public static String ONTOLOGY_SERVER_NAME = "Ontology Browser";

    public static final URL HOME_PAGE;

    static {
        try {
            HOME_PAGE = new URL("http://code.google.com/p/ontology-browser/");
        }
        catch (MalformedURLException e) {
            throw new RuntimeException(e);
        }
    }


    public enum LinkTarget{_top, content, nav, subnav, header, _blank}


    // window modes
    public static final String NO_FRAMES = "no_frames";
    public static final String SHOW_FRAMES = "frames";


    // keys for options on the server
    public static final String OPTION_CONTENT_WINDOW = "content-window";
    public static final String OPTION_INDEX_ALL_URL = "index-all-url";
    public static final String OPTION_DEFAULT_CSS = "css";
    public static final String OPTION_FRAMES = "frames";
    public static final String OPTION_REASONER_ENABLED = "reasoner.enabled";
    public static final String OPTION_SHOW_MINI_HIERARCHIES = "option_show_mini_hierarchies";    
    public static final String OPTION_SHOW_INFERRED_HIERARCHIES = "option_show_inferred_hierarchies";    
    public static final String OPTION_RENDER_PERMALINK = "option_render_permalink";
    public static final String OPTION_RENDER_ONTOLOGY_SUMMARY_CLOUD = "option_render_ontology_summary_cloud";

    // non-external values
    public static final String DEFAULT_INDEX_ALL_URL = "index-all" + DEFAULT_EXTENSION;
    public static final String CSS_DEFAULT = "default.css";
}
