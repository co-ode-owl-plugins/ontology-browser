package org.coode.owl.mngr;

import org.apache.log4j.Logger;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;
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
 * Date: Aug 10, 2007<br><br>
 */
public class ServerConstants {

    public static final String OPTION_CREATE_TOP_ONTOLOGY = "option_create_top_ontology";
    public static final String OPTION_REASONER = "option_reasoner";
    public static final String OPTION_DIG_REASONER_URL = "option_reasoner_url"; // for DIG reasoners
    public static final String OPTION_RENDER_SUB_EXPAND_LINKS = "option_render_sub_expand_links";
    public static final String OPTION_RENDER_SUBS = "option_render_subs";
    public static final String OPTION_REN = "ren";
    public static final String OPTION_LABEL_URI = "option_label_uri";
    public static final String OPTION_ACTIVE_ONT = "option_active_ont";


    public static final String TRUE = "true";
    public static final String FALSE = "false";

    // supported reasoners
    public static final String PELLET = "pellet";
    public static final String FACTPLUSPLUS = "factPlusPlus";
    public static final String DIG = "DIG";

    // supported syntaxes
//    public static final String QUICK_DESCRIPTION_SYNTAX = "qd";
//    public static final String MANCHESTER_SYNTAX = "man";
//    public static final String SIMPLE_MANCHESTER_SYNTAX = "simple";

    public static enum Syntax {man, simple, qd}

    // simple restriction vocabulary - allowed for simple descriptions
    public static final String INTERSECTION = "and";
    public static final String SOME = "some";
    public static final String ONLY = "only";
    public static final String HAS_VALUE = "hasValue";
    public static final String OPEN_PAREN = "(";
    public static final String CLOSE_PAREN = ")";

    // supported renderers
    public static final String RENDERER_FRAG = "frag";
    public static final String RENDERER_LABEL = "label";



    public static final List<String> RESTRICTION_TYPES = new ArrayList<String>();


    public static final String TOP_ONTOLOGY_URI_STR = "http://www.co-ode.org/ontologies/meta.owl";
    public static URI TOP_ONTOLOGY_URI;
    public static URI RELATED_TO;

    static {
        RESTRICTION_TYPES.add(SOME);
        RESTRICTION_TYPES.add(ONLY);
        RESTRICTION_TYPES.add(HAS_VALUE);
        
        try {
            TOP_ONTOLOGY_URI = new URI(TOP_ONTOLOGY_URI_STR);
            RELATED_TO = new URI(TOP_ONTOLOGY_URI_STR + "#relatedTo");
        }
        catch (URISyntaxException e) {
            Logger.getLogger(ServerConstants.class.getName()).error(e);
        }
    }
}
