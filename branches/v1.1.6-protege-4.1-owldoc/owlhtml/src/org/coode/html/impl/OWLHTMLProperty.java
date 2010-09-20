package org.coode.html.impl;

import java.util.Map;
import java.util.HashMap;
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
 * Author: drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Aug 7, 2009<br><br>
 */
public enum OWLHTMLProperty {

    optionContentWindow("content-window"),
    optionIndexAllURL("index-all-url"),
    optionDefaultCSS("css"),
    optionUseFrames("frames"),
    optionReasonerEnabled("reasoner.enabled"),
    optionShowMiniHierarchies("option_show_mini_hierarchies"),
    optionShowInferredHierarchies("option_show_inferred_hierarchies"),
    optionRenderPermalink("option_render_permalink"),
    optionRenderOntologySummaryCloud("option_render_ontology_summary_cloud"),
    optionRenderSubExpandLinks("option_render_sub_expand_links"),
    optionRenderSubs("option_render_subs");

    private String[] altNames;


    OWLHTMLProperty(String ... altNames) {
        this.altNames = altNames;
    }


    public String[] getAlternateNames() {
        return altNames;
    }


    public static Map<String, String> generateDeprecatedNamesMap() {
        Map<String, String> map = new HashMap<String, String>();
        for (OWLHTMLProperty v : values()){
            for (String altName : v.getAlternateNames()){
                map.put(altName, v.name());
            }
        }
        return map;
    }

}
