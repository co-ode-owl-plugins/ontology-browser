package org.coode.www.doclet;

import org.coode.html.doclet.AbstractHTMLDoclet;

import java.io.PrintWriter;
import java.net.URI;
import java.net.URL;
import java.util.Map;/*
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
 * Date: May 9, 2008<br><br>
 */
public class LoadBookmarksDoclet extends AbstractHTMLDoclet {

    private static final String ID = "doclet.ontologies.locations.bookmark";

    private String label = "Bookmarks:";


    protected void renderHeader(URL pageURL, PrintWriter out) {
        out.println("    <form style='float: right;  width: 50%;' method='POST' action='.' target='_top' >\n" +
        "        <label for='uri-bookmark'><h3 style='margin-bottom: 0;'>" + label + "</h3></label><br />\n" +
        "        <select id='uri-bookmark' name='uri' style='width:80%; margin-top: 0;'>");
    }


    protected void renderFooter(URL pageURL, PrintWriter out) {
        out.println("        </select>\n" +
        "        <input name='action' type='submit' value='load' />\n" +
        "    </form>");
    }

    public void add(final String name, final URI location){
        addDoclet(new AbstractHTMLDoclet(){
            protected void renderHeader(URL pageURL, PrintWriter out) {
                out.println("<option value='" + location + "'>" + name + " (" + location + ")");
            }
            protected void renderFooter(URL pageURL, PrintWriter out) {
                out.println("</option>");
            }
            public String getID() {
                return location.toString();
            }
        });
    }


    public void addAll(Map<String, URI> bookmarkMap) {
        for (String bm : bookmarkMap.keySet()){
            add(bm, bookmarkMap.get(bm));
        }
    }

    public void setLabel(String label){
        this.label = label;
    }

    public String getID() {
        return ID;
    }
}
