/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.www.doclet;

import org.coode.html.OWLHTMLServer;
import org.coode.html.doclet.AbstractHTMLDoclet;

import java.io.PrintWriter;
import java.net.URL;
import java.util.Map;
import java.util.List;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jan 21, 2008<br><br>
 */
public class OptionsTableDoclet extends AbstractHTMLDoclet {

    public static final String ID = "doclet.options";

    private Map<String, String> params;
    private OWLHTMLServer server;

    public OptionsTableDoclet(Map<String, String> params, OWLHTMLServer server) {
        this.params = params;
        this.server = server;
    }

    public String getTitle() {
        return "Server Options";
    }

    protected void renderHeader(URL pageURL, PrintWriter out) {
        renderBoxStart(getTitle(), out);

        out.println("<table style='margin-bottom: 10px; width: 100%;'><tr><th>Option</th><th>Value</th><th>Action</th></tr>");

        for (String option : server.getProperties().keySet()){
            String value = server.getProperties().get(option);
            String css = "";
            if (params.keySet().contains(option)){
                css = " style='font-weight: bolder'";
            }
            out.println("<tr" + css + ">");
            out.println("<td>" + option + "</td>");
            out.println("<td>");
            out.println("<form method='POST' action='./'>");

            List<String> allowedValues = server.getProperties().getAllowedValues(option);
            if (allowedValues.isEmpty()){
                renderEditor(option, value, out);
            }
            else{
                renderSelector(option, value, allowedValues, out);
            }
            out.println("<input style='display: none;' type='submit' /></form></td>");
            out.println("<td style='width: 150px;'></td>");
            out.println("</tr>");
        }

        out.println("</table>");
    }


    private void renderEditor(String option, String value, PrintWriter out) {
        out.println("<input style='width:100%;' name='" + option + "' type='text' value='" + value + "' />");
    }


    private void renderSelector(String option, String value, List<String> allowedValues, PrintWriter out) {
        out.println("<select style='width:100%;' name='" + option + "'>");
        for (String allowedValue : allowedValues){
            out.print("<option value='" + allowedValue + "'");
            if (allowedValue.equals(value)){
                out.print(" selected='selected'");
            }
            out.println(">" + allowedValue + "</option>");
        }
        out.println("</select>");
    }


    protected void renderFooter(URL pageURL, PrintWriter out) {
        renderBoxEnd(getTitle(), out);
    }

    public String getID() {
        return ID;
    }
}
