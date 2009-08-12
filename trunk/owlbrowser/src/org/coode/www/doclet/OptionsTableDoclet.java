/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.www.doclet;

import org.coode.html.OWLHTMLKit;
import org.coode.html.impl.OWLHTMLParam;
import org.coode.html.impl.OWLHTMLProperty;
import org.coode.html.impl.OWLHTMLConstants;
import org.coode.html.doclet.AbstractHTMLDoclet;
import org.coode.owl.mngr.ServerPropertiesAdapter;
import org.coode.owl.mngr.ServerProperty;

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

    private Map<OWLHTMLParam, String> params;
    private OWLHTMLKit kit;


    public OptionsTableDoclet(Map<OWLHTMLParam, String> params, OWLHTMLKit kit) {
        this.params = params;
        this.kit = kit;
    }

    public String getTitle() {
        return "Server Options";
    }

    protected void renderHeader(URL pageURL, PrintWriter out) {
        renderBoxStart(getTitle(), out);

        out.print("<div style='float: left; width: 45%;'>");
        renderProperties(kit.getHTMLProperties(), OWLHTMLProperty.values(), "Look and Feel", out);
        out.println("</div>");

        out.print("<div style='float: right; width: 45%;'>");
        renderProperties(kit.getOWLServer().getProperties(), ServerProperty.values(), "Model", out);
        out.println("</div>");
    }


    private <E extends Enum> void renderProperties(ServerPropertiesAdapter<E> properties, E[] keys, String title, PrintWriter out) {

        String lastProperty = params.get(OWLHTMLParam.property);

        out.print("<h2>");
        out.print(title);
        out.println("</h2>");
        out.println("<table style='margin-bottom: 10px;'>");
        for (E key : keys){
            String value = properties.get(key);
            String css = "";
            if (key.name().equals(lastProperty)){
                css = " style='font-weight: bolder'";
            }
            out.println("<tr" + css + ">");
            out.println("<td style='width: 250px; text-align: right;''>" + key.toString() + "</td>");
            out.println("<td>");
            out.println("<form method='POST' action='./'>");

            List<String> allowedValues = properties.getAllowedValues(key);
            if (allowedValues.isEmpty()){
                renderEditor(key, value, out);
            }
//            else if (allowedValues.contains(OWLHTMLConstants.TRUE)){
//                renderCheckbox(key, value, out);
//            }
            else{
                renderSelector(key, value, allowedValues, out);
            }
            out.println("<input type='submit' value='ok' /></form></td>");
            out.println("</tr>");
        }

        out.println("</table>");
    }


    private void renderCheckbox(Enum option, boolean value, PrintWriter out) {
        out.println("<input type='hidden' name='" + OWLHTMLParam.property + "' value='" + option.name() + "' />");

        out.print("<input type='checkbox' name='");
        out.print(OWLHTMLParam.value);
        out.print("'");
        if (value){
            out.print(" checked");
        }
        out.print(">");
    }


    private void renderEditor(Enum option, String value, PrintWriter out) {
        out.println("<input type='hidden' name='" + OWLHTMLParam.property + "' value='" + option.name() + "' />");
        out.println("<input type='text' name='" + OWLHTMLParam.value + "' value='" + value + "' style='width:80%;'/>");
    }


    private void renderSelector(Enum option, String value, List<String> allowedValues, PrintWriter out) {
        out.println("<input type='hidden' name='" + OWLHTMLParam.property + "' value='" + option.name() + "' />");
        out.println("<select style='width:80%;' name='" + OWLHTMLParam.value + "'>");
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
