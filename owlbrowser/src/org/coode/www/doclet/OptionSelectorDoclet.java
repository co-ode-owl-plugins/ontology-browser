package org.coode.www.doclet;

import org.coode.html.OWLHTMLKit;
import org.coode.html.doclet.AbstractOWLDocDoclet;
import org.coode.html.impl.OWLHTMLParam;

import java.io.PrintWriter;
import java.net.URL;
import java.util.Collections;
import java.util.List;

/**
 * Author: drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jul 27, 2010<br><br>
 */
public class OptionSelectorDoclet extends AbstractOWLDocDoclet{

    private final String option;

    private final String value;

    private final List<String> allowedValues;

    public OptionSelectorDoclet(OWLHTMLKit kit, String option, String value, List<String> allowedValues) {
        super(kit);
        this.option = option;
        this.value = value;
        this.allowedValues = allowedValues;
        Collections.sort(this.allowedValues);
    }

    @Override
    protected void renderHeader(URL pageURL, PrintWriter out) {
        out.print("\n<form id='");
        out.print(getID());
        out.println("' method='POST'>");

        out.println("<input type='hidden' name='" + OWLHTMLParam.property + "' value='" + option + "' />");

        out.print("<select name='");
        out.print(OWLHTMLParam.value);
        out.println("' onchange='optionFromSelect(this);'>");

        for (String allowedValue : allowedValues){
            out.print("<option value='" + allowedValue + "'");
            if (allowedValue.equals(value)){
                out.print(" selected='selected'");
            }
            out.println(">" + allowedValue + "</option>");
        }

        out.println("</select>");
    }

    @Override
    protected void renderFooter(URL pageURL, PrintWriter out) {
        out.println("</form>\n");
    }

    public String getID() {
        return option;
    }
}
