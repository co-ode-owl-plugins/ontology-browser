/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.www.doclet;

import org.apache.log4j.Logger;
import org.coode.html.OWLHTMLServer;
import org.coode.html.doclet.AbstractOWLDocDoclet;
import org.coode.html.impl.OWLHTMLConstants;
import org.coode.html.util.URLUtils;

import java.io.PrintWriter;
import java.net.URL;
import java.util.Set;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Feb 4, 2008<br><br>
 */
public class AutocompleteDoclet extends AbstractOWLDocDoclet {

    private static final Logger logger = Logger.getLogger(AutocompleteDoclet.class);


    private String id;
    private boolean multiword = false;
    private boolean autoSubmitOnAccept;
    private URL submitURL;
    private String initialValue;
    private String method = "get";
    private OWLHTMLConstants.LinkTarget target = null;
    private String paramName = "expression";
    private String submitName = "find";
    private String jsAction; //javascript to be run when page loads (will be contained in a 'delimited string so make sure it contains \" for literals )

    private String width = "200px";

    private static final String ID = "doclet.autocomplete";


    public AutocompleteDoclet(OWLHTMLServer server, String id, boolean autoSubmitOnAccept) {
        super(server);
        this.id = id;
        this.autoSubmitOnAccept = autoSubmitOnAccept;
    }

    public void setSubmitURL(URL submitURL){
        this.submitURL = submitURL;
    }

    public void setSubmitName(String submitName) {
        this.submitName = submitName;
    }

    public void setJsAction(String jsAction) {
        this.jsAction = jsAction;
    }

    public void setWidth(String width) {
        this.width = width;
    }

    public void setParamName(String paramName) {
        this.paramName = paramName;
    }

    public void setMultiword(boolean multiword) {
        this.multiword = multiword;
    }

    public void setInitialValue(String initialValue) {
        this.initialValue = initialValue;
    }

    public void setTarget(OWLHTMLConstants.LinkTarget target) {
        this.target = target;
    }

    public void setMethod(String method) {
        this.method = method;
    }

    protected void renderHeader(URL pageURL, PrintWriter out) {
        String actionStr = "";
        if (submitURL != null){
            actionStr = URLUtils.createRelativeURL(pageURL, submitURL);
        }
        out.println("<form class='autocomplete' method='" + method + "' id='chooser'" +
//                    " accept-charset='" + OWLHTMLConstants.DEFAULT_ENCODING + "'" + 
                    " name='" + id + "Form' action='" + actionStr + "'");

        if (!isSingleFrameNavigation() && target != null){
            out.print(" target='" + target + "'");
        }

        out.println("><input name='" + paramName + "' type='text'");
        if (initialValue != null && initialValue.length() > 0){
            out.println(" value='" + initialValue + "'");
        }
        out.println(" id='" + id + "' style='width: " + width + ";' />\n" +
                "        <input name='syntax' id='dlQuerySyntax' type='hidden' value='man' />"); // no harm leaving this in both

        if (jsAction != null){
            out.println("<input type='button' value='" + submitName + "' onmouseup='" + jsAction + "' />");
        }
        else{
            out.println("        <input type='submit' value='" + submitName + "' />");
        }

        out.println("</form>");
    }

    protected void renderFooter(URL pageURL, PrintWriter out) {

        String findURL = URLUtils.createRelativeURL(pageURL, getServer().getURLScheme().getURLForRelativePage("find/?format=xml&type=entities&"));

        out.print("<script type=\"text/javascript\">\n" +
                "    var options = {\n" +
                "        script: \"" + findURL + "\",\n" +
                "        varname: \"input\",\n" +
                "        cache: false");
        if (multiword){
            out.println(",\n    multiword: true");
        }
        if (autoSubmitOnAccept){
            out.println(",\n        callback: function (obj){document." + id + "Form.submit();}\n");
        }
        out.println("    };\n" +
                "    var as = new AutoSuggest(\"" + id + "\", options);\n" +
                "</script>");
    }

    public String getID() {
        return ID;
    }

    public Set<URL> getRequiredCSS() {
        Set<URL> css = super.getRequiredCSS();
        css.add(getServer().getURLScheme().getURLForRelativePage(OWLHTMLConstants.AUTO_SUGGEST_CSS));
        return css;
    }

    public Set<URL> getRequiredJS() {
        Set<URL> js = super.getRequiredJS();
        js.add(getServer().getURLScheme().getURLForRelativePage(OWLHTMLConstants.AUTO_SUGGEST_JS));
        return js;
    }
}
