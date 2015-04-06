package uk.co.nickdrummond.parsejs;

import java.util.*;

public class AutocompleteResult
{
    private final String expression;
    private final int position;
    private final String token;
    private final Map<String, List<String>> expected;
    
    public AutocompleteResult(final String expression, final int position, final String token, final Map<String, List<String>> expected) {
        this.expression = expression;
        this.position = position;
        this.token = token;
        this.expected = expected;
    }
    
    public String toString() {
        final StringBuilder sb = new StringBuilder("<?xml version=\"1.0\"?>");
        sb.append("\n<results  pos=\"").append(this.position);
        sb.append("\" found=\"").append(Utils.makeXMLSafe(this.token)).append("\">");
        sb.append("\n\t<expression>").append(Utils.makeXMLSafe(this.expression)).append("</expression>");
        for (final String type : this.expected.keySet()) {
            sb.append("\n\t<expected type=\"").append(Utils.makeXMLSafe(type)).append("\"");
            final List<String> matches = this.expected.get(type);
            if (!matches.isEmpty()) {
                sb.append(">");
            }
            for (final String match : matches) {
                sb.append("\n\t\t<token>").append(Utils.makeXMLSafe(match)).append("</token>");
            }
            if (matches.isEmpty()) {
                sb.append("/>");
            }
            else {
                sb.append("\n\t</expected>");
            }
        }
        sb.append("\n</results>");
        return sb.toString();
    }
}
