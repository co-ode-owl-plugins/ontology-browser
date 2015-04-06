package uk.co.nickdrummond.parsejs;

import java.util.regex.*;

public class Utils
{
    private static final Pattern START_ELEMENT;
    
    public static String makeXMLSafe(String s) {
        s = s.replaceAll("&", "&amp;");
        s = s.replaceAll(">", "&gt;");
        s = s.replaceAll("<", "&lt;");
        s = s.replaceAll("\"", "&quot;");
        return s;
    }
    
    public static int getPosition(final String expression, final int line, final int col) {
        int loc = 0;
        for (int i = 1; i < line; ++i) {
            loc = expression.indexOf("\n", loc) + 1;
        }
        return loc + col;
    }
    
    public static String getStartElement(final String expression) {
        final Matcher m2 = Utils.START_ELEMENT.matcher(expression);
        if (m2.matches()) {
            return m2.group(1);
        }
        return null;
    }
    
    public static boolean inElement(final String expression) {
        return expression.lastIndexOf(">") < expression.lastIndexOf("<");
    }
    
    static {
        START_ELEMENT = Pattern.compile("<\\?.*\\?>\\s?<(.+)\\s.*");
    }
}
