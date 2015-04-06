package uk.co.nickdrummond.parsejs.xml;

import org.xml.sax.*;
import java.util.regex.*;
import uk.co.nickdrummond.parsejs.*;
import java.util.*;

class InvalidContentFound extends AbstractPatternMatchResultGenerator
{
    private static final Pattern ONE_OF_ELEMENT;
    
    InvalidContentFound() {
        super("(.*): Invalid content was found starting with element '(.*)'\\. One of '\\{(.+)\\}' is expected\\.");
    }
    
    protected AutocompleteResult createResult(final String expression, final SAXParseException e, final Matcher m) {
        final String found = m.group(2);
        final String[] oneOf = m.group(3).split(", ");
        final List<String> expectedList = new ArrayList<String>();
        for (final String s : oneOf) {
            final Matcher m2 = InvalidContentFound.ONE_OF_ELEMENT.matcher(s);
            if (m2.matches()) {
                expectedList.add(m2.group(1));
            }
        }
        final Map<String, List<String>> expected = Collections.singletonMap("Element", expectedList);
        final int pos = expression.length() - found.length();
        return new AutocompleteResult(expression, pos, found, expected);
    }
    
    static {
        ONE_OF_ELEMENT = Pattern.compile("\".+?\":(.+)");
    }
}
