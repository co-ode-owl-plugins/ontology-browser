package uk.co.nickdrummond.parsejs.xml;

import org.xml.sax.*;
import java.util.regex.*;
import uk.co.nickdrummond.parsejs.*;
import java.util.*;

public class AttributeEqualsMissing extends AbstractPatternMatchResultGenerator
{
    AttributeEqualsMissing() {
        super("Attribute name \"(.+)\" associated with an element type \"(.+)\" must be followed by the ' = ' character.");
    }
    
    protected AutocompleteResult createResult(final String expression, final SAXParseException e, final Matcher m) {
        final Map<String, List<String>> expected = Collections.singletonMap("Seperator", Collections.singletonList("=\""));
        return new AutocompleteResult(expression, expression.length(), "<EOF>", expected);
    }
}
