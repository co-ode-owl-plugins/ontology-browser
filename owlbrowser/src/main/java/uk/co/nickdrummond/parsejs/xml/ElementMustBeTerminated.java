package uk.co.nickdrummond.parsejs.xml;

import org.xml.sax.*;
import java.util.regex.*;
import uk.co.nickdrummond.parsejs.*;
import java.util.*;

public class ElementMustBeTerminated extends AbstractPatternMatchResultGenerator
{
    ElementMustBeTerminated() {
        super("The element type \"(.+)\" must be terminated by the matching end-tag \"(.+)\".");
    }
    
    protected AutocompleteResult createResult(final String expression, final SAXParseException e, final Matcher m) {
        final int pos = expression.lastIndexOf("/") + 1;
        final Map<String, List<String>> expected = Collections.singletonMap("Element", Collections.singletonList(m.group(1) + ">"));
        return new AutocompleteResult(expression, pos, expression.substring(pos), expected);
    }
}
