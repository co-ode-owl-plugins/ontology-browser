package uk.co.nickdrummond.parsejs.xml;

import org.xml.sax.*;
import java.util.regex.*;
import uk.co.nickdrummond.parsejs.*;
import java.util.*;

class TerminateDocumentMissing extends AbstractPatternMatchResultGenerator
{
    TerminateDocumentMissing() {
        super("XML document structures must start and end within the same entity.");
    }
    
    protected AutocompleteResult createResult(final String expression, final SAXParseException e, final Matcher m) {
        final String startElement = Utils.getStartElement(expression);
        if (startElement != null) {
            final String endElement = "</" + startElement + ">";
            final Map<String, List<String>> expected = Collections.singletonMap("Element", Collections.singletonList(endElement));
            return new AutocompleteResult(expression, expression.length(), "<EOF>", expected);
        }
        throw new RuntimeException("Cannot find start element");
    }
}
