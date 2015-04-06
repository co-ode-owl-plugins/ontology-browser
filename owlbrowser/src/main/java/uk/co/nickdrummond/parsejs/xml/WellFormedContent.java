package uk.co.nickdrummond.parsejs.xml;

import org.xml.sax.*;
import java.util.regex.*;
import uk.co.nickdrummond.parsejs.*;

class WellFormedContent extends AbstractPatternMatchResultGenerator
{
    WellFormedContent() {
        super("The content of elements must consist of well-formed character data or markup.");
    }
    
    protected AutocompleteResult createResult(final String expression, final SAXParseException e, final Matcher m) {
        if (Utils.inElement(expression)) {
            return null;
        }
        return null;
    }
}
