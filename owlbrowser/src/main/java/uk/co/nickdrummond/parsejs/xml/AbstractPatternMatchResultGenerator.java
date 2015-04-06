package uk.co.nickdrummond.parsejs.xml;

import org.xml.sax.*;
import uk.co.nickdrummond.parsejs.*;
import java.util.regex.*;

public abstract class AbstractPatternMatchResultGenerator implements ResultGenerator
{
    private Pattern pattern;
    
    AbstractPatternMatchResultGenerator(final String pattern) {
        this.pattern = Pattern.compile(pattern);
    }
    
    public AutocompleteResult getResult(final String expression, final SAXParseException e) {
        final Matcher m = this.pattern.matcher(e.getMessage());
        if (m.matches()) {
            return this.createResult(expression, e, m);
        }
        return null;
    }
    
    protected abstract AutocompleteResult createResult(final String p0, final SAXParseException p1, final Matcher p2);
}
