package uk.co.nickdrummond.parsejs.xml;

import java.io.*;
import org.xml.sax.*;
import java.util.*;
import uk.co.nickdrummond.parsejs.*;

public class XMLAutocompleter implements Autocompleter
{
    private List<ResultGenerator> errorPatterns;
    
    public XMLAutocompleter() {
        this.errorPatterns = Arrays.asList(new ElementMustBeTerminated(), new InvalidContentFound(), new AttributeEqualsMissing(), new WellFormedContent(), new TerminateDocumentMissing(), new DefaultResultGenerator());
    }
    
    public AutocompleteResult autocomplete(final String expression) {
        try {
            new XMLValidator("text.xsd").validate(new StringReader(this.prepare(expression)));
            throw new RuntimeException();
        }
        catch (SAXParseException e) {
            for (final ResultGenerator p : this.errorPatterns) {
                final AutocompleteResult result = p.getResult(expression, e);
                if (result != null) {
                    return result;
                }
            }
            throw new RuntimeException("Cannot handle exception", e);
        }
        catch (SAXException e2) {
            throw new RuntimeException("Problem with schema ", e2);
        }
    }
    
    private String prepare(String expression) {
        if (Utils.inElement(expression)) {
            expression += " />";
        }
        expression = expression + "</" + Utils.getStartElement(expression) + ">";
        return expression;
    }
}
