package uk.co.nickdrummond.parsejs.xml;

import org.xml.sax.*;
import uk.co.nickdrummond.parsejs.*;
import java.util.*;

public class DefaultResultGenerator implements ResultGenerator
{
    public AutocompleteResult getResult(final String expression, final SAXParseException e) {
        System.out.println("Default result generator");
        System.out.println(e.getMessage());
        int position = Utils.getPosition(expression, e.getLineNumber(), e.getColumnNumber()) - 1;
        String token = e.getPublicId();
        if (token != null) {
            System.out.println("token = " + token);
        }
        else if (position > expression.length()) {
            token = "<EOF>";
        }
        else {
            final int end = position;
            position = expression.substring(0, end).lastIndexOf("<");
            token = expression.substring(position, end);
        }
        final Map<String, List<String>> expected = new HashMap<String, List<String>>();
        return new AutocompleteResult(expression, expression.length() - token.length(), token, expected);
    }
}
