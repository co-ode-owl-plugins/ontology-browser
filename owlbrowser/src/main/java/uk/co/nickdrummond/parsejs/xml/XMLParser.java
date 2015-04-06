package uk.co.nickdrummond.parsejs.xml;

import java.io.StringReader;

import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

import uk.co.nickdrummond.parsejs.ParseException;
import uk.co.nickdrummond.parsejs.ParseResult;
import uk.co.nickdrummond.parsejs.Parser;
import uk.co.nickdrummond.parsejs.Utils;

public class XMLParser implements Parser
{
    public ParseResult parse(final String expression) throws ParseException {
        try {
            final String result = new XMLValidator("text.xsd").validate(new StringReader(expression));
            return new ParseResult(expression, result);
        }
        catch (SAXParseException e) {
            throw this.transform(expression, e);
        }
        catch (SAXException e2) {
            throw new RuntimeException("SOmething wrong with schema", e2);
        }
    }
    
    private ParseException transform(final String expression, final SAXParseException e) {
        int position = Utils.getPosition(expression, e.getLineNumber(), e.getColumnNumber()) - 1;
        String token = e.getPublicId();
        if (token == null) {
            if (position > expression.length()) {
                token = "<EOF>";
            }
            else {
                final int end = position;
                position = expression.substring(0, end).lastIndexOf("<");
                token = expression.substring(position, end);
            }
        }
        return new ParseException(expression, e.getMessage(), position, token);
    }
}
