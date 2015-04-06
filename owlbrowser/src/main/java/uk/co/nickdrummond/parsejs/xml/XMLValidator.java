package uk.co.nickdrummond.parsejs.xml;

import org.xml.sax.helpers.*;
import org.xml.sax.*;
import java.io.*;

public class XMLValidator
{
    public static final String XML_SCHEMA_NS = "http://www.w3.org/2001/XMLSchema";
    private XMLReader xmlReader;
    
    public XMLValidator(final String schemaLoc) throws SAXException {
        (this.xmlReader = XMLReaderFactory.createXMLReader()).setFeature("http://xml.org/sax/features/validation", true);
        this.xmlReader.setFeature("http://apache.org/xml/features/validation/dynamic", true);
        this.xmlReader.setFeature("http://apache.org/xml/features/validation/schema", true);
        this.xmlReader.setErrorHandler(new ErrorHandler() {
            public void warning(final SAXParseException exception) throws SAXException {
                throw exception;
            }
            
            public void error(final SAXParseException exception) throws SAXException {
                throw exception;
            }
            
            public void fatalError(final SAXParseException exception) throws SAXException {
                throw exception;
            }
        });
    }
    
    public String validate(final Reader reader) throws SAXParseException {
        try {
            final InputSource inputSource = new InputSource(reader);
            this.xmlReader.parse(inputSource);
            return "parsed XML";
        }
        catch (SAXException e) {
            if (e instanceof SAXParseException) {
                throw (SAXParseException)e;
            }
            e.printStackTrace();
        }
        catch (IOException e2) {
            e2.printStackTrace();
        }
        return "Some other error occured";
    }
}
