package uk.co.nickdrummond.parsejs.xml;

import org.xml.sax.*;
import uk.co.nickdrummond.parsejs.*;

public interface ResultGenerator
{
    AutocompleteResult getResult(String p0, SAXParseException p1);
}
