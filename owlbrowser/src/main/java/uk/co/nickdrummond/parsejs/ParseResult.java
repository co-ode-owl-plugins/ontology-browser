package uk.co.nickdrummond.parsejs;

public class ParseResult
{
    private String expression;
    private String message;
    
    public ParseResult(final String expression, final String message) {
        this.expression = expression;
        this.message = message;
    }
    
    public String toString() {
        return "<?xml version=\"1.0\"?>\n<success>\n\t<expression>" + Utils.makeXMLSafe(this.expression) + "</expression>" + "\n\t<message>" + Utils.makeXMLSafe(this.message) + "</message>" + "\n</success>";
    }
}
