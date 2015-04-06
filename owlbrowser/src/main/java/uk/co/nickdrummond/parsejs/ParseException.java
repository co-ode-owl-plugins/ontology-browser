package uk.co.nickdrummond.parsejs;

public class ParseException extends Exception
{
    private final String expression;
    private final String message;
    private final int position;
    private final String token;
    
    public ParseException(final String expression, final String message, final int position, final String token) {
        this.expression = expression;
        this.message = message;
        this.position = position;
        this.token = token;
    }
    
    public String toString() {
        final StringBuilder sb = new StringBuilder("<?xml version=\"1.0\"?>");
        sb.append("\n<error  pos=\"").append(this.position);
        sb.append("\" found=\"");
        if (this.token.equals("<EOF>")) {
            sb.append("\" eof=\"true");
        }
        else {
            sb.append(Utils.makeXMLSafe(this.token));
        }
        sb.append("\">");
        sb.append("\n\t<expression>").append(Utils.makeXMLSafe(this.expression)).append("</expression>");
        sb.append("\n\t<message>").append(Utils.makeXMLSafe(this.message)).append("</message>");
        sb.append("\n</error>");
        return sb.toString();
    }
}
