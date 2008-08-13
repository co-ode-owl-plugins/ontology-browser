/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.owl.mngr.impl;

import org.coode.manchesterowlsyntax.ManchesterOWLSyntaxEditorParser;
import org.coode.owl.mngr.OWLDescriptionParser;
import org.coode.owl.mngr.OWLServer;
import org.semanticweb.owl.expression.ParserException;
import org.semanticweb.owl.expression.ShortFormEntityChecker;
import org.semanticweb.owl.model.OWLDataFactory;
import org.semanticweb.owl.model.OWLDescription;
import org.semanticweb.owl.model.OWLOntologyManager;
import org.semanticweb.owl.util.BidirectionalShortFormProvider;
import org.semanticweb.owl.util.BidirectionalShortFormProviderAdapter;
import org.semanticweb.owl.util.NamespaceUtil;
import org.semanticweb.owl.util.ShortFormProvider;

import java.text.ParseException;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jan 11, 2008<br><br>
 */
public class ManchesterOWLSyntaxParser implements OWLDescriptionParser {

    private OWLServer server;

    // @@TODO this should probably be shared (ie the server should have it) so that renderers can use it too
    private NamespaceUtil nsUtil;

    
    public ManchesterOWLSyntaxParser(OWLServer server) {
        this.server = server;
        nsUtil = new NamespaceUtil();
    }


    public OWLDescription parse(String str) throws ParseException {
        try {
            return getParser(str).parseDescription();
        }
        catch (ParserException e) {
            throw new ParseException(e.getMessage(), e.getStartPos());
        }
    }


    private ManchesterOWLSyntaxEditorParser getParser(String expression){
        final OWLOntologyManager ontMngr = server.getOWLOntologyManager();

        ShortFormProvider sfProvider = server.getNameRenderer();

        final BidirectionalShortFormProvider providerAdapter =
                new BidirectionalShortFormProviderAdapter(ontMngr, server.getActiveOntologies(), sfProvider);

        ShortFormEntityChecker checker = new ShortFormEntityChecker(providerAdapter);

        final OWLDataFactory df = ontMngr.getOWLDataFactory();

        ManchesterOWLSyntaxEditorParser parser = new ManchesterOWLSyntaxEditorParser(df, expression);

        parser.setOWLEntityChecker(checker);

        return parser;
    }
}
