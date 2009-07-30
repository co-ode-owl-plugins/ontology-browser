package org.coode.owl.mngr.impl;

import org.coode.owl.mngr.OWLDescriptionParser;
import org.coode.owl.mngr.OWLNamedObjectFinder;
import org.coode.owl.mngr.OWLServer;
import org.coode.owl.mngr.ServerConstants;
import org.semanticweb.owl.model.*;

import java.text.ParseException;
import java.util.HashSet;
import java.util.Set;

/**
 * Author: drummond<br>
 * The University Of Manchester<br>
 * Medical Informatics Group<br>
 * Date: Jul 14, 2006<br><br>
 * <p/>
 * nick.drummond@cs.manchester.ac.uk<br>
 * www.cs.man.ac.uk/~drummond<br><br>
 *
 * There are no expression parsers in the OWL API, so this is a very simple, naive parser for a tiny subset of OWL.
 * Used for creating (nested)intersection containing named classes, some, all and hasValue restrictions with named fillers
 *
 * May revisit this to make it handle more expressivity, but for now, we don't need anything further
 *
 * @@TODO Replace when Man Syntax Parser is part of OWL API
 *
 */
public class SimpleDescriptionParser implements OWLDescriptionParser {

    private OWLServer server;

    public SimpleDescriptionParser(OWLServer server) {
        this.server = server;
    }

    public OWLDescription parse(String str) throws ParseException {
        try{
            Set<OWLDescription> descriptions = new HashSet<OWLDescription>();

            if (str != null && str.length()>0){
                for (String token: str.split(ServerConstants.INTERSECTION)){
                    descriptions.add(getDescription(token.trim()));
                }

                if (descriptions.size() > 1){
                    final OWLDataFactory df = server.getOWLOntologyManager().getOWLDataFactory();
                    return df.getOWLObjectIntersectionOf(descriptions);
                }
                else{
                    final Set<OWLClass> clses = server.getFinder().getOWLClasses(str);
                    if (clses.size()==1){
                        return clses.iterator().next();
                    }
                }
            }
        }
        catch(Exception e){
            throw new ParseException("Cannot create a description from expression: " + str, 0);
        }
        throw new ParseException("Cannot create a description from expression: " + str, 0);
    }

    public OWLDescription getDescription(String s) {
        OWLDataFactory df = server.getOWLOntologyManager().getOWLDataFactory();
        Set<OWLDescription> intersection = new HashSet<OWLDescription>();
        final OWLNamedObjectFinder finder = server.getFinder();

        for (String token: s.split(ServerConstants.INTERSECTION)){
            token = token.trim();
            if (token.startsWith(ServerConstants.OPEN_PAREN) && token.endsWith(ServerConstants.CLOSE_PAREN)){
                token = token.substring(1, token.length()-1);
                String[] tokens = token.split(" ");

                final Set<OWLObjectProperty> props = finder.getOWLObjectProperties(tokens[0].trim());
                OWLObjectProperty prop = props.iterator().next();

                if (ServerConstants.HAS_VALUE.equals(tokens[1])){
                    OWLIndividual ind = finder.getOWLIndividuals(tokens[2].trim()).iterator().next();
                    intersection.add(df.getOWLObjectValueRestriction(prop, ind));
                }
                else if (ServerConstants.SOME.equals(tokens[1])){
                    OWLClass cls = finder.getOWLClasses(tokens[2].trim()).iterator().next();
                    intersection.add(df.getOWLObjectSomeRestriction(prop, cls));
                }
                else if (ServerConstants.ONLY.equals(tokens[1])){
                    OWLClass cls = finder.getOWLClasses(tokens[2].trim()).iterator().next();
                    intersection.add(df.getOWLObjectAllRestriction(prop, cls));
                }
            }
            else{
//                logger.debug("token = " + token);
                intersection.add(finder.getOWLClasses(token).iterator().next());
            }
        }

        if (intersection.size() > 1){
            return df.getOWLObjectIntersectionOf(intersection);
        }
        else{
            return intersection.iterator().next();
        }
    }
}
