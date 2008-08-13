/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.www;

import org.apache.log4j.Logger;
import org.coode.html.OWLHTMLServer;
import org.coode.owl.util.ModelUtil;
import org.semanticweb.owl.inference.OWLReasoner;
import org.semanticweb.owl.inference.OWLReasonerException;
import org.semanticweb.owl.model.OWLDescription;
import org.semanticweb.owl.model.OWLEntity;

import java.util.HashSet;
import java.util.Set;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Feb 6, 2008<br><br>
 */
public enum QueryType {
    equivalents, subclasses, descendants, superclasses, ancestors, instances;

    private Logger logger = Logger.getLogger(QueryType.class);

    public Set<OWLEntity> getResults(OWLDescription descr, OWLHTMLServer server) {
        Set<OWLEntity> results = new HashSet<OWLEntity>();
        try{
            final OWLReasoner r = server.getOWLReasoner();
            switch(this){
                case equivalents: results.addAll(r.getEquivalentClasses(descr)); break;
                case subclasses: results.addAll(ModelUtil.filterClasses(r.getSubClasses(descr), server)); break;
                case descendants: results.addAll(ModelUtil.filterClasses(r.getDescendantClasses(descr), server)); break;
                case superclasses: results.addAll(ModelUtil.filterClasses(r.getSuperClasses(descr), server)); break;
                case ancestors: results.addAll(ModelUtil.filterClasses(r.getAncestorClasses(descr), server)); break;
                case instances: results.addAll(r.getIndividuals(descr, false)); break;
            }
        }
        catch(OWLReasonerException e){
            logger.error("Reasoner error", e);
        }
        return results;
    }

}
