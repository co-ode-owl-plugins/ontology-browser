package org.coode.www.test;

import junit.framework.TestCase;
import org.apache.log4j.Logger;
import org.coode.html.OWLHTMLKit;
import org.coode.html.impl.OWLHTMLKitImpl;
import org.coode.html.renderer.OWLHTMLRenderer;
import org.semanticweb.owlapi.inference.OWLReasoner;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.util.NamespaceUtil;

import java.net.URI;
import java.net.URL;
import java.util.Collections;
import java.util.Set;

/**
 * Author: drummond<br>
 * The University Of Manchester<br>
 * Medical Informatics Group<br>
 * Date: Jul 4, 2006<br><br>
 * <p/>
 * nick.drummond@cs.manchester.ac.uk<br>
 * www.cs.man.ac.uk/~drummond<br><br>
 */
public class OfflineTestCase extends TestCase {

    private static final Logger logger = Logger.getLogger(OfflineTestCase.class.getName());

    public void testUnmodifiable(){
        Set s = Collections.unmodifiableSet(null);
        logger.debug("s = " + s);
    }

    public void testImports(){
        OWLOntologyManager mngr = org.semanticweb.owlapi.apibinding.OWLManager.createOWLOntologyManager();
        try {
            // B imports A
            URI a = getClass().getResource("A.owl").toURI();
            URI b = getClass().getResource("B.owl").toURI();
            mngr.loadOntologyFromPhysicalURI(b);

            assertEquals(2, mngr.getOntologies().size());

            mngr.loadOntologyFromPhysicalURI(a);

            for (OWLOntology ont : mngr.getOntologies()){
                logger.debug("ont = " + ont);
                logger.debug("classes = " + ont.getReferencedClasses());
            }

            assertEquals(2, mngr.getOntologies().size());
        }
        catch (Exception e) {
            logger.error(e);
            fail();
        }
    }

    public void testNamespaceGeneration(){
        String ontURI = "http://cohse.semanticweb.org/ontologies/people";
        String ns = new NamespaceUtil().generatePrefix(ontURI);
        assertEquals("people", ns);
    }

    public void testRenderHTMLToString(){
        try {
            URI b = getClass().getResource("B.owl").toURI();

            OWLHTMLKit kit = new OWLHTMLKitImpl("dsdsdsd", new URL("http://www.co-ode.org/ontologies/")){
                public OWLReasoner getOWLReasoner() {
                    return null;  //@@TODO implement
                }
            };
            OWLHTMLRenderer ren = new OWLHTMLRenderer(kit);
            OWLDataFactory df = kit.getOWLServer().getOWLOntologyManager().getOWLDataFactory();
            kit.getOWLServer().loadOntology(b);

            String str = ren.render(df.getOWLClass(new URI("http://www.co-ode.org/ontologies/b.owl#class1")), null);
            logger.debug("str = " + str);
        }
        catch (Exception e) {
            logger.error(e);
            fail();
        }
    }
}
