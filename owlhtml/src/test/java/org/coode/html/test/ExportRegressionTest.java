package org.coode.html.test;

import org.coode.html.OntologyExporter;
import org.coode.html.impl.OWLHTMLKitImpl;
import org.coode.owl.mngr.impl.OWLServerImpl;
import org.junit.Before;
import org.junit.After;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.*;

import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import static org.junit.Assert.*;

public class ExportRegressionTest {
    @Rule public TemporaryFolder temp = new TemporaryFolder();
    private OWLOntologyManager manager;
    private OWLDataFactory df;
    private OWLOntology ontology;
    private OWLServerImpl server;
    private OWLHTMLKitImpl kit;
    private final URL base;

    public ExportRegressionTest() throws Exception { base = new URL("http://example.org/docs/"); }

    @Before public void setUp() throws Exception {
        manager = OWLManager.createOWLOntologyManager();
        df = manager.getOWLDataFactory();
        ontology = manager.createOntology(IRI.create("urn:owldoc:test"));
        server = new OWLServerImpl(manager);
        server.setActiveOntology(ontology);
        kit = new OWLHTMLKitImpl("test", server, base);
    }

    @After public void tearDown() { kit.dispose(); }

    @Test public void exportsClassesPropertiesIndividualsAndRestrictions() throws Exception {
        OWLClass person = cls("Person"), pet = cls("Pet");
        OWLObjectProperty owns = df.getOWLObjectProperty(IRI.create("urn:owldoc:owns"));
        OWLDataProperty age = df.getOWLDataProperty(IRI.create("urn:owldoc:age"));
        OWLNamedIndividual alice = df.getOWLNamedIndividual(IRI.create("urn:owldoc:Alice"));
        manager.addAxiom(ontology, df.getOWLSubClassOfAxiom(person, df.getOWLObjectSomeValuesFrom(owns, pet)));
        manager.addAxiom(ontology, df.getOWLObjectPropertyDomainAxiom(owns, person));
        manager.addAxiom(ontology, df.getOWLObjectPropertyRangeAxiom(owns, pet));
        manager.addAxiom(ontology, df.getOWLClassAssertionAxiom(person, alice));
        manager.addAxiom(ontology, df.getOWLDataPropertyAssertionAxiom(age, alice, 42));
        Path output = export();
        for (OWLEntity entity : new OWLEntity[]{person, pet, owns, age, alice}) {
            assertTrue(page(output, entity).toString(), Files.exists(page(output, entity)));
        }
        String html = Files.readString(page(output, person));
        assertTrue(html, html.contains("owns"));
        assertTrue(html, html.contains("Pet"));
        assertLocalLinksExist(output);
    }

    @Test public void exportsImportedEntitiesAndTheirLinks() throws Exception {
        OWLOntology imported = manager.createOntology(IRI.create("urn:owldoc:imported"));
        OWLClass importedClass = cls("Imported");
        manager.addAxiom(imported, df.getOWLDeclarationAxiom(importedClass));
        manager.applyChange(new AddImport(ontology, df.getOWLImportsDeclaration(imported.getOntologyID().getOntologyIRI().get())));
        manager.addAxiom(ontology, df.getOWLSubClassOfAxiom(cls("Local"), importedClass));
        Path output = export();
        assertTrue(Files.exists(page(output, importedClass)));
        assertLocalLinksExist(output);
    }

    @Test public void unicodeAndReservedCharactersProduceWorkingLinks() throws Exception {
        for (String name : new String[]{"上吐下泻", "café", "A+B", "A%20B"}) {
            manager.addAxiom(ontology, df.getOWLDeclarationAxiom(cls(name)));
        }
        Path output = export();
        for (String name : new String[]{"上吐下泻", "café", "A+B", "A%20B"}) {
            assertTrue(name, Files.exists(page(output, cls(name))));
        }
        assertLocalLinksExist(output);
    }

    @Test public void preservesUnicodeAnnotationsAndEscapesMarkup() throws Exception {
        OWLClass entity = cls("Annotated");
        manager.addAxiom(ontology, df.getOWLDeclarationAxiom(entity));
        manager.addAxiom(ontology, df.getOWLAnnotationAssertionAxiom(df.getRDFSComment(), entity.getIRI(),
                df.getOWLLiteral("café 上吐下泻 <tag> & text", "fr")));
        String html = Files.readString(page(export(), entity));
        assertTrue(html, html.contains("café 上吐下泻"));
        assertTrue(html, html.contains("&lt;tag&gt;"));
        assertTrue(html, html.contains("&amp; text"));
        assertTrue(html, html.contains("@fr"));
    }

    @Test public void rendersEquivalentDisjointAndSuperProperties() throws Exception {
        OWLObjectProperty first = df.getOWLObjectProperty(IRI.create("urn:owldoc:first"));
        OWLObjectProperty equivalent = df.getOWLObjectProperty(IRI.create("urn:owldoc:equivalent"));
        OWLObjectProperty disjoint = df.getOWLObjectProperty(IRI.create("urn:owldoc:disjoint"));
        manager.addAxiom(ontology, df.getOWLEquivalentObjectPropertiesAxiom(first, equivalent));
        manager.addAxiom(ontology, df.getOWLDisjointObjectPropertiesAxiom(first, disjoint));
        OWLDataProperty data = df.getOWLDataProperty(IRI.create("urn:owldoc:data"));
        OWLDataProperty dataEquivalent = df.getOWLDataProperty(IRI.create("urn:owldoc:dataEquivalent"));
        manager.addAxiom(ontology, df.getOWLEquivalentDataPropertiesAxiom(data, dataEquivalent));
        manager.addAxiom(ontology, df.getOWLSubAnnotationPropertyOfAxiom(df.getRDFSLabel(), df.getRDFSComment()));
        Path output = export();
        String objectPage = Files.readString(page(output, first));
        assertTrue(objectPage, objectPage.contains("Equivalent Properties"));
        assertTrue(objectPage, objectPage.contains("Disjoint Properties"));
        String dataPage = Files.readString(page(output, data));
        assertTrue(dataPage, dataPage.contains("dataEquivalent"));
        assertLocalLinksExist(output);
    }

    @Test public void exportsAnonymousOntology() throws Exception {
        server.setActiveOntology(manager.createOntology());
        manager.addAxiom(server.getActiveOntology(), df.getOWLDeclarationAxiom(cls("AnonymousRoot")));
        assertLocalLinksExist(export());
    }

    @Test public void framedExportKeepsIndexesAndDetailsInTheirNamedPanes() throws Exception {
        manager.addAxiom(ontology, df.getOWLSubClassOfAxiom(cls("Child"), cls("Parent")));
        Path output = export();
        String frames = Files.readString(output.resolve("index.html"));
        assertTrue(frames.contains("name=\"nav\""));
        assertTrue(frames.contains("name=\"subnav\""));
        assertTrue(frames.contains("name=\"content\""));
        assertLinkTarget(Files.readString(output.resolve("classes/index.html")), "Child___", "content");
        assertLinkTarget(Files.readString(output.resolve("index-all.html")), "Child___", "content");
        String contents = Files.readString(output.resolve("contents.html"));
        assertLinkTarget(contents, "classes/index.html", "subnav");
        assertLinkTarget(contents, base.toURI().relativize(kit.getURLScheme().getURLForOWLObject(ontology).toURI()).toString(), "content");
        assertLinkTarget(Files.readString(page(output, cls("Child"))), "Parent___", "content");
        // Export configuration must not leak into the in-app single-pane renderer.
        assertNull(kit.getHTMLProperties().get(org.coode.html.impl.OWLHTMLProperty.optionContentWindow));
        String rendered = new org.coode.html.renderer.OWLHTMLRenderer(kit).render(cls("Child"), base);
        assertFalse(rendered, rendered.contains("target="));
    }

    private void assertLinkTarget(String html, String destination, String target) {
        Matcher anchors = Pattern.compile("<a\\b[^>]*>").matcher(html);
        boolean found = false;
        while (anchors.find()) {
            String anchor = anchors.group();
            if (anchor.contains(destination)) {
                found = true;
                assertTrue(anchor, Pattern.compile("target=[\"']" + target + "[\"']").matcher(anchor).find());
            }
        }
        assertTrue("No link to " + destination, found);
    }

    private OWLClass cls(String name) { return df.getOWLClass(IRI.create("urn:owldoc:" + name)); }
    private Path export() throws Exception {
        Path output = temp.newFolder().toPath();
        assertTrue(new OntologyExporter(kit).export(output.toFile()).isFile());
        return output;
    }
    private Path page(Path output, OWLObject entity) throws Exception {
        String relative = base.toURI().relativize(kit.getURLScheme().getURLForOWLObject(entity).toURI()).getPath();
        return output.resolve(relative);
    }
    private void assertLocalLinksExist(Path output) throws Exception {
        List<Path> pages;
        try (java.util.stream.Stream<Path> paths = Files.walk(output)) {
            pages = paths.filter(path -> path.toString().endsWith(".html")).collect(Collectors.toList());
        }
        assertFalse(pages.isEmpty());
        Pattern links = Pattern.compile("(?:href|src)=[\"']([^\"']+)[\"']");
        for (Path page : pages) {
            Matcher matcher = links.matcher(Files.readString(page));
            while (matcher.find()) {
                String target = matcher.group(1).replace("&amp;", "&");
                if (target.startsWith("#") || target.matches("[a-zA-Z]+:.*")) continue;
                java.net.URI resolved = page.toUri().resolve(target);
                Path local = Path.of(new java.net.URI(resolved.getScheme(), resolved.getAuthority(), resolved.getPath(), null, null));
                assertTrue(page + " -> " + target, Files.exists(local));
            }
        }
    }
}
