package org.coode.html;

import org.coode.html.impl.OWLHTMLConstants;
import org.coode.html.impl.OWLHTMLServerImpl;
import org.coode.html.index.OWLContentsHTMLPage;
import org.coode.html.index.OWLEntityIndexHTMLPage;
import org.coode.html.page.EmptyOWLDocPage;
import org.coode.html.summary.*;
import org.coode.html.url.URLScheme;
import org.coode.html.util.URLUtils;
import org.coode.html.util.FileUtils;
import org.coode.html.hierarchy.OWLClassHierarchyTreeFragment;
import org.coode.html.hierarchy.OWLPropertyHierarchyTreeFragment;
import org.coode.html.doclet.HierarchyRootDoclet;
import org.coode.owl.mngr.NamedObjectType;
import org.coode.owl.mngr.ServerConstants;
import org.semanticweb.owl.model.*;
import org.apache.log4j.Logger;

import java.io.*;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URI;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * Author: Nick Drummond<br>
 * nick.drummond@cs.manchester.ac.uk<br>
 * http://www.cs.man.ac.uk/~drummond<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jun 11, 2007<br><br>
 * <p/>
 * code made available under Mozilla Public License (http://www.mozilla.org/MPL/MPL-1.1.html)<br>
 * copyright 2006, The University of Manchester<br>
 *
 * Class that creates a set of static HTML pages in the directory given.
 * OWL entity subfolders are created for classes/properties/individuals/ontologies
 * Index pages are created for all entity folders (and overall)
 * A contents page is created
 */
public class OntologyExporter {

    private static Logger logger = Logger.getLogger(OntologyExporter.class);

    private OWLHTMLServer server;

    private File root;

    private OWLEntityIndexHTMLPage indexAllResourcesRenderer;

    private Map<NamedObjectType, OWLEntityIndexHTMLPage> typeIndices = new HashMap<NamedObjectType, OWLEntityIndexHTMLPage>();

    private final FileUtils fileUtils;


    public static void main(String[] args) {
        try {
            // the default base should never be exposed in the output because all URLs should be relative
            URL defaultBase = new URL("http://www.co-ode.org/ontologies/owldoc/");

            OWLHTMLServer server = new OWLHTMLServerImpl("server", defaultBase);

            String out = null;

            for (String arg : args){
                String[] argPair = arg.split("=");
                if (argPair.length == 1){
                    if (argPair[0].equals("-t")){
                        logger.info("Switching mini hierarchies on");
                        server.getProperties().set(OWLHTMLConstants.OPTION_SHOW_MINI_HIERARCHIES, ServerConstants.TRUE);
                    }
                    else if (argPair[0].equals("-l")){
                        logger.info("Rendering labels");
                        server.getProperties().set(OWLHTMLConstants.OPTION_REN, ServerConstants.RENDERER_LABEL);
                    }
                    else if (argPair[0].equals("-c")){
                        logger.info("Switching ontology summary cloud on");
                        server.getProperties().set(OWLHTMLConstants.OPTION_RENDER_ONTOLOGY_SUMMARY_CLOUD, ServerConstants.TRUE);
                    }
                    else{
                        // must be an ontology file
                        URI ontLoc = new File(argPair[0]).toURI();
                        logger.info("Loading ontology: " + ontLoc);
                        server.loadOntology(ontLoc);
                    }
                }
                else{
                    if (argPair[0].equals("out")){
                        out = argPair[1];
                    }
                }
            }

            if (out == null){
                out = "owldoc";
            }
            File outputDirectory = new File(out);
            if (!outputDirectory.exists()){
                outputDirectory.mkdir();
            }

            if (!outputDirectory.isDirectory()){
                logger.fatal("The output location specified is not a directory");
                System.exit(1);
            }
            if (server.getOntologies().isEmpty()){
                logger.fatal("No ontologies loaded!");
                System.exit(1);
            }

            OntologyExporter exporter = new OntologyExporter(server);

            logger.info("Compiling owldoc into directory: " + outputDirectory);

            exporter.export(outputDirectory);

            logger.info("Done!");
        }
        catch (Exception e) {
            logger.error("Problem exporting OWLDoc", e);
        }

        System.exit(0);
    }


    public OntologyExporter(OWLHTMLServer server) {
        this.server = server;
        this.fileUtils = new FileUtils("resources/", "UTF8");
    }

    public File export(File root) throws Exception {
        if (root.isDirectory()){
            this.root = root;

            copyResource(server.getProperties().get(OWLHTMLConstants.OPTION_DEFAULT_CSS), root);
            copyResource(OWLHTMLConstants.JS_TREE, root);

            // initialise the all resources index
            indexAllResourcesRenderer = new OWLEntityIndexHTMLPage(server);
            indexAllResourcesRenderer.setTitle(OWLHTMLConstants.ALL_ENTITIES_TITLE);

            // step through the ontologies
            for (OWLOntology ont : server.getVisibleOntologies()){
                exportOntology(ont);
            }

            createIndices();

            createAllResourcesIndex();

            createContents();

            return createHTMLFrames();
        }
        else{
            throw new Exception("Root is not a directory: " + root);
        }
    }


    private void copyResource(String resourceName, File root) throws IOException {
        logger.debug("copying... " + resourceName);
        InputStream in = server.getClass().getClassLoader().getResourceAsStream("resources/" + resourceName);
        File out = new File(root, resourceName);
        fileUtils.saveFile(in, out);
    }


    private void exportOntology(OWLOntology ont) throws IOException {

        // export the ontology summary
        File subDirFile = new File(root, NamedObjectType.ontologies.toString());
        if (!subDirFile.exists()){
            subDirFile.mkdir();
        }
        OWLOntologySummaryHTMLPage ontologySummary = new OWLOntologySummaryHTMLPage(server);
        logger.debug("ont = " + ont);
        ontologySummary.setUserObject(ont);
        String ontID = server.getNameRenderer().getShortForm(ont);
        URL pageURL = server.getURLScheme().getURLForNamedObject(ont);
        PrintWriter out = fileUtils.open(new File(subDirFile, ontID + OWLHTMLConstants.DEFAULT_EXTENSION));
        ontologySummary.renderAll(pageURL, out);

        out.flush();
        out.close();
        
        OWLEntityIndexHTMLPage ontologyIndexRenderer = getTypeIndexRenderer(NamedObjectType.ontologies);
        ontologyIndexRenderer.add(ont);

        exportReferencedEntities(ont);
    }

    private void exportReferencedEntities(OWLOntology ont) throws IOException {

        final OWLClassSummaryHTMLPage clsSummary = new OWLClassSummaryHTMLPage(server);
        final OWLObjectPropertySummaryHTMLPage objPropSummary = new OWLObjectPropertySummaryHTMLPage(server);
        final OWLDataPropertySummaryHTMLPage dataPropSummary = new OWLDataPropertySummaryHTMLPage(server);
        final OWLIndividualSummaryHTMLPage indSummary = new OWLIndividualSummaryHTMLPage(server);

        if (server.getProperties().isSet(OWLHTMLConstants.OPTION_SHOW_MINI_HIERARCHIES)){
            final OWLClassHierarchyTreeFragment clsTreeModel = new OWLClassHierarchyTreeFragment(server, server.getClassHierarchyProvider(), "Asserted Class Hierarchy");
            clsSummary.setOWLHierarchyRenderer(new HierarchyRootDoclet<OWLClass>(server, clsTreeModel));

            final OWLPropertyHierarchyTreeFragment<OWLObjectProperty> objPropTreeModel = new OWLPropertyHierarchyTreeFragment<OWLObjectProperty>(server, server.getPropertyHierarchyProvider());
            objPropSummary.setOWLHierarchyRenderer(new HierarchyRootDoclet<OWLObjectProperty>(server, objPropTreeModel));

            final OWLPropertyHierarchyTreeFragment<OWLDataProperty> dataPropTreeModel = new OWLPropertyHierarchyTreeFragment<OWLDataProperty>(server, server.getPropertyHierarchyProvider());
            dataPropSummary.setOWLHierarchyRenderer(new HierarchyRootDoclet<OWLDataProperty>(server, dataPropTreeModel));
        }

        // export classes
        exportReferencedEntities(NamedObjectType.classes,
                                 clsSummary,
                                 getReferencedClasses(ont),
                                 ont);

        // export object properties
        exportReferencedEntities(NamedObjectType.objectproperties,
                                 objPropSummary,
                                 ont.getReferencedObjectProperties(),
                                 ont);

        // export data properties
        exportReferencedEntities(NamedObjectType.dataproperties,
                                 dataPropSummary,
                                 ont.getReferencedDataProperties(),
                                 ont);

        // export individuals
        exportReferencedEntities(NamedObjectType.individuals,
                                 indSummary,
                                 ont.getReferencedIndividuals(),
                                 ont);
    }

    private void exportReferencedEntities(NamedObjectType type, EmptyOWLDocPage ren,
                                          Set<? extends OWLEntity>entities, OWLOntology ont) throws IOException {
        if (!entities.isEmpty()){

            File subDirFile = new File(root, type.toString());
            if (!subDirFile.exists()){
                subDirFile.mkdir();
            }

            // index generation for current ontology
            final URLScheme urlScheme = server.getURLScheme();
            String filename = urlScheme.getFilenameForOntologyIndex(ont, type);
            PrintWriter indexWriter = fileUtils.open(new File(subDirFile, filename));
            URL indexBaseURL = urlScheme.getURLForOntologyIndex(ont, type);
            OWLEntityIndexHTMLPage ontIndexRenderer = new OWLEntityIndexHTMLPage(server);
            ontIndexRenderer.setTitle(server.getNameRenderer().getShortForm(ont) + ": " + type);

            // index generation for all ontologies
            OWLEntityIndexHTMLPage indexAllRenderer = getTypeIndexRenderer(type);

            for (OWLEntity entity : entities){

                // add to ontology index
                ontIndexRenderer.add(entity);

                // add to all index
                indexAllRenderer.add(entity);

                // add to all resources index
                indexAllResourcesRenderer.add(entity);

                exportEntity(entity, ren);
            }

            ontIndexRenderer.renderAll(indexBaseURL, indexWriter);
            indexWriter.flush();
            indexWriter.close();
        }
    }

    private void exportEntity(OWLEntity entity, EmptyOWLDocPage ren) throws IOException {
        ren.setUserObject(entity);
        if (entity.getURI().toString().contains("hasIn")){
            System.out.println("entity = " + entity);
        }
        URL entityURL = server.getURLScheme().getURLForNamedObject(entity);
        String localFilename = URLUtils.createRelativeURL(server.getBaseURL(), entityURL);
        File entitySummaryFile = getFileAndEnsurePathExists(root, localFilename);
        Writer fileWriter = new FileWriter(entitySummaryFile);
        PrintWriter writer = new PrintWriter(fileWriter);
        ren.renderAll(entityURL, writer);
        fileWriter.close();
    }

    private File getFileAndEnsurePathExists(File dir, String path) {
        String[] elements = path.split(File.pathSeparator);
        if (elements.length > 1){
            File subDirFile = new File(dir, elements[0]);
            if (!subDirFile.exists()){
                subDirFile.mkdir();
            }
            String subpath = path.substring(elements[0].length()+1);
            return getFileAndEnsurePathExists(subDirFile, subpath);
        }
        else{
            return new File(dir, path);
        }
    }

    private OWLEntityIndexHTMLPage getTypeIndexRenderer(NamedObjectType type) throws MalformedURLException {
        OWLEntityIndexHTMLPage indexAllRenderer = typeIndices.get(type);
        if (indexAllRenderer == null){
            indexAllRenderer = new OWLEntityIndexHTMLPage(server);
            indexAllRenderer.setTitle("All " +
                    type.toString().substring(0, 1).toUpperCase() +
                    type.toString().substring(1).toLowerCase());
            typeIndices.put(type, indexAllRenderer);
        }
        return indexAllRenderer;
    }

    private void exportHTMLFrames(PrintWriter out) {

        final URL ontURL = server.getURLScheme().getURLForNamedObject(server.getActiveOntology());
        String activeOntologyPage = URLUtils.createRelativeURL(server.getBaseURL(), ontURL);

        out.print("<html>\n" +
                "<head>\n" +
                "<title>\n" +
                "OWLDoc\n" +
                "</title>\n" +
                "</head>\n" +
                "<frameset cols=\"30%,70%\">\n" +
                "    <frameset rows=\"30%,70%\">\n");
        out.print("<frame src=\"" + OWLHTMLConstants.CONTENTS_HTML + "\" name=\"nav\" title=\"Contents\"/>\n");
        out.print("<frame src=\"" + server.getProperties().get(OWLHTMLConstants.OPTION_INDEX_ALL_URL) + "\" name=\"subnav\" title=\"Index\"/>\n");
        out.print("    </frameset>\n");
        out.print("    <frame src=\"" + activeOntologyPage + "\" name=\"content\" title=\"Content\"/>\n");
        out.print("    <noframes>classes/index.html</noframes>\n" +
                "</frameset>\n" +
                "</html>");
    }

    private void createIndices() throws IOException {
        for (NamedObjectType type : typeIndices.keySet()){
            PrintWriter writer = fileUtils.open(new File(root, type + "/" + OWLHTMLConstants.INDEX_HTML));
            URL pageURL = server.getURLScheme().getURLForIndex(type);
            typeIndices.get(type).renderAll(pageURL, writer);
            writer.flush();
            writer.close();
        }
    }

    private void createAllResourcesIndex() throws IOException {
        // by this point we've accumulated the resources into the renderer
        String indexFile = server.getProperties().get(OWLHTMLConstants.OPTION_INDEX_ALL_URL);
        PrintWriter indexAllWriter = fileUtils.open(new File(root, indexFile));
        URL pageURL = server.getURLScheme().getURLForRelativePage(indexFile);
        indexAllResourcesRenderer.renderAll(pageURL, indexAllWriter);
        indexAllWriter.flush();
        indexAllWriter.close();
    }

    private void createContents() throws IOException {
        OWLContentsHTMLPage contentsRenderer = new OWLContentsHTMLPage(server);
        PrintWriter contentsWriter = fileUtils.open(new File(root, OWLHTMLConstants.CONTENTS_HTML));
        URL pageURL = server.getURLScheme().getURLForRelativePage(OWLHTMLConstants.CONTENTS_HTML);
        contentsRenderer.renderAll(pageURL, contentsWriter);
        contentsWriter.flush();
        contentsWriter.close();
    }

    private File createHTMLFrames() throws IOException {
        final File indexFile = new File(root, OWLHTMLConstants.INDEX_HTML);
        PrintWriter htmlFramesWriter = fileUtils.open(indexFile);
        exportHTMLFrames(htmlFramesWriter);
        htmlFramesWriter.flush();
        htmlFramesWriter.close();
        return indexFile;
    }


    // get all classes (including owl:Thing)
    private Set<OWLClass> getReferencedClasses(OWLOntology ont) {
        Set<OWLClass> referencedClasses = new HashSet<OWLClass>();
        referencedClasses.addAll(ont.getReferencedClasses());
        referencedClasses.add(server.getOWLOntologyManager().getOWLDataFactory().getOWLThing());
        return referencedClasses;
    }
}
