package org.coode.www.mngr;

import org.apache.log4j.Logger;
import org.coode.html.OWLHTMLServer;
import org.coode.html.impl.OWLHTMLConstants;
import org.coode.html.impl.OWLHTMLServerImpl;
import org.coode.html.url.RestURLScheme;
import org.coode.owl.mngr.OWLServer;
import org.coode.owl.mngr.ServerConstants;
import org.coode.owl.mngr.ServerProperties;
import org.coode.owl.mngr.impl.ManchesterOWLSyntaxParser;
import org.coode.suggestor.api.SuggestorManager;
import org.coode.www.OntologyBrowserConstants;
import org.coode.www.exception.OntServerException;
import org.coode.www.query.QuickDescriptionParser;
import org.coode.www.servlet.ManageOntologies;
import org.semanticweb.owl.model.OWLOntologyManager;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;
import javax.servlet.http.HttpSessionBindingEvent;
import javax.servlet.http.HttpSessionBindingListener;
import java.io.*;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.text.ParseException;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;


/**
 * Author: Nick Drummond<br>
 * nick.drummond@cs.manchester.ac.uk<br>
 * http://www.cs.man.ac.uk/~drummond<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jun 28, 2007<br><br>
 * <p/>
 * code made available under Mozilla Public License (http://www.mozilla.org/MPL/MPL-1.1.html)<br>
 * copyright 2006, The University of Manchester<br>
 *
 * For a given key we will have completely independent ontology managers running with seperate reasoners etc
 * as each will be handling different ontologies (each session is independent).
 *
 * can 2 sessions ever use the same reasoner if the ontology loaded is the same? Probably not as we don't
 * know this is the same version of the same ontology (as changes are not forbidden)
 */
public class SessionManager {

    private static Logger logger = Logger.getLogger(SessionManager.class.getName());

    private static final String ID = "ID";

    private static Map<SessionID, OWLHTMLServer> activeServers = new HashMap<SessionID, OWLHTMLServer>();
    private static Map<String, SuggestorManager> activeSuggestorManagers = new HashMap<String, SuggestorManager>();
    private static final String URI_MAPPING_MARKER = "##";


    /**
     * Get a server (creates a new one if you have a new session)
     * @param request
     * @return
     * @throws OntServerException
     */
    public synchronized static OWLHTMLServer getServer(HttpServletRequest request) throws OntServerException {
        HttpSession session = request.getSession(true);

        if (session.isNew()){
            create(session, request);
        }

        SessionID id = (SessionID)session.getAttribute(ID);
        return activeServers.get(id);
    }


    /**
     * Copy a server that has been created/saved by another user - dumps all current state
     * @param request
     * @param label
     * @return
     * @throws OntServerException
     */
    public synchronized static OWLHTMLServer getServer(HttpServletRequest request, String label) throws OntServerException {
        OWLHTMLServer server = getServer(request);
        if (label != null && !label.equals(server.getCurrentLabel())){
            loadServerState(server, label);
        }
        return server;
    }


    /**
     * Persist the current loaded ontologies with their mappings
     * @param server note: will set the current label on the server
     * @throws OntServerException
     */
    public synchronized static void labelServerState(OWLHTMLServer server) throws OntServerException {
        String label = server.getID() + "-" + createID();
        File file = getFile(label + OntologyBrowserConstants.SERVER_STATES_EXT);
        try {
            OutputStream out = new FileOutputStream(file);
            PrintWriter writer = new PrintWriter(out);
            Map<URI, URI> ontologyMappings = ManageOntologies.getMap(server);
            Set<URI> onts = ontologyMappings.keySet();

            server.getProperties().save(out);

            // always print the active ontology first
            URI activeOnt = server.getActiveOntology().getURI();
            writer.println(URI_MAPPING_MARKER + activeOnt + "=" + ontologyMappings.get(activeOnt));

            onts.remove(activeOnt);
            for (URI ont : onts){
                writer.println(URI_MAPPING_MARKER + ont + "=" + ontologyMappings.get(ont));
            }

            writer.flush();
            out.flush();

            writer.close();
            out.close();

            server.setCurrentLabel(label);
            logger.debug("server state saved at: " + file.getAbsolutePath());
        }
        catch (IOException e) {
            throw new OntServerException(e);
        }
    }


    /**
     * Clears the given server and replaces its state with that specified by the label given
     * @param server
     * @param label
     * @throws OntServerException
     */
    private synchronized static void loadServerState(OWLHTMLServer server, String label) throws OntServerException {

        server.clearOntologies(); // dump all ontologies and caches

        try {
            File file = getFile(label + OntologyBrowserConstants.SERVER_STATES_EXT);

            // we are currently reading the file twice - @@TODO make this much nicer

            // pass 1 to get the properties
            BufferedInputStream in = new BufferedInputStream(new FileInputStream(file));
            server.getProperties().load(in);
            in.close();

            // pass 2 to get the ontology mappings
            BufferedReader reader = new BufferedReader(new FileReader(file));
            int currentLine = 0;
            try{
                String line;
                while ((line = reader.readLine()) != null){
                    if (line.startsWith(URI_MAPPING_MARKER)){
                        line = line.substring(URI_MAPPING_MARKER.length(), line.length());
                        String[] param = line.split("=");
                        URI ontURI = new URI(param[0].trim());
                        URI physicalURI = new URI(param[1].trim());
                        if (ontURI.isAbsolute() && physicalURI.isAbsolute()){
                            server.loadOntology(physicalURI); // auto set active ont to the first
                        }
                    }
                    currentLine++;
                }

                server.setCurrentLabel(label);
            }
            catch (URISyntaxException e) {
                throw new ParseException("Invalid URI in content: " + e.getInput(), currentLine);
            }
            finally{
                reader.close();
            }
        }
        catch (Exception e) {
            throw new OntServerException(e);
        }
    }


    public static File getFile(String name) {
        File cacheDir = new File(OntologyBrowserConstants.SERVER_STATES_DIR);
        if (!cacheDir.exists()){
            cacheDir.mkdir();
        }
        return new File(OntologyBrowserConstants.SERVER_STATES_DIR + name);
    }

    /**
     * One suggestor per server
     * @param server
     * @return
     */
    public synchronized static SuggestorManager getSuggestorManager(OWLServer server) {
        final String id = server.getID();
        SuggestorManager sm = activeSuggestorManagers.get(id);
        if (sm == null){
            sm = new SuggestorManagerAdapter(server);
            activeSuggestorManagers.put(id, sm);
        }
        return sm;
    }

    public synchronized static void closeSession(HttpSession mySession) {
        if (mySession != null){
            SessionID id = (SessionID) mySession.getAttribute(ID);
            cleanupSession(id);
            mySession.invalidate();
        }
    }

    public synchronized static void cleanupSession(SessionID id) {

        OWLServer server = activeServers.remove(id);
        if (server != null){ // might already have been tidied away
            final String serverID = server.getID();
            server.dispose();

            SuggestorManager sm = activeSuggestorManagers.remove(serverID);
            if (sm != null){
                sm.dispose();
            }

            logger.debug("(killed) active servers: " + activeServers.size());
            logger.debug("(killed) active suggestor managers: " + activeSuggestorManagers.size());

            System.gc();
            System.gc();
        }
    }

    private synchronized static void create(HttpSession mySession, HttpServletRequest request) {
        try{
            StringBuffer fullURL = request.getRequestURL();
            int ontServerIndex = fullURL.indexOf(OWLHTMLConstants.ONTOLOGY_SERVER);
            URL basePath = new URL(fullURL.substring(0, ontServerIndex + OWLHTMLConstants.ONTOLOGY_SERVER.length()));

            SessionID id = new SessionID();

            OWLHTMLServer server = createServer(id.id, basePath);

            activeServers.put(id, server);
            mySession.setAttribute(ID, id);
        }
        catch (MalformedURLException e) {
            logger.error(e);
        }
        logger.debug("active servers: " + activeServers.size());
        logger.debug("active suggestor managers: " + activeSuggestorManagers.size());
    }

    private static OWLHTMLServer createServer(String id, URL basePath) {
        OWLOntologyManager mngr = org.semanticweb.owl.apibinding.OWLManager.createOWLOntologyManager();

        // set silent error handling for missing imports
        mngr.setSilentMissingImportsHandling(true);
//        mngr.addMissingImportListener(missingImportsListener);

        OWLHTMLServer server = new OWLHTMLServerImpl(id, mngr, basePath);

        // use a servlet URL scheme which encodes the names in params
        server.setURLScheme(new RestURLScheme(server));

        // register parsers
        server.registerDescriptionParser(ServerConstants.Syntax.man.toString(), new ManchesterOWLSyntaxParser(server));
        server.registerDescriptionParser(ServerConstants.Syntax.qd.toString(), new QuickDescriptionParser(server));

        boolean defaultsLoaded = false;

        File file = getFile("default" + OntologyBrowserConstants.SERVER_STATES_EXT);
        if (file.exists()){
            try {
                BufferedInputStream in = new BufferedInputStream(new FileInputStream(file));
                server.getProperties().load(in);
                in.close();
                defaultsLoaded = true;
            }
            catch (IOException e) {
                logger.error("Could not load default properties");
            }
        }

        if (!defaultsLoaded){

            setupDefaultServerProperties(server);

            try {
                OutputStream out = new FileOutputStream(file);
                server.getProperties().save(out);
            }
            catch (IOException e) {
                logger.error("Could not save default properties");
            }
        }

        return server;
    }


    private static void setupDefaultServerProperties(OWLHTMLServer server) {
        ServerProperties properties = server.getProperties();

        // by default, do not use frames navigation
        properties.set(OWLHTMLConstants.OPTION_CONTENT_WINDOW, null);

//        // a top ontology is created (with a top level property etc)
//        properties.set(ServerConstants.OPTION_CREATE_TOP_ONTOLOGY, ServerConstants.TRUE);

        // the default entities index is at the location "entities/"
        properties.set(OWLHTMLConstants.OPTION_INDEX_ALL_URL, "entities/");

        // make sure the reasoner is enabled to allow dl query etc
        properties.set(OWLHTMLConstants.OPTION_REASONER_ENABLED, ServerConstants.TRUE);

        // default to factplusplus
        properties.set(ServerConstants.OPTION_REASONER, ServerConstants.PELLET);

        // render a permalink
        properties.set(OWLHTMLConstants.OPTION_RENDER_PERMALINK, ServerConstants.TRUE);

        // default location for DIG reasoner
        if (server.getBaseURL().toString().contains("localhost")){ // just to make sure I don't accidentally publish this address
            properties.set(ServerConstants.OPTION_DIG_REASONER_URL, "http://rpc295.cs.man.ac.uk:8080");
        }

        properties.set(OWLHTMLConstants.OPTION_SHOW_MINI_HIERARCHIES, ServerConstants.TRUE);

        properties.set(OWLHTMLConstants.OPTION_SHOW_INFERRED_HIERARCHIES, ServerConstants.FALSE);

        properties.set(ServerConstants.OPTION_RENDER_SUB_EXPAND_LINKS, ServerConstants.FALSE);

        properties.set(ServerConstants.OPTION_LABEL_LANG, "");
    }


    /**
     * The ID will get notified when a session expires and calls the session manager to cleanup
     */
    static class SessionID implements HttpSessionBindingListener, java.io.Serializable {
        private static Integer i = 0;

        private String id;

        public SessionID() {
            this.id = createID() + "-" + i++;
        }

        public void valueBound(HttpSessionBindingEvent httpSessionBindingEvent) {
        }

        public void valueUnbound(HttpSessionBindingEvent httpSessionBindingEvent) {
            logger.debug("do unbind for session");
            cleanupSession(this);
        }
    }

    private static String createID() {
        return Long.toHexString(System.currentTimeMillis());
    }
}
