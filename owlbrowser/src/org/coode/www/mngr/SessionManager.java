package org.coode.www.mngr;

import org.apache.log4j.Logger;
import org.coode.html.OWLHTMLKit;
import org.coode.html.impl.OWLHTMLConstants;
import org.coode.html.impl.OWLHTMLKitImpl;
import org.coode.html.impl.OWLHTMLProperty;
import org.coode.html.url.PermalinkURLScheme;
import org.coode.html.url.RestURLScheme;
import org.coode.owl.mngr.ServerConstants;
import org.coode.owl.mngr.ServerPropertiesAdapter;
import org.coode.owl.mngr.ServerProperty;
import org.coode.owl.mngr.impl.ManchesterOWLSyntaxParser;
import org.coode.owl.util.ModelUtil;
import org.coode.www.OntologyBrowserConstants;
import org.coode.www.exception.OntServerException;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntologyID;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;
import javax.servlet.http.HttpSessionBindingEvent;
import javax.servlet.http.HttpSessionBindingListener;
import java.io.*;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URL;
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

    private static final String URI_MAPPING_MARKER = "##";


    private static Map<SessionID, OWLHTMLKit> activeServers = new HashMap<SessionID, OWLHTMLKit>();



    /**
     * Get a server (creates a new one if you have a new session)
     * @param request
     * @return
     * @throws OntServerException
     */
    public synchronized static OWLHTMLKit getServer(HttpServletRequest request) throws OntServerException {
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
    public synchronized static OWLHTMLKit getServer(HttpServletRequest request, String label) throws OntServerException {
        OWLHTMLKit kit = getServer(request);
        if (label != null && !label.equals(kit.getCurrentLabel())){
            loadServerState(kit, label);
        }
        return kit;
    }


    /**
     * Persist the current loaded ontologies with their mappings
     * @param kit note: will set the current label on the server
     * @throws OntServerException
     */
    public synchronized static void labelServerState(OWLHTMLKit kit) throws OntServerException {
        String label = kit.getID() + "-" + createID();
        File file = getFile(label + OntologyBrowserConstants.SERVER_STATES_EXT);
        try {
            OutputStream out = new FileOutputStream(file);
            PrintWriter writer = new PrintWriter(out);
            Map<OWLOntologyID, URI> ontologyMappings = kit.getOWLServer().getLocationsMap();
            Set<OWLOntologyID> onts = ontologyMappings.keySet();

            // this saves the base properties (which includes the OWL server properties)
            kit.getHTMLProperties().save(out);

            // always print the active ontology first
            OWLOntologyID activeOnt = kit.getOWLServer().getActiveOntology().getOntologyID();
            writer.println(URI_MAPPING_MARKER + ModelUtil.getOntologyIdString(activeOnt) + "=" + ontologyMappings.get(activeOnt));

            onts.remove(activeOnt);
            for (OWLOntologyID ont : onts){
                writer.println(URI_MAPPING_MARKER + ModelUtil.getOntologyIdString(ont) + "=" + ontologyMappings.get(ont));
            }

            writer.flush();
            out.flush();

            writer.close();
            out.close();

            kit.setCurrentLabel(label);
            logger.debug("kit state saved at: " + file.getAbsolutePath());
        }
        catch (IOException e) {
            throw new OntServerException(e);
        }
    }


    /**
     * Clears the given server and replaces its state with that specified by the label given
     * @param kit
     * @param label
     * @throws OntServerException
     */
    private synchronized static void loadServerState(OWLHTMLKit kit, String label) throws OntServerException {

        File file = getFile(label + OntologyBrowserConstants.SERVER_STATES_EXT);

        if (!file.exists()){
            throw new OntServerException("Cannot find stored state for unknown session " + label);
        }

        kit.getOWLServer().clearOntologies(); // dump all ontologies and caches

        try{
            // we are currently reading the file twice - @@TODO make this much nicer

            // pass 1 to get the ontology mappings
            BufferedReader reader = new BufferedReader(new FileReader(file));
            String line;
            Map<IRI, IRI> ontMap = new HashMap<IRI, IRI>();
            while ((line = reader.readLine()) != null){
                    if (line.startsWith(URI_MAPPING_MARKER)){
                        line = line.substring(URI_MAPPING_MARKER.length(), line.length());
                        String[] param = line.split("=");
                        IRI ontURI = IRI.create(param[0].trim());
                        IRI physicalURI = IRI.create(param[1].trim());
//                        if (ontURI.isAbsolute() && physicalURI.isAbsolute()){
                            ontMap.put(ontURI, physicalURI);
//                        }
                    }
            }

            kit.getOWLServer().loadOntologies(ontMap);

            // pass 2 to get the properties
            BufferedInputStream in = new BufferedInputStream(new FileInputStream(file));
            kit.getHTMLProperties().load(in);
            in.close();
            cleanupProperties(kit);

            kit.setCurrentLabel(label);
        }
        catch(IOException e){
            throw new OntServerException(e);
        }
    }

    private static void cleanupProperties(OWLHTMLKit kit) {
        // fix the default css that was in the root
        String css = kit.getHTMLProperties().get(OWLHTMLProperty.optionDefaultCSS);
        if (!css.startsWith("http") && !css.startsWith(OWLHTMLConstants.CSS_BASE)){
            kit.getHTMLProperties().set(OWLHTMLProperty.optionDefaultCSS, OWLHTMLConstants.CSS_BASE + css);
        }
    }


    public static File getFile(String name) {
        File cacheDir = new File(OntologyBrowserConstants.SERVER_STATES_DIR);
        if (!cacheDir.exists()){
            cacheDir.mkdir();
        }
        return new File(OntologyBrowserConstants.SERVER_STATES_DIR + name);
    }

    public synchronized static void closeSession(HttpSession mySession) {
        if (mySession != null){
            SessionID id = (SessionID) mySession.getAttribute(ID);
            cleanupSession(id);
            mySession.invalidate();
        }
    }

    public synchronized static void cleanupSession(SessionID id) {

        OWLHTMLKit kit = activeServers.remove(id);
        if (kit != null){ // might already have been tidied away
            final String serverID = kit.getID();
            kit.dispose();

            logger.debug("killed " +  serverID + " - active servers: " + activeServers.size());

            System.gc();
            System.gc();
        }
    }

    private synchronized static void create(HttpSession mySession, HttpServletRequest request) {
        try{
            String url = request.getRequestURL().toString();

            int index = url.indexOf(request.getServletPath());

            if (index != -1){
                url = url.substring(0, index+1);
            }

            URL basePath = new URL(url);

            SessionID id = new SessionID();

            OWLHTMLKit kit = createServer(id.id, basePath);

            activeServers.put(id, kit);
            mySession.setAttribute(ID, id);
            logger.debug("started " + id + " - active servers: " + activeServers.size());
        }
        catch (MalformedURLException e) {
            logger.error(e);
        }
    }

    private static OWLHTMLKit createServer(String id, URL basePath) {

        OWLHTMLKit kit = new OWLHTMLKitImpl(id, basePath);

        // set silent error handling for missing imports
        kit.getOWLServer().getOWLOntologyManager().setSilentMissingImportsHandling(true);

        // use a servlet URL scheme which encodes the names in params
        kit.setURLScheme(new PermalinkURLScheme(new RestURLScheme(kit)));

        // register parsers
        kit.getOWLServer().registerDescriptionParser(ServerConstants.Syntax.man.toString(),
                                                     new ManchesterOWLSyntaxParser(kit.getOWLServer()));

        boolean defaultsLoaded = false;

        // we will likely want different defaults for different versions (or run versions on the same server)
        File file = getFile("default" + OntologyBrowserConstants.VERSION + OntologyBrowserConstants.SERVER_STATES_EXT);

        if (file.exists()){
            try {
                BufferedInputStream in = new BufferedInputStream(new FileInputStream(file));
                kit.getHTMLProperties().load(in);
                in.close();
                defaultsLoaded = true;
            }
            catch (IOException e) {
                logger.error("Could not load default properties");
            }
        }

        if (!defaultsLoaded){

            setupDefaultServerProperties(kit);

            try {
                OutputStream out = new FileOutputStream(file);
                kit.getHTMLProperties().save(out);
            }
            catch (IOException e) {
                logger.error("Could not save default properties");
            }
        }


        return kit;
    }


    private static void setupDefaultServerProperties(OWLHTMLKit kit) {

        // make sure the reasoner is enabled to allow dl query etc
        kit.getOWLServer().getProperties().setBoolean(ServerProperty.optionReasonerEnabled, true);


        ServerPropertiesAdapter<OWLHTMLProperty> properties = kit.getHTMLProperties();

        // by default, do not use frames navigation
        properties.set(OWLHTMLProperty.optionContentWindow, null);

        // the default entities index is at the location "entities/"
        properties.set(OWLHTMLProperty.optionIndexAllURL, "entities/");

        // render a permalink
        properties.setBoolean(OWLHTMLProperty.optionRenderPermalink, true);

        properties.setBoolean(OWLHTMLProperty.optionShowMiniHierarchies, true);

        properties.setBoolean(OWLHTMLProperty.optionShowInferredHierarchies, false);

        // default location for DIG reasoner
        if (kit.getBaseURL().toString().contains("localhost")){ // just to make sure I don't accidentally publish this address
            kit.getOWLServer().getProperties().set(ServerProperty.optionReasonerUrl, "http://rpc295.cs.man.ac.uk:8080");
        }
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
