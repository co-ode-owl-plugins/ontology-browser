/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.doclet;

import java.io.PrintWriter;
import java.net.URL;
import java.util.Set;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Feb 6, 2008<br><br>
 *
 * A basic piece of HTML.
 * A doclet can specify if it is dependent on any particular CSS or JS
 */
public interface HTMLDoclet<O> {

    String getID();

    void renderContent(URL pageURL, PrintWriter out);

    void renderAll(URL pageURL, PrintWriter out);

    /**
     * Implementations should make sure the user object is passed down through the doclet hierarchy
     * Ignoring doclets that are pinned.
     * <em>Always call if overridden</em>
     * @param object the user object that will be returned via getUserObject()
     */
    void setUserObject(O object);

    O getUserObject();

    /**
     * whether the doclet should follow the selection of its parent, or retain its own user object
     * @return false if the doclet should follow its parent's selection
     */
    boolean isPinned();

    Set<URL> getRequiredCSS();

    Set<URL> getRequiredJS();
}
