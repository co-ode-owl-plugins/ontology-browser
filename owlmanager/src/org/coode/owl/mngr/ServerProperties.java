/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.owl.mngr;

import java.beans.PropertyChangeListener;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Set;
import java.util.List;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jan 22, 2008<br><br>
 */
public interface ServerProperties {

    String get(String key);

    boolean set(String key, String value);

    Set<String> keySet();

    void remove(String key);    

    void addPropertyChangeListener(PropertyChangeListener l);

    void removePropertyChangeListener(PropertyChangeListener l);

    void save(OutputStream out) throws IOException;

    void load(InputStream in) throws IOException;

    boolean isSet(String booleanOption);

    void setAllowedValues(String key, List<String> values);

    List<String> getAllowedValues(String key);
}
