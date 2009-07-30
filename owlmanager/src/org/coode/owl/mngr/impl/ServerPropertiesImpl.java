/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.owl.mngr.impl;

import org.coode.owl.mngr.ServerConstants;
import org.coode.owl.mngr.ServerProperties;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.*;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jan 22, 2008<br><br>
 */
public class ServerPropertiesImpl implements ServerProperties {

    private Properties properties = new Properties();

    private List<PropertyChangeListener> listeners = new ArrayList<PropertyChangeListener>();

    private Map<String, List<String>> allowedValues = new HashMap<String, List<String>>();


    public String get(String key) {
        return properties.getProperty(key);
    }

    public boolean set(String key, String value) {
        String currentValue = properties.getProperty(key);
        if (currentValue == null){
            if (value != null){
                properties.setProperty(key, value);
                notifyPropertyChanged(key, currentValue, value);
                return true;
            }
        }
        else {
            if (value == null){
                properties.remove(key);
                notifyPropertyChanged(key, currentValue, value);
                return true;
            }
            else if (!currentValue.equals(value)){
                properties.setProperty(key, value);
                notifyPropertyChanged(key, currentValue, value);
                return true;
            }
        }
        return false;
    }


    public Set<String> keySet() {
        Set<String> keys = new HashSet<String>();
        Enumeration<?> it = properties.propertyNames();
        while(it.hasMoreElements()){
            keys.add((String)it.nextElement());
        }
        return keys;
    }


    public void remove(String key) {
        String oldValue = properties.getProperty(key);
        properties.remove(key);
        notifyPropertyChanged(key, oldValue, null);
    }


    public void addPropertyChangeListener(PropertyChangeListener l) {
        listeners.add(l);
    }


    public void removePropertyChangeListener(PropertyChangeListener l) {
        listeners.remove(l);
    }


    public void save(OutputStream out) throws IOException {
        properties.store(out, null);
    }


    public void load(InputStream in) throws IOException {
        properties.clear();
        properties.load(in);
    }


    public boolean isSet(String booleanOption) {
        return ServerConstants.TRUE.equals(properties.get(booleanOption));
    }


    public void setAllowedValues(String key, List<String> values) {
        allowedValues.put(key, values);
    }


    public List<String> getAllowedValues(String key) {
        List<String> result = allowedValues.get(key);
        return result != null ? result : Collections.EMPTY_LIST;
    }


    private void notifyPropertyChanged(String key, String oldValue, String newValue) {
        for (PropertyChangeListener l : listeners){
            l.propertyChange(new PropertyChangeEvent(this, key, oldValue, newValue));
        }
    }
}
