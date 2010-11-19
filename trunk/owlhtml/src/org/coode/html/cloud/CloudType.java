/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.cloud;

import org.coode.owl.mngr.NamedObjectType;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jan 15, 2008<br><br>
 */
public enum CloudType {
    classusage ("Class Usage", NamedObjectType.classes),
    indusage ("Individual Usage", NamedObjectType.individuals),
    objpropusage ("Object Property Usage", NamedObjectType.objectproperties),
    datapropusage ("Data Property Usage", NamedObjectType.dataproperties);

    private String label;

    private NamedObjectType type;

    CloudType(String label, NamedObjectType type) {
        this.label = label;
        this.type = type;
    }

    public String getRendering() {
        return label;
    }

    public NamedObjectType getType(){
        return type;
    }
}
