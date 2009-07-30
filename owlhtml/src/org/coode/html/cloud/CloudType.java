/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.cloud;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jan 15, 2008<br><br>
 */
public enum CloudType {
        classusage ("Class Usage"),
        indusage ("Individual Usage"),
        objpropusage ("Object Property Usage"),
        datapropusage ("Data Property Usage");

        private String label;

        CloudType(String label) {
            this.label = label;
        }

        public String getRendering() {
            return label;
        }
}
