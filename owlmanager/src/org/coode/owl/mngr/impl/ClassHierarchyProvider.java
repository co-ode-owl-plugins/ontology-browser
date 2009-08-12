package org.coode.owl.mngr.impl;

import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.util.ToldClassHierarchyReasoner;
import org.semanticweb.owlapi.inference.OWLClassReasoner;
import org.semanticweb.owlapi.inference.OWLReasonerException;
import org.coode.owl.mngr.HierarchyProvider;
import org.coode.owl.mngr.OWLServer;
import org.coode.owl.mngr.OWLServerListener;
import org.coode.owl.util.ModelUtil;
import org.apache.log4j.Logger;

import java.util.Set;
import java.util.Collections;

import com.sun.tools.example.debug.gui.Environment;
/*
* Copyright (C) 2007, University of Manchester
*
* Modifications to the initial code base are copyright of their
* respective authors, or their employers as appropriate.  Authorship
* of the modifications may be determined from the ChangeLog placed at
* the end of this file.
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.

* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.

* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*/

/**
 * Author: drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Aug 6, 2009<br><br>
 */
public class ClassHierarchyProvider implements HierarchyProvider<OWLClass>{

    private Logger logger = Logger.getLogger(ClassHierarchyProvider.class);

    private OWLServer server;

    private ToldClassHierarchyReasoner r;

    private OWLServerListener serverListener = new OWLServerListener(){

        public void activeOntologyChanged(OWLOntology ont) {
            reset();
        }
    };


    public ClassHierarchyProvider(OWLServer server) {
        this.server = server;
        server.addServerListener(serverListener);
    }


    public OWLClass getRoot() {
        return getServer().getOWLOntologyManager().getOWLDataFactory().getOWLThing();
    }


    public Set<OWLClass> getParents(OWLClass node) {
        try {
            return ModelUtil.filterClasses(getReasoner().getSuperClasses(node), getServer().getOWLOntologyManager().getOWLDataFactory());
        }
        catch (OWLReasonerException e) {
            logger.error(e);
        }
        return Collections.emptySet();
    }


    public Set<OWLClass> getChildren(OWLClass node) {
        try {
            return ModelUtil.filterClasses(getReasoner().getSubClasses(node), getServer().getOWLOntologyManager().getOWLDataFactory());
        }
        catch (OWLReasonerException e) {
            logger.error(e);
        }
        return Collections.emptySet();
    }


    public Set<OWLClass> getEquivalents(OWLClass node) {
        try{
            return getReasoner().getEquivalentClasses(node);
        }
        catch (OWLReasonerException e) {
            logger.error(e);
        }
        return Collections.emptySet();
    }


    public Set<OWLClass> getDescendants(OWLClass node) {
        try {
            return ModelUtil.filterClasses(getReasoner().getDescendantClasses(node), getServer().getOWLOntologyManager().getOWLDataFactory());
        }
        catch (OWLReasonerException e) {
            logger.error(e);
        }
        return Collections.emptySet();
    }


    public Set<OWLClass> getAncestors(OWLClass node) {
        try{
            return ModelUtil.filterClasses(getReasoner().getAncestorClasses(node), getServer().getOWLOntologyManager().getOWLDataFactory());
        }
        catch (OWLReasonerException e) {
            logger.error(e);
        }
        return Collections.emptySet();
    }


    public void dispose() {
        getServer().removeServerListener(serverListener);
    }


    protected final OWLServer getServer() {
        return server;
    }


    protected Set<OWLOntology> getOntologies() {
        return getServer().getActiveOntologies();
    }


    protected OWLClassReasoner getReasoner() {
        if (r == null){
            r = new ToldClassHierarchyReasoner(getServer().getOWLOntologyManager());
            r.loadOntologies(getOntologies());
            r.classify();
        }
        return r;
    }

    private void reset() {
        if (r != null){
            r.dispose();
            r = null;
        }
    }
}
