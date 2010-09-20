package org.coode.html.doclet;

import org.coode.html.OWLHTMLKit;
import org.coode.html.cloud.CloudModel;
import org.coode.html.impl.OWLHTMLConstants;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLEntity;

import java.awt.*;
import java.io.PrintWriter;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;


/**
 * Author: Nick Drummond<br>
 * nick.drummond@cs.manchester.ac.uk<br>
 * http://www.cs.man.ac.uk/~drummond<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jun 15, 2007<br><br>
 * <p/>
 * code made available under Mozilla Public License (http://www.mozilla.org/MPL/MPL-1.1.html)<br>
 * copyright 2006, The University of Manchester<br>
 */
public class CloudDoclet<O extends OWLEntity> extends AbstractHTMLDoclet<OWLOntology> {

    private static final String ID = "doclet.cloud";

    private static final String SELECTION_COLOR = "#0000FF";

    // capped maximum size of the font used to display entities
    private static final int MAX_SIZE = 40;

    private Comparator<? super O> comparator;

    private java.util.List<O> entities;

    private OWLHTMLKit kit;

    private CloudModel<O> model;

    private int threshold = 0;
    private int zoom = 0;
    private boolean normalise = false;
    private boolean inverted = false;
//    private int count = -1; // number of entities shown

    private O currentSelection;

    private OWLHTMLConstants.LinkTarget target;


    public CloudDoclet(CloudModel<O> model, OWLHTMLKit kit){
        this.kit = kit;
        this.model = model;
    }

    protected void renderHeader(URL pageURL, PrintWriter out) {

        renderBoxStart(getTitle(), out);

        out.println("<div style='width: 100%; text-align: center'>");

        model.reload();

        entities = new ArrayList<O>(model.getEntities(threshold));

        if (comparator != null){
            Collections.sort(entities, comparator);
        }

        for (O entity : entities){
            renderLabel(entity, pageURL, out);
            out.print(" ");
        }

        out.println("</div>");
    }

    protected void renderFooter(URL pageURL, PrintWriter out) {
        renderBoxEnd(getTitle(), out);
    }


    public String getTitle() {
        return model.getTitle();
    }

    private Color getColor(int value) {
        int score;
        if (normalise) {
            int relativeScore = value - model.getMin();
            int scoreRange = model.getRange();
            score = 50 + ((relativeScore * 205) / scoreRange);
        }
        else {
            score = Math.min(255, 50 + (zoom * value / 2));
        }
        if (!inverted) {
            score = 255 - score;
        }
        return new Color(score, score, score);
    }

    private int getFontSize(int value) {
        int size;
        if (normalise) {
            int displayMin = zoom;
            int displayRange = MAX_SIZE - displayMin;
            int scoreRange = model.getRange();
            int relativeScore = value - model.getMin();
            size = displayMin + ((relativeScore * displayRange) / scoreRange);
        }
        else {
            size = Math.min(MAX_SIZE, zoom + (value / 2));
        }

        if (size > MAX_SIZE) {
            throw new RuntimeException("ERROR, OVER MAX SIZE: " + size);
        }

        return size;
    }

    private void renderLabel(O entity, URL pageURL, PrintWriter out) {
        int score = model.getValue(entity);

        String colour = SELECTION_COLOR;
        if (!entity.equals(currentSelection)){
            final String rgb = Integer.toHexString(getColor(score).getRGB());
            colour = "#" + rgb.substring(2, rgb.length());
        }
        int size = getFontSize(score);

        LinkDoclet link = new LinkDoclet<O>(entity, kit);
        link.setCSS("color: " + colour + "; font-size: " + size + ";");
        link.setTarget(target);
        link.addAttribute("title", Integer.toString(score));

        link.renderAll(pageURL, out);
    }

    public void setComparator(Comparator<? super O> comparator) {
        this.comparator = comparator;
    }

    public Comparator<? super O> getComparator(){
        return comparator;
    }

    public boolean getNormalise() {
        return normalise;
    }

    public void setNormalise(boolean normalise) {
        this.normalise = normalise;
    }

    public int getZoom(){
        return zoom;
    }

    public void setZoom(int zoom) {
        this.zoom = zoom;
    }

    public int getThreshold() {
        return threshold;
    }

    public void setThreshold(int threshold) {
        this.threshold = threshold;
    }

    public void setSelection(O currentSelection) {
        this.currentSelection = currentSelection;
    }

    public void setTarget(OWLHTMLConstants.LinkTarget target) {
        this.target = target;
    }

    public String getID() {
        return ID;
    }
}
