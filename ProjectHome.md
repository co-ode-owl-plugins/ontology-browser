Ontology Browser allows you to navigate around OWL ontologies and Linked Open Data online.

![http://ontology-browser.googlecode.com/svn/wiki/images/ontologybrowser-thumb.png](http://ontology-browser.googlecode.com/svn/wiki/images/ontologybrowser-thumb.png)

## Try It ##

Try browsing one of these examples:
  * <a href='http://owl.cs.manchester.ac.uk/browser/ontologies/-1940493937/?session=12a6df741bb-107-12a6dfe673d'>pizza ontology</a>
  * <a href='http://owl.cs.manchester.ac.uk/browser/individuals/1893627120/?session=12ccb8f29ba-36-12ccb8f9cc1'>Ordnance Survey's Manchester</a>
  * <a href='http://owl.cs.manchester.ac.uk/browser/individuals/-662275923/?session=12ccb917033-37-12ccb968ff2'>BBC elephants</a>

Or, <a href='http://owl.cs.manchester.ac.uk/browser/'>load one of your own ontologies</a> then add a link on your own pages to your ontology for others to browse:
```
http://owl.cs.manchester.ac.uk/browser/ontologies/?action=load&clear=true&uri=http://www.example.org/ontology.owl
```

## Features ##

  * Load content from anywhere on the web
  * **Browse OWL ontologies** and/or RDF **Linked Open Data** together in one interface
  * All content created dynamically created on the fly
  * Dynamically load additional resources in as you go
  * Fully indexed and linked content, split down by module and entity type
  * Simple support for **images**, **sounds** and **locations** (other media by request)
  * Expandable class and property **hierarchies**
  * **Find** entities by name with **autocomplete**
  * **DL queries** using full Manchester Syntax with autocompletion and parsing
  * **Permalinking** for sharing pages with friends
  * REST(ish) API with xml/html results (work in progress)


**Note** only RDF that can be loaded into an OWL2 model (the OWLAPI) is supported

## Documentation ##

  * [Getting started](GettingStarted.md) with the ontology browser
  * [Release Notes](ReleaseNotes.md)
  * [Install the browser](Installation.md) on your own server (or locally).

## Level of Support ##

Since CO-ODE finished work on the browser has been mostly in my spare time.
With this in mind I will make a good attempt at helping people as much as I can.

## Acknowledgements ##

Initial development was funded by <a href='http://www.jisc.ac.uk/'>JISC</a> as part of
the <a href='http://www.co-ode.org'>CO-ODE project</a>.

The project grew out of <a href='http://code.google.com/p/co-ode-owl-plugins/wiki/OWLDoc'>OWLDoc</a> - static OWL HTML generator.

Ontology Browser uses <a href='http://owlapi.sourceforge.net/'>OWL API</a> developed at <a href='http://www.cs.manchester.ac.uk'>The University of Manchester</a>.

Ontology Browser also uses
  * <a href='http://jquery.com/'>JQuery</a> javascript framework
  * <a href='http://www.brandspankingnew.net/specials/ajax_autosuggest/ajax_autosuggest_autocomplete.html'>BSN autosuggest</a>
  * <a href='http://www.nickdrummond.co.uk/gaphu.js/'>gaphu.js</a>

all code copyright © 2007 The University Of Manchester