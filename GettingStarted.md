You can find an instance of the browser hosted on a very old linux box sat under my desk [here](http://owl.cs.manchester.ac.uk/browser/).

Once you have the browser in front of you, you can try it with any content you can find on the web.

The steps below cover the basics and should only take a couple of minutes to walk through:




---


## Load content ##

To load content into the browser, enter the location of the OWL or RDF file.

Alternatively try one of the example bookmarks provided.

**Hint** Some LOD providers such as the BBC will use the same URI for html and rdf - this will be resolved by the browser.

**Note** because we use the OWLAPI under the hood, only RDF that can forms a valid OWL2 model can be loaded. This is still a lot of RDF.


![http://ontology-browser.googlecode.com/svn/wiki/images/load.png](http://ontology-browser.googlecode.com/svn/wiki/images/load.png)


All resources must be publicly accessible for the server to find them.

If you want to load local files or files on an internal network you will have to [download the server and run it locally](Installation.md).


### Loading ontologies with imports ###

The server will attempt to find any imports in the same location as the root ontology and load these as well. If it cannot find them there, it will default to looking on the web at a URL matching the ontology URI.

Once the browser has loaded the content you can start browsing.


---


## Finding your way around ##

Once content has been successfully opened you will be presented with either a page describing that ontology or if there is an entity with the same URI then it will navigate straight to this.

From here it should be easy to navigate around.

Multiple tabs across the top allow you to see entities by type (class, property, individual or datatype).

If a tab is faded out it is because there are no entities of that type in the loaded content.

![http://ontology-browser.googlecode.com/svn/wiki/images/cityOfSalford.png](http://ontology-browser.googlecode.com/svn/wiki/images/cityOfSalford.png)

For each type you can navigate around using a hierarchy on the left of the screen.

The tree can be expanded and collapsed as required by using the **+** (but each new page is automatically pruned to show the current selection).

Selecting a name will navigate to that entity.

Current selection will always be shown highlighted in yellow.

### Find ###

Alternatively, you can do a search on the name of the entity you are looking for.

Use the search box at the top right of the screen and start typing the name of the thing you are looking for.

A dropdown is implemented to help you with this.

![http://ontology-browser.googlecode.com/svn/wiki/images/find.png](http://ontology-browser.googlecode.com/svn/wiki/images/find.png)


---


## Loading additional content ##

Providers of Linked Open Data commonly publish RDF at the location corresponding to the URI of the entity.

To keep things running quickly Ontology Browser does not automatically attempt to load additional content as you browse.

The URI for an entity may have 2 icons after it:
  * The first allows you to open the URI in another browser window (useful for html pages)
  * The second (if visible) allows you to attempt to pull in more content from the URL

In the example below we have only loaded the City of Salford content so when we navigate to Manchester we find very little.

![http://ontology-browser.googlecode.com/svn/wiki/images/manchesterBeforeLoad.png](http://ontology-browser.googlecode.com/svn/wiki/images/manchesterBeforeLoad.png)

However, if we then use the link to pull in content from the Manchester URL we find much more.

![http://ontology-browser.googlecode.com/svn/wiki/images/manchesterAfterLoad.png](http://ontology-browser.googlecode.com/svn/wiki/images/manchesterAfterLoad.png)

**Hint** once the content has been loaded the second icon should disappear.


---


## Permalinking ##

At any time you can select or copy the permalink link and bookmark it or send it to your friends.

![http://ontology-browser.googlecode.com/svn/wiki/images/permalink.png](http://ontology-browser.googlecode.com/svn/wiki/images/permalink.png)

The permalink takes into account any settings you have updated, and includes pointers to where you loaded the ontologies from.

[try it](http://owl.cs.manchester.ac.uk/browser/individuals/1182946835/?session=12ccb674365-29-12ccb6a0445)


---


## Display Names ##

If your ontology appears with lots of meaningless names it is possible that the human-intended labels are in annotations.

By default, Ontology Browser looks for rdfs:label annotations with no language set, but you can change this in the options (top right, under the search box).

The annotation property can be changed as well as the language (in the case where multiple alternatives exist)

![http://ontology-browser.googlecode.com/svn/wiki/images/russian.png](http://ontology-browser.googlecode.com/svn/wiki/images/russian.png)

If the labels appear in data property assertions you can also change this in the options.


---


## Active ontology ##

The selector on the top bar determines the current active ontology.

The browser will only show content from the active ontology (and its imports).

![http://ontology-browser.googlecode.com/svn/wiki/images/activeOntology.png](http://ontology-browser.googlecode.com/svn/wiki/images/activeOntology.png)

In addition, if reasoning is being used (using the DLQuery tab or inferences are shown) then the reasoner will only take into account the active ontology (and its imports).

The default is **All ontologies** - this is a system ontology that imports all of the others.

**Hint** The import structure of the ontologies can be seen in the hierarchy in the ontologies tab.


---


## Ending / Starting again ##

You can clear a session by clicking the red cross at the top left of the screen.

![http://ontology-browser.googlecode.com/svn/wiki/images/quit.png](http://ontology-browser.googlecode.com/svn/wiki/images/quit.png)

This clears all ontologies and resets the options to their defaults.

Don't worry, any permalinks you have saved will be in exactly the same state as they were.