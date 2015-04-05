# API #

**9th Dec 2010 - to be updated - I cannot guarantee these interfaces will work currently**

The server has been written with an HTTP interface such that programmatic or user interaction is possible.

For example, an ontology repository (or a user's own HTML pages) could incorporate a link to the browser to show a particular ontology.

Such a link might look like this:

```
http://owl.cs.manchester.ac.uk/browser/ontologies/?action=load&clear=true&uri=http://www.co-ode.org/ontologies/pizza/2007/02/12/pizza.owl
```

This loads a particular ontology (pizza.owl) from the physical location specified by the uri parameter. This ontology (and its imports) replaces all existing loaded ontologies in this session because of the `clear=true` parameter.

Unless the user application has control over the loading of ontologies, when interacting with the server it is advisable to specify a saved session using the **session** parameter. You can get this from any permalink in the browser.

session=12d09432c9b-1959-12d094335ef


## Examples ##


Find all individuals in the BBC species ontology starting with the letter B:

`http://owl.cs.manchester.ac.uk/browser/find/?input=B&type=individuals&format=xml&session=12d09432c9b-1959-12d094335ef`

```
<?xml version="1.0" encoding="UTF-8" ?>
<results>
<rs id="http://www.bbc.co.uk/nature/life/Blackcap#species" info="">Blackcap</rs>
<rs id="http://www.bbc.co.uk/nature/life/Eurasian_Bittern#species" info="">Bittern</rs>
<rs id="http://www.bbc.co.uk/nature/life/Blackbuck#species" info="">Blackbuck</rs>
<rs id="http://www.bbc.co.uk/nature/life/Bharal#species" info="">Bharal</rs>
<rs id="http://www.bbc.co.uk/nature/life/Common_Blackbird#species" info="">Blackbird</rs>
<rs id="http://www.bbc.co.uk/nature/life/Bongo_(antelope)#species" info="">Bongo</rs>
<rs id="http://www.bbc.co.uk/nature/life/Bonobo#species" info="">Bonobo</rs>
<rs id="http://www.bbc.co.uk/nature/life/Blackberry#species" info="">Blackberry</rs>
<rs id="http://www.bbc.co.uk/nature/life/European_Badger#species" info="">Badger</rs>
</results>
```

Get all pizzas with a spicy topping:

`http://owl.cs.manchester.ac.uk/browser/query/?query=subclasses&expression=hasTopping+some+%28hasSpiciness+some+Hot%29&format=xml&syntax=man&session=12d0976b82b-1966-12d0976f189`

```
<?xml version="1.0" encoding="UTF-8" ?>
<results>
<rs id="http://www.co-ode.org/ontologies/pizza/pizza.owl#AmericanHot" info="">AmericanHot</rs>
<rs id="http://www.co-ode.org/ontologies/pizza/pizza.owl#PolloAdAstra" info="">PolloAdAstra</rs>
<rs id="http://www.co-ode.org/ontologies/pizza/pizza.owl#Cajun" info="">Cajun</rs>
<rs id="http://www.co-ode.org/ontologies/pizza/pizza.owl#SloppyGiuseppe" info="">SloppyGiuseppe</rs>
</results>
```

## Parameters ##

<table border='1px black solid'>
<blockquote><tr><th>URL</th><th>parameters</th><th>usage notes</th></tr></blockquote>

<blockquote><tr><td>ontologies/</td><td>
<em>action</em> = [load|remove|reload|browse] <br />
<em>clear=true</em><br />
<em>uri</em> = the physical location the ontology (for loading) or the ontology URI otherwise<br />
<em>clear</em> = true in combination with load to dump all existing loaded ontologies</td>
<td>browse makes the ontology and its imports closure active (much like the active ontology in Protege4).<br />clear = true causes the existing state of the browser to be cleared on a load</td></tr></blockquote>

<blockquote><tr><td>find/</td><td>
<em>type</em> = [entities|classes|objectproperties|dataproperties|individuals|ontologies]<br />
<em>input</em> = name search string<br /><code>*</code></td><td></td></tr></blockquote>

<blockquote><tr><td>entities/</td><td>
<em>uri</em> = the full URI of the entity (should be escaped - particularly # = %23)<br />
<em>name</em> = the rendered name of the entity<br />
<em>ontology</em> = the URI of the ontology to produce an index for<br /><code>*</code></td></blockquote>

<td>If no parameters are specified, an index of all entities will be shown.<br />
If the name is specified on its own the entity(ies) with that rendering will be shown.</td></tr>

<blockquote><tr><td>classes/</td><td>
see entities/<br /><code>*</code></td><td></td></tr></blockquote>

<blockquote><tr><td>objectproperties/</td><td>
see entities/<br /><code>*</code></td><td></td></tr></blockquote>

<blockquote><tr><td>dataproperties/</td><td>
see entities/<br /><code>*</code></td><td></td></tr></blockquote>

<blockquote><tr><td>individuals/</td><td>
see entities/<br /><code>*</code></td><td></td></tr></blockquote>

<blockquote><tr><td>query/</td><td>
<em>query</em> = [subclasses|equivalents|superclasses|ancestors|descendants|instances]<br />
<em>expression</em> = the OWL class description<br /><code>*</code></td></blockquote>

<blockquote><td>Reasoner query of the type specified.<br />
The OWL description can only be an intersection with named classes, existential/universal/value restrictions with named fillers</td></tr></blockquote>

<blockquote><tr><td>cloud/</td><td>
<em>type</em> = [classusage|objpropusage|datapropusage|annotpropusage|indusage, datatypeusage]</td><td>Tag clouds for entities</td></tr></blockquote>

<blockquote><tr><td>signout.html</td><td>confirm = true to do this without prompting</td><td>Kill this session, clearing all ontologies.</td></tr>
<tr><td>option</td><td>
<em>opt</em> = <a href='option.md'>option</a><br />
<em>value</em> = <a href='value.md'>value</a></td></blockquote>

<blockquote><td>Options<br />ren: [frag|label]</td></tr></blockquote>

<blockquote></table></blockquote>



`*` indicates that a format for the results can be specified (see below)

## Content type ##

An optional format param can be specified in the request URL.
The valid values are:

  * `xml` of the form:
```
<results>
 <rs id="[entity url]" info="[for future use]">[entity name]<rs>
</results>
```
  * `html` creates a full html page, with CSS and title etc
  * `html-frag` creates just the bit of html required to add to an existing page (useful for AJAX calls for extra content)

If no format param is used in the URL, the browser uses (very simple) content negotiation on the http requests Accept header to decide what format the response is in. Options are simply:
  * application/xml
  * text/html

If you require html-frag you must ask for it in the requested URL.