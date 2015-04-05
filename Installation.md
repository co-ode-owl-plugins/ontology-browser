# Installation #

If your ontologies are just too private to send to the server (not that we're watching), you can run the server on any machine with a servlet engine installed (eg Tomcat).

The following instructions are for installing and running on linux under tomcat5.5 - but should be straightforward for other systems. (Please send me updates if you had to do anything special on your system).

`CATALINA_HOME` is the tomcat folder on your machine. You might want to set this up as a system variable for convenience.

## Run the browser ##

  * Install <a href='http://tomcat.apache.org/'>tomcat</a>. The browser has been run in a tomcat 5.5 and tomcat 6 environment.
  * Download the <a href='http://code.google.com/p/ontology-browser/downloads/list'>war file</a> or build it from scratch (see below)
  * Rename the war file to "browser.war" and copy into the `CATALINA_HOME/webapps/` folder
  * You might want to consider giving the tomcat jvm a reasonable amount of memory if you will be browsing lots of medium-large ontologies. Use the java parameter -Xmx1000M wherever you start tomcat. Try 500M-2500M depending on the size of your machine.
  * Start tomcat
  * The browser should be available off the root of your tomcat server (typically by default this will be `http://yourmachine.com:8080/browser/`)

## Enable FaCT++ support ##

FaCT++ is written in C++ and is therefore distributed as several native libraries (depending on your system). FaCT++ support is available through a JNI bridge and therefore a small amount of extra work is required.
Please be aware that native code can kill the jvm it is running alongside - this means keeping an eye on tomcat once in a while.

  * Download the <a href='http://code.google.com/p/factplusplus/downloads/list'>appropriate FaCT++ bundle</a> for your system.
  * Extract the JNI library from the package:
    * `FaCTPlusPlusJNI.dll` on windows
    * `libFaCTPlusPlusJNI.jnilib` on OS X
    * `libFaCTPlusPlusJNI.so` on linux (in the lib/ folder)
  * Copy this to `CATALINA_HOME/shared/lib` - <a href='http://wiki.apache.org/tomcat/HowTo#head-a4b7185ee95d0cf14a48f92c08d1eb66b561139d'>also see here</a>
  * However you start your tomcat instance, you need to provide java with the path to the native libraries
> > I do this with a small script, so I just add `-Djava.library.path=$CATALINA_HOME/shared/lib`
  * (Re)start tomcat
  * Change the reasoner to `factPlusPlus` either in a session or in the defaults (see below).
  * In most cases it appears the libraries cannot be released once loaded without restarting tomcat. So if you update the servlets, a restart may be necessary.

## Additional notes ##

The browser stores state so that permalinks can be used (state is reloaded when a permalink is reused). The files are persisted in a caches/ folder wherever your working directory is (typically `CATALINA_HOME/bin/`). Deleting, moving or renaming these files will cause all previous permalinks to be voided.

The ontology bookmarks shown on the load screen are customizable. The browser looks for a file in the caches/ directory called `custom.bookmarks.xml`. This file is generated automatically if none is found.

```
<?xml version="1.0"?>
<bookmarks>
    <bookmark name="pizza">http://www.co-ode.org/ontologies/pizza/2007/02/12/pizza.owl</bookmark>
    <bookmark name="wine">http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine</bookmark>
    <bookmark name="people+pets">http://www.cs.man.ac.uk/~horrocks/ISWC2003/Tutorial/people+pets.owl.rdf</bookmark>
    <bookmark name="mygrid">http://www.mygrid.org.uk/ontology</bookmark>
</bookmarks>
```

The browser also looks for a file in `caches/` called `default.saved` for its default options. Again, this file is created automatically if none is found. Each time a new session is started the settings in this file will be used (so it can be edited any time).

```
option_render_sub_expand_links=false
option_render_permalink=true
option_show_inferred_hierarchies=false
reasoner.enabled=true
css=default.css
option_show_mini_hierarchies=true
index-all-url=entities/
option_label_uri=http\://www.w3.org/2000/01/rdf-schema\#label
option_reasoner=pellet
ren=frag
option_render_subs=true
```

## Get the code ##

Source code available on <a href='http://code.google.com/p/ontology-browser/source/checkout'>SVN</a> under trunk/.

See also [CompilingAndRunningOWLBrowser](CompilingAndRunningOWLBrowser.md) - instructions for compiling and running from source.

## Level of Support ##

As with most new software, things are changing rapidly, so we do not have the resources to actively support the server.

## License ##

Made available under the <a href='http://www.gnu.org/licenses/lgpl.html'>GNU Lesser General Public License</a>

Copyright © 2007 The University Of Manchester