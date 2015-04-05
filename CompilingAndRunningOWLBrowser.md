# Compiling and running the OWL browser #

## IntelliJ ##

These instructions are for IntelliJ7.0. Other versions may vary.

### Setup a project ###

  * Checkout all 3 packages from svn to `<checkout-dir>`
  * Create a new module for each package setting each `src/` folder as a source
  * Each module should include dependencies on the jars in its `lib/` folder
    * Export `junit.jar`, `owlapi-bin.jar`, `log4j.jar` from owlmanager
    * OWLBrowser is dependent on OWLHTML
    * OWLHTML is dependent on OWLManager
  * OWLBrowser has a web facet
    * For each module, set **Copy module output to** `/WEBINF/classes`
    * Set up a **Web Resource Directory**
      * `<checkout-dir>/owlbrowser/resources` should be copied to /
    * Set `<checkout-dir>/owlbrowser/src` as a source root
    * In the `Build Settings` select **create web module war file** and enter a name for this

You can now build the war file and deploy to your own server or follow the instructions below to run/debug directly from IntelliJ.

### Run the browser ###

  * Create a new **Tomcat Server** run/debug configuration
  * Select the **application server** (tested with Tomcat 5.5 - I believe 6.0 also works)
  * Set the **startup page** to `http://localhost:8080/browser/`
  * In the **VM parameters**, opt to give the server more memory (eg `-Xmx1500M`)
  * In **Deployment** select the previously created web facet
  * Ensure **Deploy Web Facet** is selected
  * In **Deployment Source** choose the 'war' file you have specified previously
  * Set the **Application Context** to `/browser`

You can now run/debug the browser in the usual way. A web browser should pop up with the correct location once Tomcat has started.