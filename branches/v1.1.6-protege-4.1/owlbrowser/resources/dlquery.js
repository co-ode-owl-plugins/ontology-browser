/*
 * Control of AJAX calls to build the query form dynamically and request results from the ontology kit
 *
 */
var BUSY_IMAGE = "images/busy.gif";

var EQUIVALENTS = 0;
var SUBCLASSES = 1;
var DESCENDANTS = 2;
var SUPERCLASSES = 3;
var ANCESTORS = 4;
var INSTANCES = 5; // NOT to be confused with individuals/
var QUERY_COUNT = 6;

var PARAM_QUERYTYPE = "query";
var PARAM_EXPRESSION = "expression";
var PARAM_SYNTAX = "syntax";

var NAME = 0;
var XML_OBJ = 1;
var CALLBACK = 2;

var queryArray = [
    ["equivalents", null, inferredEquivalentsReceived],
    ["subclasses", null, inferredSubclassesReceived],
    ["descendants", null, inferredDescendantsReceived],
    ["superclasses", null, inferredSuperclassesReceived],
    ["ancestors", null, inferredAncestorsReceived],
    ["instances", null, inferredInstancesReceived]
];

////////////////////////////////////////////////////////////////////////////////////////////

// have to pass the servlet base in dynamically
function sendQuery(expression, syntax, servletBase){
    if (expression != ""){
        document.getElementById("resultsForm").innerHTML="";

        for (var i=0; i<QUERY_COUNT; i++){
            queryArray[i][XML_OBJ]=getXmlHttpObject(queryArray[i][CALLBACK]);
            sendSubQuery(expression, syntax, queryArray[i][NAME], servletBase, queryArray[i][XML_OBJ]);
        }
    }
}

function sendSubQuery(expression, syntax, querytype, servletBase, xmlHttpReq){

    if (xmlHttpReq==null) {
        alert ("Browser does not support HTTP Request");
    }
    else{
        xmlHttpReq.open("POST", servletBase + "query/",true);

        xmlHttpReq.setRequestHeader("Content-Type", "application/x-www-form-urlencoded; charset=UTF-8");

        xmlHttpReq.send(PARAM_QUERYTYPE + "=" + querytype + "&" +
                        PARAM_EXPRESSION + "=" + expression + "&" +
                        PARAM_SYNTAX + "=" + syntax + "&" +
                        "format=html-frag");

        var busy = document.createElement('div');
        busy.setAttribute('id', querytype);
        busy.innerHTML = "<h2 style='display: inline;'>" + querytype +
                         "</h2><img src='" + servletBase + BUSY_IMAGE + "' width='18px' height='18px' />";
        document.getElementById("resultsForm").appendChild(busy);
    }
}

function receivedResults(i) {
    if (queryArray[i][XML_OBJ].readyState==4){
        if (queryArray[i][XML_OBJ].status==200) {
            document.getElementById(queryArray[i][NAME]).innerHTML=queryArray[i][XML_OBJ].responseText;
        }
        else{
            document.getElementById(queryArray[i][NAME]).innerHTML="<h2>" + queryArray[i][NAME] + " (0)</h2>"
        }
    }
}

function inferredEquivalentsReceived() {
    receivedResults(EQUIVALENTS);
}

function inferredSubclassesReceived() {
    receivedResults(SUBCLASSES);
}

function inferredDescendantsReceived() {
    receivedResults(DESCENDANTS);
}

function inferredSuperclassesReceived() {
    receivedResults(SUPERCLASSES);
}

function inferredAncestorsReceived() {
    receivedResults(ANCESTORS);
}

function inferredInstancesReceived() {
    receivedResults(INSTANCES);
}

///////////////////////

// just shorthand for below
function getValueOfElementByID(id){
    return getValueForElement(document.getElementById(id));
}

function getValueForElement(element){
    switch(element.type) {
        case "select-one":
            return element.options[element.selectedIndex].value;
        case "anchorNode":
            return element.getAttribute("title");
        case "text": // dropthrough
        case "hidden":
            return element.value;
        default:
            alert("type of property element: " + element.type);
            return "";
    }
}

function focusComponent(id){
    document.getElementById(id).focus();
}

/////////////////////////

function getXmlHttpObject(callback) {
    var objXMLHttp=null;
    if (window.XMLHttpRequest) { // for IE7 and other standard browsers
        objXMLHttp=new XMLHttpRequest();
    }
    else if (window.ActiveXObject) { // for IE6 and earlier
        objXMLHttp=new ActiveXObject("Microsoft.XMLHTTP");
    }
    objXMLHttp.onreadystatechange=callback;
    return objXMLHttp;
}