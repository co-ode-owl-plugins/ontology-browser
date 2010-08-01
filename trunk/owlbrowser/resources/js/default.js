

var xmlHttpOption;  // XML HTTP object for option
var onSuccess; // page to go to once the option has been set
var optionsURL;

////////////////////////////////////////////////////////////////////////////////////////////

function optionFromSelect(select){
    Item = select.selectedIndex;
    Result = select.options[Item].text;
    optionId = select.form.id; // get the ID of the form

    option(optionId, Result, null);
}

// successPage is optional
// - if specified, this page will be loaded when the option is set successfully
// - if omitted, the current page will be refreshed when the option is set successfully
function option(opt, value, successpage){

    xmlHttpOption=GetXmlHttpObject(optionSet);

    if (xmlHttpOption==null) {
        alert ("Browser does not support HTTP Request");
    }
    else{
        xmlHttpOption.open("POST", optionsURL, true);

        xmlHttpOption.setRequestHeader("Content-Type", "application/x-www-form-urlencoded; charset=UTF-8");

        var attrs =  "property=" + opt + "&value=" + value + "&format=xml";

        onSuccess = successpage;

        xmlHttpOption.send(attrs);
    }
}

function optionSet(){
    if (xmlHttpOption.readyState==4 || xmlHttpOption.readyState=="complete") {
        if (onSuccess != null){
            if (parent != null){
                parent.window.location = onSuccess;
            }
            else{
                window.location = onSuccess;
            }
        }
        else{
            reloadAllFrames();
        }
    }
}

function reloadAllFrames(){
    if (parent.frames.length > 0){
        for(var i=0; i<parent.frames.length; i++){
            var sURL = parent.frames[i].location;
            parent.frames[i].location.replace(sURL);
        }
    }
    else{
        // we have to strip out the session if it is in the url
        // otherwise the previous state will just be reloaded
        var url = location.toString();
        url = url.replace(/[&|?]session=[^&]+/, "");
        location.replace(url);
    }
}

function getContentURL(){
    if (parent.frames.length > 0){
        return parent.frames["content"].location;
    }
    else{
        return ONT_SERVER;
    }
}

function GetXmlHttpObject(callback) {
    var objXMLHttp=null;
    if (window.XMLHttpRequest) {
        objXMLHttp=new XMLHttpRequest();
    }
    else if (window.ActiveXObject) {
        objXMLHttp=new ActiveXObject("Microsoft.XMLHTTP");
    }
    objXMLHttp.onreadystatechange=callback;
    return objXMLHttp;
}
