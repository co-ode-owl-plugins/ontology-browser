package org.coode.html.util;

import org.apache.log4j.Logger;

import java.net.URL;
import java.util.*;

/**
 * Author: Nick Drummond<br>
 * nick.drummond@cs.manchester.ac.uk<br>
 * http://www.cs.man.ac.uk/~drummond<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jun 18, 2007<br><br>
 * <p/>
 * code made available under Mozilla Public License (http://www.mozilla.org/MPL/MPL-1.1.html)<br>
 * copyright 2006, The University of Manchester<br>
 */
public class URLUtils {

    private static final Logger logger = Logger.getLogger(URLUtils.class.getName());

//    public static String createRelativeURL(URL current, URL target) {
//        try {
//            URI currentURI = current.toURI();
//            URI targetURI = target.toURI();
//            URI rel = currentURI.relativize(targetURI);
//            logger.debug("rel = " + rel);
//            return rel.toString();
//        }
//        catch (URISyntaxException e) {
//            logger.error(e);
//        }
//        return "";
//    }

    public static String createRelativeURL(URL current, URL target) {

        try{
            if (current.equals(target)){
                return "";
            }
            else if (!current.getHost().equals(target.getHost())){ // if host different, can only use absolute URL
                return target.toString();
            }
        }
        catch(Throwable e){
            logger.debug(current);
            logger.debug(target);
        }

        List<String> currentPath = new ArrayList<String>(Arrays.asList(current.getPath().split("/")));
        List<String> targetPath = new ArrayList<String>(Arrays.asList(target.getPath().split("/")));

        // strip off empty path elements at the start of each path
        if (!currentPath.isEmpty() && currentPath.get(0).equals("")){
            currentPath.remove(0);
        }
        if (!targetPath.isEmpty() && targetPath.get(0).equals("")){
            targetPath.remove(0);
        }

        // strip all of the common path from each
        while(true){
            if (!currentPath.isEmpty() && !targetPath.isEmpty() &&
                currentPath.get(0).equals(targetPath.get(0))){
                currentPath.remove(0);
                targetPath.remove(0);
            }
            else{
                break;
            }
        }

        StringBuffer relativeURL = new StringBuffer();

        int currentSubCount = currentPath.size();
        if (!current.getPath().endsWith("/")){ // then there must be a file at the end
            currentSubCount--;
        }

        for (int i=0; i<currentSubCount; i++){
            relativeURL.append("../");
        }

        for (String s: targetPath){
            relativeURL.append(s);
            relativeURL.append("/");
        }

//        if (relativeURL.equals("")){
//            relativeURL.append(".");
//        }

        // unless the original path ends in "/", remove it
        int len = relativeURL.length();
        if (len > 0 &&
            relativeURL.charAt(len -1)=='/' &&
            !target.getPath().endsWith("/")){
            relativeURL.deleteCharAt(len -1);
        }

        if (target.getQuery() != null){
            if (len == 0){
                relativeURL.append("./"); // otherwise it won't work
            }
            relativeURL.append("?");
            relativeURL.append(target.getQuery());
        }

        if (relativeURL.length() == 0){ // will be achieved if current = a.com/?something=la   and target = a.com/
            return ".";
        }
        else{
            return relativeURL.toString();
        }
    }

    public static Map<String, String> getParams(URL url) {
        Map<String, String> paramMap = new HashMap<String, String>();
        String query = url.getQuery();
        if (query != null){
            String[] params = query.split("&");
            for (String param : params) {
                String[] pair = param.split("=");
                paramMap.put(pair[0], pair[1]);
            }
        }
        return paramMap;

    }
}
