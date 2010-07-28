package org.coode.www;

import org.coode.html.impl.OWLHTMLParam;

import javax.servlet.http.HttpServletRequest;
import java.io.IOException;
import java.net.URL;

/**
 * Author: drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jul 26, 2010<br><br>
 */
public class ServletUtils {

    // requestURL on its own is not good enough - doesn't include params
    // Need to rebuild the URL
    public static URL rebuildRequestURL(HttpServletRequest request) throws IOException {
        StringBuilder requestURL = new StringBuilder(request.getRequestURL().toString());
        boolean appendedParams = false;

        String query = request.getQueryString();
        if (query != null){
            for (String param : query.split("&")){
                if (!param.startsWith(OWLHTMLParam.session.name())){
                    if (appendedParams){
                        requestURL.append("&");
                    }
                    else{
                        requestURL.append("?");
                        appendedParams = true;
                    }
                    requestURL.append(param);
                }
            }
        }
        return new URL(requestURL.toString());
    }

    
}
