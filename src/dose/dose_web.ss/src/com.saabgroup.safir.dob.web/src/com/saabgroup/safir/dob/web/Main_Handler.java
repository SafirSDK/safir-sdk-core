// -*- coding: utf-8 -*-
/******************************************************************************
*
* Copyright Saab AB, 2007-2010 (http://www.safirsdk.com)
*
* Created by: Mikael Wennerberg / stmiwn
*
*******************************************************************************
*
* This file is part of Safir SDK Core.
*
* Safir SDK Core is free software: you can redistribute it and/or modify
* it under the terms of version 3 of the GNU General Public License as
* published by the Free Software Foundation.
*
* Safir SDK Core is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with Safir SDK Core.  If not, see <http://www.gnu.org/licenses/>.
*
******************************************************************************/
package com.saabgroup.safir.dob.web;

import com.sun.net.httpserver.Headers;
import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpHandler;
import java.io.IOException;
import java.io.OutputStream;

class Main_Handler implements HttpHandler {

    public void handle(HttpExchange exchange) throws IOException {
        // check the request method and process if it is a GET
        String requestMethod = exchange.getRequestMethod();
        if (requestMethod.equalsIgnoreCase("GET")) {
            // Set response headers
            Headers responseHeaders = exchange.getResponseHeaders();
            responseHeaders.set("Content-Type", "text/html");
            //* response is OK (200)
            exchange.sendResponseHeaders(200, 0);

            // Get response body
            OutputStream responseBody = exchange.getResponseBody();

            responseBody.write("<html>".getBytes());
            responseBody.write("<body bgcolor=\"#EAEAEA\" lang=SV link=blue vlink=blue style='tab-interval:65.2pt'>".getBytes());
            responseBody.write("<title>DOB webserver</title>".getBytes());
            responseBody.write("<h3>DOB WEB SERVER </h3>".getBytes());

            responseBody.write(("<b>Node : </b>" + com.saabgroup.safir.dob.ThisNodeParameters.getNodeNumber() + "<br>").getBytes());
            responseBody.write(("<b>Address : </b>" + exchange.getLocalAddress().toString() + "<br>").getBytes());

            responseBody.write("<b>SAFIR_RUNTIME : </b>".getBytes());
            responseBody.write(System.getenv("SAFIR_RUNTIME").getBytes());
            responseBody.write("<br><b>SAFIR_SDK : </b>".getBytes());
            responseBody.write((System.getenv("SAFIR_SDK") + "<br>").getBytes());

            responseBody.write("<br><A HREF=\"/docs/\"> <b>Documentation</b></A>".getBytes());
            responseBody.write("<br><A HREF=\"/custom/\"> <b>Custom</b></A>".getBytes());

            responseBody.write("<br><br><A HREF=\"dots/view?Entities\"> view entities</A>".getBytes());
            responseBody.write("<br><A HREF=\"dots/view?Parameters\"> view parameters</A>".getBytes());
            responseBody.write("<br><A HREF=\"dots/view?Items\"> view items</A>".getBytes());
            responseBody.write("<br><A HREF=\"dots/view?Structs\"> view structs</A>".getBytes());
            responseBody.write("<br><A HREF=\"dots/view?Messages\"> view messages</A>".getBytes());
            responseBody.write("<br><A HREF=\"dots/view?Services\"> view services</A>".getBytes());
            responseBody.write("<br><A HREF=\"dots/view?Responses\"> view responses</A>".getBytes());
            responseBody.write("<br><A HREF=\"dots/view?All\"> view all</A>".getBytes());

            responseBody.write("</body></html>".getBytes());

            // Close the responseBody
            responseBody.close();
        }
    }
}
