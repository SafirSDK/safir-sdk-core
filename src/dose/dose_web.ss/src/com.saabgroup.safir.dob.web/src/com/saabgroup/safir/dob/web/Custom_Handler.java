// -*- coding: utf-8 -*-
/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
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

import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpHandler;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.OutputStream;

class Custom_Handler implements HttpHandler {

    Dots_Handler DotsHandler = new Dots_Handler();

    public String path() {
        return System.getenv("SAFIR_RUNTIME") + com.saabgroup.safir.dob.web.Parameters.getHtmlDir() + "/";
    }

    @Override
    public void handle(HttpExchange exchange) throws IOException {
        // check the request method and process if it is a GET
        String requestMethod = exchange.getRequestMethod();
        if (requestMethod.equalsIgnoreCase("GET")) {


            if (exchange.getRequestURI().getPath().endsWith("show")) {
                DotsHandler.handle(exchange);
                return;
            }


            // Get response body
            OutputStream responseBody = exchange.getResponseBody();

            //* response is OK (200)
            exchange.sendResponseHeaders(200, 0);
            String reqPath = exchange.getRequestURI().getPath();

            if (reqPath.endsWith("/")) {
                reqPath = reqPath.concat("index.html");
            }

            if (reqPath.startsWith("/custom/")) {
                reqPath = reqPath.substring("/custom/".length());
            }

            File f = new File(path() + reqPath);
            FileInputStream fs = new FileInputStream(f);
            byte[] b = new byte[1024];
            int i = fs.read(b);
            while (i != -1) {
                responseBody.write(b, 0, i);
                i = fs.read(b);
            }
            fs.close();
            // Close the responseBody
            responseBody.close();
        }

    }
}
