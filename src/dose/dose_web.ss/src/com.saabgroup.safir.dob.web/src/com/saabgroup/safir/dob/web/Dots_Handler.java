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

import com.saabgroup.safir.dob.typesystem.Operations;
import com.sun.net.httpserver.Headers;
import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpHandler;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Collections;

class Dots_Handler implements HttpHandler {

    public File FindFile(String dirName, String fileName) {
        File dir = new File(dirName);
        File f = new File(dirName + File.separator + fileName);
        if (f.exists()) {
            return f;
        }

        String objects[] = dir.list();
        for (int i = 0; i < objects.length; i++) {
            File f1 = new File(dirName + File.separator + objects[i]);
            if (f1.isDirectory()) {
                File f2 = FindFile(f1.getAbsolutePath(), fileName);
                if (f2 != null) {
                    return f2;
                }
            }
        }
        return null;
    }

    @Override
    public void handle(HttpExchange exchange) throws IOException {
        // check the request method and process if it is a GET
        String requestMethod = exchange.getRequestMethod();
        if (requestMethod.equalsIgnoreCase("GET")) {
            // Set response headers
            Headers responseHeaders = exchange.getResponseHeaders();

            // Get response body
            OutputStream responseBody = exchange.getResponseBody();

            if (exchange.getRequestURI().getPath().endsWith("show")) {
                responseHeaders.set("Content-Type", "text/plain");
                //* response is OK (200)
                exchange.sendResponseHeaders(200, 0);

                File f = FindFile(System.getenv("SAFIR_RUNTIME") + "/data/text/dots/classes/", exchange.getRequestURI().getQuery() + ".dou");
                FileInputStream fs = new FileInputStream(f);
                byte[] b = new byte[1024];
                int i = fs.read(b);
                while (i != -1) {
                    responseBody.write(b, 0, i);
                    i = fs.read(b);
                }
                fs.close();

            } else if (exchange.getRequestURI().getPath().endsWith("view")) {

                responseHeaders.set("Content-Type", "text/html");
                //* response is OK (200)
                exchange.sendResponseHeaders(200, 0);

                responseBody.write("<html>".getBytes());
                responseBody.write("<body bgcolor=\"#EAEAEA\" lang=SV link=blue vlink=blue style='tab-interval:65.2pt'>".getBytes());
                responseBody.write(("<title>" + exchange.getRequestURI().getQuery() + "</title>").getBytes());
                responseBody.write(("<h3>" + exchange.getRequestURI().getQuery() + "</h3>").getBytes());

                long[] classIds;

                if (exchange.getRequestURI().getQuery().equals("Entities")) {
                    classIds = Operations.getClassTree(com.saabgroup.safir.dob.Entity.ClassTypeId);
                } else if (exchange.getRequestURI().getQuery().equals("Parameters")) {
                    classIds = Operations.getClassTree(com.saabgroup.safir.dob.Parametrization.ClassTypeId);
                } else if (exchange.getRequestURI().getQuery().equals("Items")) {
                    classIds = Operations.getClassTree(com.saabgroup.safir.dob.Item.ClassTypeId);
                } else if (exchange.getRequestURI().getQuery().equals("Structs")) {
                    classIds = Operations.getClassTree(com.saabgroup.safir.dob.Struct.ClassTypeId);
                } else if (exchange.getRequestURI().getQuery().equals("Messages")) {
                    classIds = Operations.getClassTree(com.saabgroup.safir.dob.Message.ClassTypeId);
                } else if (exchange.getRequestURI().getQuery().equals("Responses")) {
                    classIds = Operations.getClassTree(com.saabgroup.safir.dob.Response.ClassTypeId);
                } else if (exchange.getRequestURI().getQuery().equals("Services")) {
                    classIds = Operations.getClassTree(com.saabgroup.safir.dob.Service.ClassTypeId);
                } else if (exchange.getRequestURI().getQuery().equals("All")) {
                    classIds = Operations.getClassTree(com.saabgroup.safir.dob.typesystem.Object.ClassTypeId);
                } else {
                    classIds = new long[0];
                }

                ArrayList<String> list = new ArrayList<String>();
                for (int i = 0; i < classIds.length; i++) {
                    list.add(com.saabgroup.safir.dob.typesystem.Operations.getName(classIds[i]));
                }
                Collections.sort(list);
                if (exchange.getRequestURI().getQuery().equals("Entities")) {
                    for (int i = 0; i < list.size(); i++) {
                        responseBody.write(("<A HREF=\"show?" + list.get(i) + "\"> " + list.get(i) + "</A> <A HREF=\"/dob/entity/show?" + list.get(i) + "\"><b> - Data </b><br>").getBytes());
                    }
                } else {
                    for (int i = 0; i < list.size(); i++) {
                        responseBody.write(("<A HREF=\"show?" + list.get(i) + "\"> " + list.get(i) + "</A><br>").getBytes());
                    }
                }
                responseBody.write("</body></html>".getBytes());

            }

            // Close the responseBody
            responseBody.close();
        }
    }
}
