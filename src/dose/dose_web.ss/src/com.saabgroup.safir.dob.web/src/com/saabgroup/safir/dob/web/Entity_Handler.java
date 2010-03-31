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

import com.saabgroup.safir.dob.Connection;
import com.saabgroup.safir.dob.Entity;
import com.saabgroup.safir.dob.EntityIterator;
import com.saabgroup.safir.dob.EntityProxy;
import com.saabgroup.safir.dob.Item;
import com.saabgroup.safir.dob.NotFoundException;
import com.saabgroup.safir.dob.Struct;
import com.saabgroup.safir.dob.typesystem.ContainerBase;
import com.saabgroup.safir.dob.typesystem.EntityId;
import com.saabgroup.safir.dob.typesystem.HandlerId;
import com.saabgroup.safir.dob.typesystem.InstanceId;
import com.saabgroup.safir.dob.typesystem.MemberInfo;
import com.saabgroup.safir.dob.typesystem.Members;
import com.saabgroup.safir.dob.typesystem.Operations;
import com.sun.net.httpserver.Headers;
import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpHandler;
import java.io.IOException;
import java.io.OutputStream;
import java.lang.reflect.InvocationTargetException;


class Entity_Handler implements HttpHandler {

    private Connection connection;

    public Entity_Handler() {
        connection = new Connection();
    }

    public void handle(HttpExchange exchange) throws IOException {
        // check the request method and process if it is a GET
        String requestMethod = exchange.getRequestMethod();
        if (requestMethod.equalsIgnoreCase("GET")) {
            // Set response headers
            Headers responseHeaders = exchange.getResponseHeaders();

            // Get response body
            OutputStream responseBody = exchange.getResponseBody();

            connection.open("webserver", "0", 0, null, null);
            responseHeaders.set("Content-Type", "text/html");
            //* response is OK (200)
            exchange.sendResponseHeaders(200, 0);

            responseBody.write("<html>".getBytes());
            responseBody.write("<body bgcolor=\"#EAEAEA\" lang=SV link=blue vlink=blue style='tab-interval:65.2pt'>".getBytes());
            responseBody.write(("<title>" + exchange.getRequestURI().getQuery() + "</title>").getBytes());

            if (exchange.getRequestURI().getPath().endsWith("show")) {


                responseBody.write(("<b>" + exchange.getRequestURI().getQuery() + "</b><br><br>").getBytes());

                long id = Operations.getTypeId(exchange.getRequestURI().getQuery());
                long count = connection.getNumberOfInstances(id, HandlerId.ALL_HANDLERS, false);

                if (count > 0) {
                    responseBody.write(("Count : " + count + "<br>").getBytes());
                    responseBody.write(("<b>Instances : </b><br>").getBytes());
                    EntityIterator iter = connection.getEntityIterator(id, false);
                    while (iter.hasNext()) {
                        EntityProxy prox = iter.next();
                        responseBody.write(("<A HREF=\"/dob/entity/view?" + prox.getEntityId().toString() + "\">" + prox.getEntityId().getInstanceId().toString() + "</A><br>").getBytes());
                        prox.dispose();
                    }
                    iter.dispose();
                } else {
                    responseBody.write(("No objects<br>").getBytes());
                }

            } else if (exchange.getRequestURI().getPath().endsWith("view")) {

                String[] query = exchange.getRequestURI().getQuery().split(",");
                InstanceId instanceId = new InstanceId(Long.parseLong(query[1].substring(0, query[1].length() - 1).trim()));
                long typeId = Operations.getTypeId(query[0].substring(1));
                EntityId id = new EntityId(typeId, instanceId);
                try {
                    EntityProxy prox = connection.read(id);

                    Entity entity = prox.getEntity();
                    responseBody.write(("<b>" + exchange.getRequestURI().getQuery() + "</b><br>").getBytes());
                    responseBody.write(GetObject(entity, false).getBytes());

                    prox.dispose();
                } catch (NotFoundException ex) {
                    responseBody.write("<b>Failed to read entity!</b>".getBytes());
                }
            } else {
                responseBody.write("<b>Not implemented!</b>".getBytes());
            }

            responseBody.write("</body></html>".getBytes());

            // Close the responseBody
            responseBody.close();
            connection.close();
        }
    }

    private final String GetObject(com.saabgroup.safir.dob.typesystem.Object obj, boolean useTab) {
        long typeId = obj.getTypeId();
        String response = "<br>";
        String tab = "";

        if (useTab) {
            tab = "&nbsp&nbsp";
        }

        for (int i = 0; i < Members.getNumberOfMembers(typeId); i++) {
            MemberInfo info = Members.getInfo(typeId, i);
            String memberName = Members.getName(typeId, i);
            ContainerBase cont;
            try {
                // array?
                if (info.getIsArray()) {
                    response = response.concat(("<b>" + memberName + " : </b><br>"));
                    for (int idx = 0; idx < info.getArrayLength(); idx++) {
                        cont = obj.getMember(i, idx);
                        response = response.concat(" &nbsp &nbsp ");
                        if (cont.isNull()) {
                            response = response.concat(("Index<b> " + idx + " :  NULL </b><br>"));
                        } else {
                            // item or struct?
                            if (Operations.isOfType(info.getMemberTypeId(), Item.ClassTypeId) || Operations.isOfType(info.getMemberTypeId(), Struct.ClassTypeId)) {
                                response = response.concat(("Index<b> " + idx + " : </b>" + GetObject((com.saabgroup.safir.dob.typesystem.Object) cont.getClass().getMethod("getObj", (Class<?>[]) null).invoke(cont, (Object[]) null), true)));
                            } else {
                                response = response.concat(("Index<b> " + idx + " : </b>" + cont.getClass().getMethod("getVal", (Class<?>[]) null).invoke(cont, (Object[]) null) + "<br>"));
                            }
                        }
                    }
                } else {
                    cont = obj.getMember(i, 0);
                    if (cont.isNull()) {
                        response = response.concat((tab + "<b>" + Members.getName(typeId, i) + " : NULL </b><br>"));
                    } else {
                        // item or struct?
                        if (Operations.isOfType(info.getMemberTypeId(), Item.ClassTypeId) || Operations.isOfType(info.getMemberTypeId(), Struct.ClassTypeId)) {
                            response = response.concat(("<b>" + Members.getName(typeId, i) + " : </b>" + GetObject((com.saabgroup.safir.dob.typesystem.Object) cont.getClass().getMethod("getObj", (Class<?>[]) null).invoke(cont, (Object[]) null), true)));
                        } else {
                            response = response.concat(tab + "<b>" + memberName + " : </b>" + cont.getClass().getMethod("getVal", (Class<?>[]) null).invoke(cont, (Object[]) null) + "<br>");
                        }
                    }
                }
            } catch (IllegalAccessException ex) {
                response = response.concat("<b>IllegalAccessException!</b><br>");
            } catch (IllegalArgumentException ex) {
                response = response.concat("<b>IllegalArgumentException!</b><br>");
            } catch (InvocationTargetException ex) {
                response = response.concat("<b>InvocationTargetException!</b><br>");
            } catch (NoSuchMethodException ex) {
                response = response.concat("<b>NoSuchMethodException!</b><br>");
            } catch (SecurityException ex) {
                response = response.concat("<b>SecurityException!</b><br>");
            }
        }

        return response;
    }
}
