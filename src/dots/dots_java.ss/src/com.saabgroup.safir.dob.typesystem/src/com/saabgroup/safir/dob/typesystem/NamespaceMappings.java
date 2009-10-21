// -*- coding: utf-8 -*-
/******************************************************************************
*
* Copyright Saab AB, 2009 (http://www.safirsdk.com)
*
* Created by: Lars Hagström / stlrha
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
package com.saabgroup.safir.dob.typesystem;
import java.io.*;

/**
 * This singleton can be used to find out mappings between the "dou-namespaces"
 * and the java namespaces.
 *
 * These are defined in the dots_generated/XXX-java.namespace.txt files.
 */
public class NamespaceMappings {
    /**
     * Get the instance of the singleton.
     *
     * @return The instance of the singleton.
     */
    public static NamespaceMappings getInstance()
    {
        return m_instance;
    }

    private static String join(String [] strings, int last){
        StringWriter wr = new StringWriter();
        for (int i = 0; i <= last; ++i){
            wr.write(strings[i]);
            if (i != last) {
                wr.write('.');
            }
        }
        return wr.toString();
    }

    /**
     * Convert a "dou" namespace to a java namespace, according to the rules set up 
     * in the xxx-java.namespace.txt files.
     * 
     * For example, calling this function with "Safir.Dob.Entity" will yield
     * "com.saabgroup.safir.dob.Entity" (assuming that there is a file named
     * "Safir-java.namespace.txt" containing "com.saabgroup" in the correct directory).
     * 
     * @param douFullClassName Fully qualified "dou" namespace.
     * @return a fully qualified java namespace.
     */
    public String toJava(String douFullClassName) {
        int lastDot = douFullClassName.lastIndexOf('.');
        String className = douFullClassName.substring(lastDot);
        String namespace = douFullClassName.substring(0,lastDot);

        String split [] = douFullClassName.split("\\.");

        if (split.length < 2){
            throw new SoftwareViolationException("NamespaceMappings.toJava got a class name that had an unexpected format: " + douFullClassName);
        }

        for (int i = split.length - 1; i >= 0; --i)
        {
            String ns = join(split,i);
            String javaNs = m_toJavaMapping.get(ns);

            if (javaNs != null) {
                return javaNs + "." + namespace.toLowerCase() + className;
            }
        }
        return namespace.toLowerCase() + className;
    }

    static private String readNamespacePrefix(String filename){
        try{
            File mappingFile = new File(filename);
            BufferedReader reader = new BufferedReader(new FileReader(mappingFile));
            
            while(reader.ready()) {
                String line = reader.readLine();
                if (line.matches("^[a-zA-Z][a-zA-Z\\.]+")) {
                    return line.trim();
                }
            }
        }
        catch(java.io.FileNotFoundException exc) {
            exc.printStackTrace();
        }
        catch(java.io.IOException exc) {
            exc.printStackTrace();
        }
        throw new SoftwareViolationException("Failed to find a valid namespace in " + filename);
    }

    private NamespaceMappings() {
        String dirpath = System.getenv("SAFIR_RUNTIME") + "/data/text/dots/classes/";
        File dir = new File(dirpath);
        String [] children = dir.list(new FilenameFilter() {
                public boolean accept(File dir, String name) {
                    return name.endsWith("-java.namespace.txt");
                }
            });
        
        for (String str: children) {
            String javaNamespacePrefix = readNamespacePrefix(dirpath + str);
            String douNamespace = str.substring(0,str.lastIndexOf("-java.namespace.txt"));
            m_toJavaMapping.put(douNamespace, javaNamespacePrefix);
        }
    }

    private static final NamespaceMappings m_instance = new NamespaceMappings();

    private java.util.Map<String, String> m_toJavaMapping = new java.util.HashMap<String, String>();
}
