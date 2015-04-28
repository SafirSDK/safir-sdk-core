// -*- coding: utf-8 -*-
/******************************************************************************
*
* Copyright Saab AB, 2009-2013 (http://safir.sourceforge.net)
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



/**
 * This class is an object factory for all automatically generated DOB classes.
 *
 * Each generated class automatically registers itself with this class.
 *
 * Users can call the CreateObject(TypeId) routine to create objects of a desired type
 * (this is if they receive the type id from some other application so that they cannot
 * call the Create routine of the class itself directly).
 */
public class ObjectFactory {

    private ObjectFactory(){
        NamespaceMappings.getInstance();
    }

    /**
     * Get the instance of the singleton.
     *
     * @return The instance of the singleton.
     */
    public static ObjectFactory getInstance()
    {
        return m_instance;
    }

    /**
     * Create a new object from a blob.
     *
     * This method takes a blob and extracts the typeId from it and then calls the
     * appropriate callback to create the object.
     *
     * @param blob [in] - The blob to deserialize.
     * @return A boost::shared_ptr to the object.
     * @exception IllegalValueException If the type represented by the blob isn't found
     *                                   in the ObjectFactory.
    */

    public com.saabgroup.safir.dob.typesystem.Object createObject(java.nio.ByteBuffer blob){
        if (!blob.isDirect()) {
            throw new SoftwareViolationException("blob ByteBuffer passed to createObject must be direct");
        }

        long typeId = BlobOperations.getTypeId(blob);                        
        
        try {
            long handle=Kernel.CreateBlobReader(blob);
            Class<?> theClass = getClass(typeId);
            Object obj = (Object) theClass.getConstructor(long.class).newInstance(handle);
            Kernel.DeleteBlobReader(handle);
            return obj;
        } catch (java.lang.Exception e) {
            e.printStackTrace();
            throw new SoftwareViolationException("Failed to create object (from blob) of type " + typeId + ". Got exception " + e.toString());
        }
    }

    /**
     * Create a new "empty" object from a TypeId.
     *
     * This method takes a TypeId and calls the
     * appropriate callback to create an object of the desired type.
     *
     * @param typeId The TypeId of the object to create.
     * @return The newly created object.
     * @exception IllegalValueException If the type couldn't be found in the ObjectFactory.
    */
    public com.saabgroup.safir.dob.typesystem.Object createObject(long typeId){
        try {
            Class<?> theClass = getClass(typeId);
            return (Object) theClass.getConstructor().newInstance();
        } catch (java.lang.Exception e) {
            throw new SoftwareViolationException("Failed to create object (from blob) of type " + typeId + ". Got exception " + e.toString());
        }
    }

    private String getClassName(long typeId){
        String fullClassName = Operations.getName(typeId);

        //Object is a "special name".
        if (fullClassName.equals("Object")) {
            fullClassName = "Safir.Dob.Typesystem.Object";
        }

        return NamespaceMappings.getInstance().toJava(fullClassName);
    }


    private Class<?> getClass(long typeId) throws ClassNotFoundException {
        //first try jars that were on our classpath
        try {
            return Class.forName(getClassName(typeId));
        }
        catch (ClassNotFoundException e) {
        }

        try {
            //then try the jars from typesystem.ini
            return m_classLoader.loadClass(getClassName(typeId));
        }
        catch (ClassNotFoundException e) {
            throw new ClassNotFoundException(e.toString()
                                             + "\nLoaded jars are '" 
                                             + m_classLoader.getJarList() + "'");
        }

    }

    private class GeneratedClassLoader extends java.net.URLClassLoader
    {
        public GeneratedClassLoader()
        {
            super (new java.net.URL[0]);
            m_generatedJars = Kernel.GetGeneratedJars();
            for (String file : m_generatedJars)
            {
                try
                {
                    java.io.File jar = new java.io.File(file);
                    addURL (jar.toURI().toURL());
                }
                catch (java.net.MalformedURLException e)
                {
                    throw new SoftwareViolationException("Failed to load jar file " 
                                                         + file 
                                                         + ". Got exception " 
                                                         + e.toString());
            
                }
            }
        }
        
        public String getJarList() {
            java.lang.StringBuilder builder = new java.lang.StringBuilder();
            boolean first = false;
            for (String jar : m_generatedJars) {
                if (!first) {
                    first = true;
                }
                else {
                    builder.append(", ");
                }
                builder.append(jar);
            }
            return builder.toString();
        }

        private String[] m_generatedJars = null;
    }

    private static final ObjectFactory m_instance = new ObjectFactory();
    private GeneratedClassLoader m_classLoader = new GeneratedClassLoader();
}
