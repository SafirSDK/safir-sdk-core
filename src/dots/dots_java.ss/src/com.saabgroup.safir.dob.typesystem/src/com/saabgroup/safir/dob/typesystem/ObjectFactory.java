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
            Class<?> theClass = Class.forName(getClassName(typeId));
            return (Object) theClass.getConstructor(java.nio.ByteBuffer.class).newInstance(blob);
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
            Class<?> theClass = Class.forName(getClassName(typeId));
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

    /**
     * Register a class in the object factory along with its constructors.
     *
     * @param typeId typeid of the class.
     * @param typeIdConstructor A constructor that takes one argument, typeid.
     * @param blobConstructor A constructor that takes one argument, a blob.
     */
    public void registerClass(long typeId,
                              java.lang.reflect.Constructor<?> typeIdConstructor,
                              java.lang.reflect.Constructor<?> blobConstructor){
        m_callbackMap.put(typeId, new Constructors(typeIdConstructor,blobConstructor));
    }

    private static final ObjectFactory m_instance = new ObjectFactory();

    private class Constructors
    {
        Constructors(java.lang.reflect.Constructor<?> typeIdConstructor,
                     java.lang.reflect.Constructor<?> blobConstructor){
            m_typeIdConstructor = typeIdConstructor;
            m_blobConstructor = blobConstructor;
        }

        java.lang.reflect.Constructor<?> m_typeIdConstructor;
        java.lang.reflect.Constructor<?> m_blobConstructor;
    }

    private java.util.HashMap<Long, Constructors> m_callbackMap = new java.util.HashMap<Long, Constructors>();

}
