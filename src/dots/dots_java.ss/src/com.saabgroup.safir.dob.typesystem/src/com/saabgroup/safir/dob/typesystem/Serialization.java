// -*- coding: utf-8 -*-
/******************************************************************************
*
* Copyright Saab AB, 2005-2013 (http://safir.sourceforge.net)
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
 * Serialization and deserialization of objects, xml, json and base64.
 */
public class Serialization {
    /**
     * Serialize an object to XML.
     *
     * @param obj - The object to serialize
     * @return The xml serialization
     * @exception IllegalValueException - There is something wrong with the object.
     */
    public static String toXml(com.saabgroup.safir.dob.typesystem.Object obj)
    {
    	byte[] bin=toBinary(obj);
    	return toXml(bin);
    }
    
    /**
     * Convert a binary serialization to XML.
     *
     * @param binary The binary serialization to convert to xml.
     * @return The xml of the binary serialization.
     */
    public static String toXml(byte [] binary) {
        java.nio.ByteBuffer blob = java.nio.ByteBuffer.allocateDirect(binary.length);
        blob.put(binary);
        return toXml(blob);
    }
    
    /**
     * Convert a blob to XML.
     *
     * @param blob - the blob to convert to xml.
     * @return The xml of the blob.
     */
    public static String toXml(java.nio.ByteBuffer blob) {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        String res = Kernel.BlobToXml(blob);
        if (res == null) {
            throw new SoftwareViolationException("Error in serialization buffer sizes!!!");
        }
        return res;
    }


    /**
     * Deserialize an XML serialization.
     *
     * This method creates a new object from a given xml serialization.
     * It uses the ObjectFactory to accomplish this.
     *
     * @param xml The xml to convert.
     * @return A boost::shared_ptr to the new object
     * @exception IllegalValueException If the type represented by the serialization isn't found
     *                                   in the ObjectFactory.
     */
    public static com.saabgroup.safir.dob.typesystem.Object toObject(String xml) {
        java.nio.ByteBuffer blob [] = new java.nio.ByteBuffer[1];
        java.nio.ByteBuffer deleter [] = new java.nio.ByteBuffer[1];
        Kernel.XmlToBlob(blob, deleter, xml);
        if (blob[0] == null) {
            throw new IllegalValueException("Something is wrong with the XML-formated object");
        }
        com.saabgroup.safir.dob.typesystem.Object obj = ObjectFactory.getInstance().createObject(blob[0]);
        Kernel.InvokeDeleter(deleter[0],blob[0]);
        return obj;
    }

    

    

    /**
     * Serialize an object to JSON.
     *
     * @param obj - The object to serialize
     * @return The json serialization
     * @exception IllegalValueException - There is something wrong with the object.
     */
    public static String toJson(com.saabgroup.safir.dob.typesystem.Object obj)
    {
    	byte[] bin=toBinary(obj);
    	return toJson(bin);
    }
    
    /**
     * Convert a binary serialization to JSON.
     *
     * @param binary The binary serialization to convert to json.
     * @return The json of the binary serialization.
     */
    public static String toJson(byte [] binary) {
        java.nio.ByteBuffer blob = java.nio.ByteBuffer.allocateDirect(binary.length);
        blob.put(binary);
        return toJson(blob);
    }

    /**
     * Convert a blob to JSON.
     *
     * @param blob - the blob to convert to json.
     * @return The json of the blob.
     */
    public static String toJson(java.nio.ByteBuffer blob) {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        String res = Kernel.BlobToJson(blob);
        if (res == null) {
            throw new SoftwareViolationException("Error in serialization buffer sizes!!!");
        }
        return res;
    }


    /**
     * Deserialize an JSON serialization.
     *
     * This method creates a new object from a given json serialization.
     * It uses the ObjectFactory to accomplish this.
     *
     * @param json The json to convert.
     * @return A boost::shared_ptr to the new object
     * @exception IllegalValueException If the type represented by the serialization isn't found
     *                                   in the ObjectFactory.
     */
    public static com.saabgroup.safir.dob.typesystem.Object toObjectFromJson(String json) {
        java.nio.ByteBuffer blob [] = new java.nio.ByteBuffer[1];
        java.nio.ByteBuffer deleter [] = new java.nio.ByteBuffer[1];
        Kernel.JsonToBlob(blob, deleter, json);
        if (blob[0] == null) {
            throw new IllegalValueException("Something is wrong with the JSON-formated object");
        }
        com.saabgroup.safir.dob.typesystem.Object obj = ObjectFactory.getInstance().createObject(blob[0]);
        Kernel.InvokeDeleter(deleter[0],blob[0]);
        return obj;
    }

    

    /**
     * Serialize an object to binary form.
     *
     * The serialization is put into a variable of type BinarySerialization, which
     * is of type std::vector<char>. If you need to get hold of a "raw" C-pointer to the data
     * use &binary[0]. See Effective STL Item 16 for more info.
     *
     * @param obj The object to serialize.
     * @return The object serialized to binary form.
     * @exception IllegalValueException - There is something wrong with the object.
     */
    public static byte[] toBinary(com.saabgroup.safir.dob.typesystem.Object obj){
        
    	long handle = Kernel.CreateBlobWriter(obj.getTypeId());
        obj.writeToBlob (handle);
        int size = Kernel.CalculateBlobSize (handle);
        java.nio.ByteBuffer blob=java.nio.ByteBuffer.allocateDirect(size);
        Kernel.WriteBlob(handle, blob);
        Kernel.DeleteBlobWriter(handle);
        blob.clear(); //reset read position
        byte [] binary = new byte[size];
        blob.get(binary, 0, binary.length);
        return binary;
    }

    /**
     * Deserialize a binary serialization and create an object.
     *
     * It uses the ObjectFactory to accomplish this.
     * If you have a char * that you want to deserialize the easiest way is to
     * pass it to the ObjectFactory instead.
     *
     * @param binary - The binary serialization to deserialize.
     * @return The deserialized object.
     * @exception IllegalValueException If the type represented by the serialization isn't found
     *                                   in the ObjectFactory.
     */
        public static com.saabgroup.safir.dob.typesystem.Object toObject(byte[] binary) {
            java.nio.ByteBuffer blob = java.nio.ByteBuffer.allocateDirect(binary.length);
            blob.clear(); //reset position
            blob.put(binary);
            return ObjectFactory.getInstance().createObject(blob);
        }
}
