// -*- coding: utf-8 -*-
/******************************************************************************
*
* Copyright Saab AB, 2008-2013 (http://safirsdkcore.com)
*
* Created by: Henrik Sundberg / sthesu
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

import java.io.UnsupportedEncodingException;

/**
 * Class containing the identity of an instance.
 */
public class InstanceId implements Comparable<InstanceId> {

    /**
     * Returns a random instance id.
     *
     * @return A random instance id.
     */
    public static InstanceId generateRandom(){
        return new InstanceId(Kernel.GenerateRandom64());
    }

    /**
     * Default constructor.
     *
     * Creates an unspecified instance id.
     */
    public InstanceId(){
        this.m_instanceId = -1;
        this.m_instanceIdStr = "";
    }

    /**
     * Constructor.
     *
     * Creates a instance id from the given string.
     *
     * @param id String identifying the instance.
     */
    public InstanceId(String id){
        if(id == null || id.length() == 0){
            throw new SoftwareViolationException("InstanceId can not be generated from null/empty string");
        }
        this.m_instanceId = Kernel.Generate64(id);
        this.m_instanceIdStr = id;
    }

    /**
     * Constructor.
     *
     * Creates an instance id using a 64 bit integer.
     *
     * @param id The 64bit integer id of the instance.
     */
    public InstanceId(long id){
        this.m_instanceId = id;
        this.m_instanceIdStr = "";
    }

    /**
     * Constructor.
     *
     * Creates a instance id from the given data.
     *
     * @param id Identifier identifying the instance.
     * @param idStr String identifying the instance.
     */
    public InstanceId(long id, String idStr){
        this.m_instanceId = id;
        this.m_instanceIdStr = idStr;
        if (idStr == null){
            throw new SoftwareViolationException("String argument to InstanceId constructor cannot be null");
        }
    }

    /**
     * Remove the included string from the instance id.
     *
     * This is meant to be used when this type is used as a member of a Dob object.
     * Using this call before the object gets serialized to binary or xml (i.e.
     * also before sending it anywhere) means that the string will not be included
     * when the object is sent.
     */
    public void removeString(){
        this.m_instanceIdStr = "";
    }

    /**
     * Equals.
     *
     * @param other The handler id to compare with.
     * @return True if the handler ids are equal.
     */
    public boolean equals(java.lang.Object other){
        if ((other == null) || !(other instanceof InstanceId)){
            return false;
        }
        else {
            return m_instanceId == ((InstanceId)other).m_instanceId;
        }
    }

    /**
     * Return a string representation of the instance id.
     * If the string that created the instance id is available this is the string that will be returned,
     * otherwise it is the number that will be returned.
     *
     * The purpose of this function is for debug output and such.
     * The resulting string can *not* reliably be used in the "string constructor" for InstanceId to
     * recreate the same InstanceId.
     *
     * @return String representation of the handler id.
     */
    public String toString(){
        if (m_instanceIdStr.length() > 0){
            return m_instanceIdStr;
        }
        else{
            return java.lang.Long.toString(m_instanceId);
        }
    }

    /**
     * Overridden base class method.
     *
     * @return Hash code.
     */
    public int hashCode(){
        return (int)m_instanceId;
    }

    /**
     * Get the raw 64 bit integer identifier.
     *
     * @return The raw 64 bit identifier.
     */
    public long getRawValue(){
        return m_instanceId;
    }

    /**
     * Get the string that was used to create this id.
     *
     * If no string was used this method returns an empty string.
     *
     * @return The string (if any) that was used to create this id.
     */
    public String getRawString(){
        return m_instanceIdStr;
    }

    /**
     * Get the length of the string when converted to UTF-8 encoding.
     * Includes one byte for a null termination.
     *
     * @return The length of the string of the id when converted to UTF-8
     */
    public int utf8StringLength() {
        if (m_instanceIdStr.length() == 0) {
            return 0;
        }

        if (m_cachedUtf8String == null) {
            try {
                m_cachedUtf8String = m_instanceIdStr.getBytes("UTF-8");
            } catch (UnsupportedEncodingException e) {
                throw new SoftwareViolationException("Failed to convert string to UTF-8!!!");
            }
        }

        return m_cachedUtf8String.length + 1;
    }

    /**
     * Convert the string to UTF-8.
     *
     * Returns an empty string if there is no string.
     *
     * @return UTF-8 representation of the string.
     */
    public byte [] utf8String() {
        if (m_cachedUtf8String == null) {
            try {
                m_cachedUtf8String = m_instanceIdStr.getBytes("UTF-8");
            } catch (UnsupportedEncodingException e) {
                throw new SoftwareViolationException("Failed to convert string to UTF-8!!!");
            }
        }
        return m_cachedUtf8String;
    }
    
    /**
     * Compares two instances
     * @param other The object to compare to.
     * @return -1 if this instance is less than other, 1 if this is bigger, else 0.
     */
    @Override
    public int compareTo(InstanceId other) {
        if (getRawValue()<other.getRawValue())
            return -1;
        else if (getRawValue()>other.getRawValue())
            return 1;
        else
            return 0;
    }

    private long m_instanceId = -1;
    private String m_instanceIdStr;
    private byte [] m_cachedUtf8String;
}
