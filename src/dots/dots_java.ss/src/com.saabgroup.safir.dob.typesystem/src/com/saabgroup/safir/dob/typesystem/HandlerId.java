// -*- coding: utf-8 -*-
/******************************************************************************
*
* Copyright Saab AB, 2008 (http://www.safirsdk.com)
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
 * Class containing the identity of a handler.
 */
public class HandlerId {

    /** Constant representing all handlers */
    public static final HandlerId ALL_HANDLERS = new HandlerId("ALL_HANDLERS");

    /**
     * Default constructor.
     *
     * Creates a default handler id.
     */
    public HandlerId(){
        this.m_handlerId = Kernel.Generate64("DEFAULT_HANDLER");
        this.m_handlerIdStr = "DEFAULT_HANDLER";
    }

    /**
     * Constructor.
     *
     * Creates a handler id from the given string.
     *
     * @param id String identifying the handler.
     */
    public HandlerId(String id){
        if(id == null || id.length() == 0){
            throw new SoftwareViolationException("HandlerId can not be generated from null/empty string");
        }
        this.m_handlerId = Kernel.Generate64(id);
        this.m_handlerIdStr = id;
    }

    /**
     * Constructor.
     *
     * Creates a handler id from the given id.
     *
     * @param id Identifier identifying the handler.
     */
    public HandlerId(long id){
        this.m_handlerId = id;
        this.m_handlerIdStr = "";
    }

    /**
     * Constructor.
     *
     * Creates a handler id from the given data.
     *
     * @param id Identifier identifying the handler.
     * @param idStr String identifying the handler.
     */
    public HandlerId(long id, String idStr){
        this.m_handlerId = id;
        this.m_handlerIdStr = idStr;
        if (idStr == null){
            throw new SoftwareViolationException("String argument to HandlerId constructor cannot be null");
        }
    }

    /**
     * Remove the included string from the handler id.
     *
     * This is meant to be used when this type is used as a member of a Dob object.
     * Using this call before the object gets serialized to binary or xml (i.e.
     * also before sending it anywhere) means that the string will not be included
     * when the object is sent.
     */
    public void removeString(){
        this.m_handlerIdStr = "";
    }

    /**
     * Equals.
     *
     * @param other The handler id to compare with.
     * @return True if the handler ids are equal.
     */
    public boolean equals(java.lang.Object other){
        if ((other == null) || !(other instanceof HandlerId)){
            return false;
        }
        else {
            return m_handlerId == ((HandlerId)other).m_handlerId;
        }
    }

    /**
     * Return a string representation of the handler id.
     *
     * @return String representation of the handler id.
     */
    public String toString(){
        if (m_handlerIdStr.length() > 0){
            return m_handlerIdStr;
        }
        else if (m_handlerId == DEFAULT_HANDLER.getRawValue()){
            return DEFAULT_HANDLER.getRawString();
        }
        else if (m_handlerId == ALL_HANDLERS.getRawValue()){
            return ALL_HANDLERS.getRawString();
        }
        else{
            return java.lang.Long.toString(m_handlerId);
        }
    }

    /**
     * Overridden base class method.
     *
     * @return Hash code.
     */
    public int hashCode(){
        return (int)m_handlerId;
    }

    /**
     * Get the raw 64 bit integer identifier.
     *
     * @return The raw 64 bit identifier.
     */
    public long getRawValue(){
        return m_handlerId;
    }

    /**
     * Get the string that was used to create this id.
     *
     * If no string was used this method returns an empty string.
     *
     * @return The string (if any) that was used to create this id.
     */
    public String getRawString(){
        return m_handlerIdStr;
    }

    /**
     * Get the length of the string when converted to UTF-8 encoding.
     * Includes one byte for a null termination.
     *
     * @return The length of the string of the id when converted to UTF-8
     */
    public int utf8StringLength() {
        if (m_handlerIdStr.length() == 0) {
            return 0;
        }

        if (m_cachedUtf8String == null) {
            try {
                m_cachedUtf8String = m_handlerIdStr.getBytes("UTF-8");
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
                m_cachedUtf8String = m_handlerIdStr.getBytes("UTF-8");
            } catch (UnsupportedEncodingException e) {
                throw new SoftwareViolationException("Failed to convert string to UTF-8!!!");
            }
        }
        return m_cachedUtf8String;
    }

    private long m_handlerId = -1;
    private String m_handlerIdStr;
    private byte [] m_cachedUtf8String;
    private static final HandlerId DEFAULT_HANDLER = new HandlerId();

}
