// -*- coding: utf-8 -*-
/******************************************************************************
*
* Copyright Saab AB, 2008-2013 (http://safir.sourceforge.net)
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

/**
 * Class containing the identity of an entity.
 * This class represents the identity of a DOB-entity. It consists of a type identifier (TypeId) and an instance number.
 */
public class EntityId implements Comparable<EntityId> {

    /**
     * Default constructor.
     */
    public EntityId(){
        this.m_typeId = 0;
        this.m_instanceId = new InstanceId();
    }

    /**
     * Constructor with type id and instance arguments.
     * Creates an EntityId with the given typeid and instance number.
     *
     * @param typeId [in] - The type id of the entity that the EntityId is to refer to.
     * @param instanceId [in] - The instance of the entity that the EntityId is to refer to.
    */
    public EntityId(long typeId, InstanceId instanceId){
        this.m_typeId = typeId;
        this.m_instanceId = instanceId;
    }

    /**
     * Get the Instance number out of the EntityId.
     *
     * @return The instance number.
     */
    public InstanceId getInstanceId(){
        return m_instanceId;
    }

    /**
     * Set the instance number of the EntityId.
     *
     * @param instanceId [in] - The new instance number.
     */
    public void setInstanceId(InstanceId instanceId){
        m_instanceId = instanceId;
    }

    /**
     * Get the type id out of the EntityId.
     *
     * @return The type id.
     */
    public long getTypeId(){
        return m_typeId;
    }

    /**
     * Set the type id of the EntityId.
     *
     * @param typeId [in] - The new type id.
     */
    public void setTypeId(long typeId){
        m_typeId = typeId;
    }

    /**
     * Remove the included string from the instance id of the entity id.
     *
     * This is meant to be used when this type is used as a member of a Dob object.
     * Using this call before the object gets serialized to binary or xml (i.e.
     * also before sending it anywhere) means that the string will not be included
     * when the object is sent.
     */
    public void removeString(){
        m_instanceId.removeString();
    }

    /**
     * Equals.
     *
     * @param other The entity id to compare with.
     * @return True if the entity ids are equal.
     */
    public boolean equals(java.lang.Object other){
        if ((other == null) || !(other instanceof EntityId)){
            return false;
        }
        else {
            return m_typeId == ((EntityId)other).m_typeId && m_instanceId.equals(((EntityId)other).m_instanceId);
        }
    }

    /**
     * Convert an entity id to a string.
     *
     * Will convert the entity id to a string on the form "(Safir.Dob.Entity, 10)".
     * This is meant to be used for debug output only.
     * If the type does not exist output will be on the form "(Unknown type: 32873478348, 10)"
     * If the string representation of the instance exists, the numerical instance id may be
     * replaced by that string.
     *
     * The purpose of this function is for debug output and such.
     * The resulting string can *not* reliably be parsed or passed to constructors to recreate the same
     * entity id.
     *
     * @return The entity id as a string.
     */
    public String toString(){
        String str = "(";
        if (Operations.exists(m_typeId)){
            str += Operations.getName(m_typeId);
        }
        else{
            str += "Unknown type: "
                + m_typeId;
        }
        str += ", " + m_instanceId.toString() + ")";
        return str;
    }

    /**
     * Convert an entity id to a string that has only numeric parts.
     *
     * Will convert the entity id to a string on the form "(10109232329848, 2884849309093)".
     * Use the normal ToString method if you need something for debug output. This is intended
     * to be used when a consistent string is needed.
     *
     * @return The entity id as a string.
     */
    public String toStringNumeric(){
        String str = "(";
        str += m_typeId;
        str += ", " + m_instanceId.getRawValue() + ")";
        return str;
    }

    /**
     * Get hash code.
     *
     * @return hash code for entity id.
     */
    public int hashCode(){
        return (int)m_typeId ^ m_instanceId.hashCode();
    }
    
    /**
     * Compares two instances
     * @param other The object to compare to.
     * @return -1 if this instance is less than other, 1 if this is bigger, else 0.
     */
    @Override
    public int compareTo(EntityId other) {
        if (m_typeId<other.m_typeId)
            return -1;
        else if (m_typeId>other.m_typeId)
            return 1;
        else
            return m_instanceId.compareTo(other.m_instanceId);
    }

    private long m_typeId = 0;
    private InstanceId m_instanceId;
}
