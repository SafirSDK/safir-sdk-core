/******************************************************************************
*
* Copyright Saab AB, 2008-2013 (http://safir.sourceforge.net)
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

#ifndef __DOTS_ENTITY_ID_H__
#define __DOTS_ENTITY_ID_H__

#include <Safir/Dob/Typesystem/Defs.h>
#include <Safir/Dob/Typesystem/InstanceId.h>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
    /**
     * Class containing the identity of an entity.
     * This class represents the identity of a DOB-entity. It consists of a type identifier (TypeId) and an instance number.
     */
    class EntityId
    {
    public:
        /**
         * Default constructor.
         */
        EntityId(): m_typeId(0),m_instanceId() {}

        /**
         * Constructor with type id and instance arguments.
         * Creates an EntityId with the given typeid and instance number.
         *
         * @param typeId [in] - The type id of the entity that the EntityId is to refer to.
         * @param instanceId [in] - The instance of the entity that the EntityId is to refer to.
        */
        EntityId(const Dob::Typesystem::TypeId typeId,
                 const Dob::Typesystem::InstanceId instanceId)
                 : m_typeId(typeId),m_instanceId(instanceId) {}


        /**
         * Remove the included string from the instance id of the entity id.
         *
         * This is meant to be used when this type is used as a member of a Dob object.
         * Using this call before the object gets serialized to binary or xml (i.e.
         * also before sending it anywhere) means that the string will not be included
         * when the object is sent.
         */
        void RemoveString() {m_instanceId.RemoveString();}


        /**
         * Get the Instance number out of the EntityId.
         *
         * @return The instance number.
         */
        const Dob::Typesystem::InstanceId & GetInstanceId() const {return m_instanceId;}

        /**
         * Set the instance number of the EntityId.
         *
         * @param instanceId [in] - The new instance number.
         */
        void SetInstanceId(const Dob::Typesystem::InstanceId & instanceId) {m_instanceId = instanceId;}

        /**
         * Get the type id out of the EntityId.
         *
         * @return The type id.
         */
        Dob::Typesystem::TypeId GetTypeId() const {return m_typeId;}

        /**
         * Set the type id of the EntityId.
         *
         * @param typeId [in] - The new type id.
         */
        void SetTypeId(const Dob::Typesystem::TypeId typeId) {m_typeId = typeId;}

        /**
         * Equality operator.
         * Compares both the instance and the type id.
         *
         * @param other [in] - The EntityId to compare with.
         */
        bool operator ==(const EntityId & other) const
        {
            return m_typeId == other.m_typeId && m_instanceId == other.m_instanceId;
        }

        /**
         * Inequality operator.
         * Compares both the instance and the type id.
         *
         * @param other [in] - The EntityId to compare with.
         */
        bool operator !=(const EntityId & other) const
        {
            return !(*this==other);
        }

        /**
         * Less-than operator.
         * This is provided to allow EntityIds to be stored in STL containers that need strict weak ordering.
         * Compares both the instance and the type id.
         *
         * @param other [in] - The EntityId to compare with.
         */
        bool operator < (const EntityId & other) const
        {
            if (m_typeId == other.m_typeId)
            {
                return m_instanceId < other.m_instanceId;
            }
            else
            {
                return m_typeId < other.m_typeId;
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
        DOTS_CPP_API const std::wstring ToString() const;

        /**
         * Convert an entity id to a string that has only numeric parts.
         *
         * Will convert the entity id to a string on the form "(10109232329848, 2884849309093)".
         * Use the normal ToString method if you need something for debug output. This is intended
         * to be used when a consistent string is needed.
         *
         * @return The entity id as a string.
         */
        DOTS_CPP_API const std::wstring ToStringNumeric() const;
    private:

        Dob::Typesystem::TypeId m_typeId;
        Dob::Typesystem::InstanceId m_instanceId;
    };

    static inline std::wostream & operator << (std::wostream & out, const EntityId & entityId)
    {return out << entityId.ToString();}
}
}
}
#endif

