/* ****************************************************************************
*
* Copyright Saab AB, 2008-2015 (http://safirsdkcore.com)
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

using System;
using System.Collections.Generic;
using System.Text;

namespace Safir.Dob.Typesystem
{
    /// <summary>
    /// Class containing the identity of an entity.
    /// <para/>
    /// This class represents the identity of a DOB-entity. It consists of a type identifier (TypeId) and an instance number.
    /// </summary>
    public class EntityId : IComparable
    {
        /// <summary>
        /// Default constructor.
        /// </summary>
        public EntityId()
        {
            m_typeId = 0;
            m_instanceId = new InstanceId();
        }

        /// <summary>
        /// Constructor with type id and instance arguments.
        /// <para/>
        /// Creates an EntityId with the given typeid and instance number.
        /// </summary>
        /// <param name="type">The type id of the entity that the EntityId is to refer to.</param>
        /// <param name="instanceId">The instance of the entity that the EntityId is to refer to.</param>
        public EntityId (System.Int64 type, InstanceId instanceId)
        {
            if (instanceId == null)
            {
                throw new SoftwareViolationException("instance cannot be null in EntityId constructor");
            }
            m_typeId = type;
            m_instanceId = instanceId;
        }

        /// <summary>
        /// Remove the included string from the instance id of the entity id.
        ///
        /// <para/>
        /// This is meant to be used when this type is used as a member of a Dob object.
        /// Using this call before the object gets serialized to binary or xml (i.e.
        /// also before sending it anywhere) means that the string will not be included
        /// when the object is sent.
        /// </summary>
        public void RemoveString()
        {
            m_instanceId.RemoveString();
        }

        /// <summary>
        /// Property for type id part of the EntityId.
        /// </summary>
        public Int64 TypeId
        {
            get { return m_typeId; }
            set { m_typeId = value; }
        }

        /// <summary>
        /// Property for instance id part of the EntityId.
        /// </summary>
        public InstanceId InstanceId
        {
            get { return m_instanceId; }
            set
            {
                if (value == null)
                {
                    throw new SoftwareViolationException("EntityId.InstanceId cannot be set to null!");
                }
                m_instanceId = value;
            }
        }

        /// <summary>
        /// Convert an entity id to a string.
        /// <para/>
        /// Will convert the entity id to a string on the form "(Safir.Dob.Entity, 10)".
        /// This is meant to be used for debug output only.
        /// If the type does not exist output will be on the form "(Unknown type: 32873478348, 10)"
        /// If the string representation of the instance exists, the numerical instance id may be
        /// replaced by that string.
        /// <para/>
        /// The purpose of this function is for debug output and such.
        /// The resulting string can *not* reliably be parsed or passed to constructors to recreate the same
        /// entity id.
        /// </summary>
        /// <returns>The entity id as a string.</returns>
        public override string ToString()
        {
            string str = "(";

            if (Operations.Exists(m_typeId))
            {
                str += Operations.GetName(m_typeId);
            }
            else
            {
                str += "Unknown type: "
                    + m_typeId;
            }
            str += ", " + m_instanceId.ToString() + ")";
            return str;
        }

        /// <summary>
        /// Convert an entity id to a string that has only numeric parts.
        /// <para/>
        /// Will convert the entity id to a string on the form "(10109232329848, 2884849309093)".
        /// Use the normal ToString method if you need something for debug output. This is intended
        /// to be used when a consistent string is needed.
        /// </summary>
        /// <returns>The entity id as a string.</returns>
        public string ToStringNumeric()
        {
            string str = "(";
            str += m_typeId;
            str += ", " + m_instanceId.RawValue + ")";
            return str;
        }

        /// <summary>
        /// Equals.
        /// <para/>
        /// Compares both the instance and the type id.
        /// </summary>
        /// <param name="obj">The EntityId to compare with.</param>
        /// <returns></returns>
        public override bool Equals(object obj)
        {
            EntityId other = obj as EntityId;
            if (other == null)
            {
                return false;
            }
            else
            {
                return m_typeId == other.m_typeId && m_instanceId.Equals(other.m_instanceId);
            }
        }

        /// <summary>
        /// Static equality operator.
        /// </summary>
        /// <param name="first">First Entity Id</param>
        /// <param name="second">Second Entity Id</param>
        /// <returns>True if both the instance and the type id match</returns>
        public static bool operator ==(EntityId first, object second)
        {
            // If both are null, or both are same instance, return true.
            if (System.Object.ReferenceEquals(first, second))
            {
                return true;
            }

            // If one is null, but not both, return false.
            if (((object)first == null) || ((object)second == null))
            {
                return false;
            }

            // Return true if the fields match:
            return first.Equals(second);
        }

        /// <summary>
        /// Static inequality operator.
        /// </summary>
        /// <param name="first">First Entity Id</param>
        /// <param name="second">Second Entity Id</param>
        /// <returns>True if the instance or the type id doesn't match</returns>
        public static bool operator !=(EntityId first, object second)
        {
            return !(first == second);
        }

        /// <summary>
        /// GetHashCode.
        /// </summary>
        /// <returns>Hash code</returns>
        public override int GetHashCode()
        {
            return m_instanceId.GetHashCode() ^ m_typeId.GetHashCode();
        }

        #region IComparable implementation

        /// <summary>
        /// Compare entity ids.
        /// </summary>
        public int CompareTo (object obj)
        {
            EntityId other = obj as EntityId;
            if (other == null)
                throw new ArgumentException ("Can't compare EntityId to object of another type");

            if (m_typeId < other.m_typeId) {
                return -1;
            }
            else if (m_typeId > other.m_typeId) {
                return 1;
            }
            else {
                return m_instanceId.CompareTo (other.m_instanceId);
            }
        }

        #endregion

        private System.Int64 m_typeId;
        private InstanceId m_instanceId;
    }
}
