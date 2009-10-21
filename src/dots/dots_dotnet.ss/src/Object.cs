/******************************************************************************
*
* Copyright Saab AB, 2005-2008 (http://www.safirsdk.com)
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

namespace Safir.Dob.Typesystem
{
    /// <summary>
    /// The base class of all Dob-classes.
    /// <para/>
    /// This class is the base class for all automatically generated DOB classes.
    /// </summary>
    public class Object : ICloneable
    {
        #region Constructors

        /// <summary>
        /// Default constructor.
        /// </summary>
        public Object()
        {
           
        }

        #endregion

        #region TypeId

        /// <summary>
        /// The TypeId of the Object class.
        /// </summary>
        public const System.Int64 ClassTypeId = 5955188366590963785;

        /// <summary>
        /// Get the type id of this object.
        /// <para>
        /// This method is overridden by all auto-generated classes.
        /// </para>
        /// </summary>
        /// <returns>The TypeId of the object.</returns>
        virtual public System.Int64 GetTypeId() //MUST BE OVERRIDDEN!
        {
            return ClassTypeId;
        }

        #endregion

        #region Change flags

        /// <summary>
        /// Check if any member of this object is changed.
        /// <para>
        /// This method will recursively check if any member of the object has its change flag set.
        /// </para>
        /// <para>
        /// This method is overridden by all auto-generated classes.
        /// </para>
        /// </summary>
        /// <returns>True if any member has changed.</returns>
        virtual public bool IsChanged() { return false; }

        /// <summary>
        /// Recursively set change flags in all members of this object.
        /// <para>
        /// This method is overridden by all auto-generated classes.
        /// </para>
        /// </summary>
        /// <param name="changed">The value to set the change flags to.</param>
        virtual public void SetChanged(bool changed) { }

        #endregion

        #region Cloning

        object ICloneable.Clone()
        {
            return new Object(this);
        }

        /// <summary>
        /// Create a copy of the object.
        /// <para>
        /// Cast it to the type of object that you're after.
        /// </para>
        /// </summary>
        /// <returns></returns>
        public virtual Object Clone()
        {
            return (Object)((ICloneable)this).Clone();
        }

        /// <summary>
        /// Copy constructor for use by Clone.
        /// </summary>
        /// <param name="other">Other object.</param>
        protected Object(Object other)
        {

        }

        #endregion

        #region Blob serialization and deserialization

        /// <summary>
        /// Create an Object from a blob.
        /// </summary>
        /// <param name="blob">The blob to deserialize.</param>
        public Object(System.IntPtr blob)
        {

        }

        /// <summary>
        /// Calculate the size of the blob-serialized form of this object.
        /// </summary>
        /// <returns>The needed size in bytes.</returns>
        virtual public System.Int32 CalculateBlobSize()
        {
            if (m_InitialSize == -1)
            {
                m_InitialSize = BlobOperations.GetInitialSize(ClassTypeId);
            }
            return m_InitialSize;
        }

        /// <summary>
        /// Write the object to a blob.
        /// <para>
        /// Note that the size of the blob is assumed to be correct! No checks are made!
        /// </para>
        /// </summary>
        /// <param name="blob">The blob to write to.</param>
        /// <param name="beginningOfUnused">The beginning of unused dynamic blob space.</param>
        virtual public void WriteToBlob(System.IntPtr blob, ref System.IntPtr beginningOfUnused)
        {

        }
        #endregion

        #region Reflection part

        /// <summary>
        /// Get a reference to a member container from an object.
        /// <para>
        /// Use the methods in Members to get member indices and array sizes for use
        /// with this method.
        /// </para>
        /// <para>
        /// Note: Do not use this method unless you're very sure it is the one you need!
        /// </para>
        /// </summary>
        /// <param name="member">The index of the member to get.</param>
        /// <param name="index">The array index of the member to get.</param>
        /// <returns>A reference to the member container.</returns>
        /// <exception cref="Safir.Dob.Typesystem.IllegalValueException">If the index is not in the range of the array.</exception>
        /// <exception cref="Safir.Dob.Typesystem.SoftwareViolationException">If the element is not an array and the index is not 0.</exception>
        public virtual ContainerBase GetMember(System.Int32 member,
                                               System.Int32 index)
        {
            throw new SoftwareViolationException("Object does not have any members!");
        }

        #endregion

        #region Private Data

        private static System.Int32 m_InitialSize = -1;

        #endregion

    }
    
}
