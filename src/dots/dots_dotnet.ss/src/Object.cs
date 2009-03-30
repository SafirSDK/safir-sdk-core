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
    /// Summary description for Object.
    /// </summary>
    public class Object : ICloneable
    {
        #region Constructors

        public Object()
        {
           
        }

        #endregion

        #region TypeId

        public const System.Int64 ClassTypeId = 5955188366590963785;

        virtual public System.Int64 GetTypeId() //MUST BE OVERRIDDEN!
        {
            return ClassTypeId;
        }

        #endregion

        #region Change flags

        virtual public bool IsChanged() { return false; }

        virtual public void SetChanged(bool changed) { }

        #endregion

        #region Cloning

        object ICloneable.Clone()
        {
            return new Object(this);
        }

        public virtual Object Clone()
        {
            return (Object)((ICloneable)this).Clone();
        }

        //Copy constructor for use by Clone
        protected Object(Object other)
        {

        }

        #endregion

        #region Blob serialization and deserialization
        public Object(System.IntPtr blob)
        {

        }

        virtual public System.Int32 CalculateBlobSize()
        {
            if (m_InitialSize == -1)
            {
                m_InitialSize = BlobOperations.GetInitialSize(ClassTypeId);
            }
            return m_InitialSize;
        }

        virtual public void WriteToBlob(System.IntPtr blob, ref System.IntPtr beginningOfUnused)
        {

        }
        #endregion

        #region Reflection part

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
