/* ****************************************************************************
*
* Copyright Saab AB, 2005-2013 (http://safirsdkcore.com)
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
    /// Base class for all Containers.
    /// <para/>
    ///This class contains common functionality for all Containers.
    /// Basically this amounts to the interface for nullability and
    /// the change flag.
    /// </summary>
    abstract public class ContainerBase
    {
        /// <summary>
        /// Default Constructor.
        /// <para/>
        /// Construct a container that is not changed.
        /// </summary>
        public ContainerBase()
        {
            m_bIsChanged = false;
        }

        /// <summary>
        /// Is the container set to null?
        /// </summary>
        /// <returns>True if the container is set to null.</returns>
        abstract public bool IsNull();

        /// <summary>
        /// Set the container to null.
        /// </summary>
        abstract public void SetNull();

        /// <summary>
        /// Is the change flag set on the container?
        /// <para/>
        /// The change flag gets updated every time the contained value changes.
        /// <para/>
        /// Note: If this is a container containing objects this call will recursively
        /// check change flags in the contained objects.
        /// </summary>
        /// <returns> True if the containers change flag is set.</returns>
        virtual public bool IsChanged()
        {
            return m_bIsChanged;
        }

        /// <summary>
        /// Set the containers change flag.
        /// <para/>
        /// It should be fairly unusual for an application to have to use this
        /// operation. There is nothing dangerous about it, but are you sure this
        /// is the operation you were after?
        /// <para/>
        /// The change flag is how receivers of objects can work out what the
        /// sender really wanted done on the object.
        /// <para/>
        /// Note: If this is a container containing one or more objects this call
        /// will recursively set all the change flags in the contained objects.
        /// </summary>
        /// <param name="changed">The value to set the change flag(s) to.</param>
        virtual public void SetChanged(bool changed)
        {
            m_bIsChanged = changed;
        }
        #region Cloning

        /// <summary>
        /// Create a copy of the Container.
        /// <para>
        /// This method is deprecated.
        /// </para>
        /// </summary>
        public dynamic Clone()
        {
            return this.DeepClone();
        }

        /// <summary>
        /// Copy.
        /// </summary>
        /// <param name="other">Other ContainerBase.</param>
        virtual public void Copy(ContainerBase other)
        {
            ShallowCopy(other);
        }

        #endregion


        /// <summary>
        /// Flag accessible from subclasses.
        /// </summary>
        protected internal bool m_bIsChanged;

        virtual internal void ShallowCopy(ContainerBase other)
        {
            if (this.GetType() != other.GetType())
            {
                throw new SoftwareViolationException("Invalid call to Copy, containers are not of same type");
            }
            m_bIsChanged = other.m_bIsChanged;
        }

    }
}
