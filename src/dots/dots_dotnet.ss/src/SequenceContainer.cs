/******************************************************************************
*
* Copyright Saab AB, 2005-2015 (http://safir.sourceforge.net)
* 
* Created by: Joel Ottosson / joot
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
    /// Container for base types.
    /// 
    /// <para/>
    /// This class holds a value of the template argument type and a null flag.
    /// The operations that modify the value update the null flag and the change flag
    /// (which is inherited from ContainerBase).
    /// 
    /// <para/>
    /// This container is intended for the simple types of the DOB typesystem.
    /// </summary>
    /// <typeparam name="T">The type to contain.</typeparam>
    public class SequenceContainer<T> : ContainerBase, ICloneable
    {
        private List<T> values;
        /// <summary>
        /// Default constructor
        /// <para/>
        /// Creates a null and not changed container.
        /// </summary>
        public SequenceContainer(): base()
        {           
        }

        #region Cloning

        object ICloneable.Clone()
        {
            return new SequenceContainer<T>(this);
        }

        /// <summary>
        /// 
        /// </summary>
        /// <returns></returns>
        public new SequenceContainer<T> Clone()
        {
            return (SequenceContainer<T>)((ICloneable)this).Clone(); 
        }

        /// <summary>
        /// Copy constructor for use by Clone
        /// </summary>
        /// <param name="other"></param>
        protected SequenceContainer(SequenceContainer<T> other):
            base(other)
        {
            values = other.values;
        }

        /// <summary>
        /// Override ContainerBase.
        /// </summary>
        /// <param name="other"></param>
        public override void Copy(ContainerBase other)
        {
            base.Copy(other);
            SequenceContainer<T> that = other as SequenceContainer<T>;          
            values = that.values;
        }

        #endregion



        /// <summary>
        /// Override ContainerBase.
        /// </summary>
        public override bool IsNull()
        {
            return false;
        }

        /// <summary>
        /// Override ContainerBase.
        /// </summary>
        public override void SetNull()
        {
            throw new SoftwareViolationException("Sequences cannot be null!");
        }

        /// <summary>
        /// Not implementd!
        /// </summary>
        /// <returns></returns>
        public override int GetHashCode()
        {
            throw new System.Exception("The method or operation is not implemented.");
        }

        /// <summary>
        /// Not implementd!
        /// </summary>
        /// <param name="obj"></param>
        /// <returns></returns>
        public override bool Equals(object obj)
        {
            throw new System.Exception("The method or operation is not implemented.");
        }
    }


}
