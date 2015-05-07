/* ****************************************************************************
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

using System;
using System.Collections.Generic;
using System.Text;

namespace Safir.Dob.Typesystem
{
    /// <summary>
    ///  Base class for containers of enumeration values.
    ///  <para>
    ///  The containers for enumerations are defined in the automatically generated code,
    ///  but this class defines the common functionality for them.
    ///  Enumeration containers really store the ordinal values (integer representation of
    ///  the enumeration), and this class has methods for setting and getting the ordinal.
    ///  The derived class (in the generated code) has methods for setting and getting the
    ///  value as an enumeration value.
    ///  Most applications should not use the Ordinal property, but should
    ///  use the Val property defined in the derived classes.
    ///  </para>
    /// </summary>
    abstract public class EnumerationContainerBase : ContainerBase
    {
        /// <summary>
        /// Default constructor.
        /// <para>
        /// Constructs an enumeration container that is null and not changed.
        /// </para>
        /// </summary>
        public EnumerationContainerBase(): base()
        {
            m_bIsNull = true;
        }

        /// <summary>
        /// Ordinal value of the enumeration container.
        /// </summary>
        abstract public int Ordinal { get;  set; }

        /// <summary>
        /// Overrides method in ContainerBase.
        /// </summary>
        /// <returns>True if null.</returns>
        public override bool IsNull()
        {
            return m_bIsNull;
        }

        /// <summary>
        /// Overrides method in ContainerBase.
        /// </summary>
        public override void SetNull()
        {
            m_bIsNull = true;
            m_bIsChanged = true;
        }

        internal override void ShallowCopy(ContainerBase other)
        {
            base.ShallowCopy(other);
            EnumerationContainerBase that = (EnumerationContainerBase)other;
            m_Value = that.m_Value;
            m_bIsNull = that.m_bIsNull;
        }

        /// <summary>The stored value</summary>
        protected internal int m_Value;

        /// <summary>The null flag.</summary>
        protected internal bool m_bIsNull;
    }

    /// <summary>
    /// Abstract class to allow dictionary containers to manipulate enum values.
    /// </summary>
    abstract public class EnumerationContainerImpl<EnumT>: EnumerationContainerBase
        where EnumT : struct
    {
        /// <summary>
        /// Constructor.
        /// </summary>
        public EnumerationContainerImpl() : base()
        {
        }

        /// <summary>
        /// Abstract property.
        /// </summary>
        abstract public EnumT Val {
            get;
            set;
        }
    }
}
