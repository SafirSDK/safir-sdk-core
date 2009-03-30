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
using System.Collections.Generic;
using System.Text;

namespace Safir.Dob.Typesystem
{
    abstract public class EnumerationContainerBase : ContainerBase, ICloneable
    {
        public EnumerationContainerBase(): base()
        {
            m_bIsNull = true;
        }

        #region Cloning

        object ICloneable.Clone()
        {
            throw new SoftwareViolationException("Cannot clone EnumerationContainerBase");
        }

        public new EnumerationContainerBase Clone()
        {
            return (EnumerationContainerBase)((ICloneable)this).Clone(); 
        }

        //Copy constructor for use by Clone
        protected EnumerationContainerBase(EnumerationContainerBase other)
            : base(other)
        {
            m_bIsNull = other.m_bIsNull;
            m_Value = other.m_Value;
        }

        public override void Copy(ContainerBase other)
        {
            base.Copy(other);
            EnumerationContainerBase that = other as EnumerationContainerBase;
            m_bIsNull = that.m_bIsNull;
            m_Value = that.m_Value;
        }

        #endregion

        abstract public int Ordinal { get;  set; }

        public override bool IsNull() 
        {
            return m_bIsNull;
        }

        public override void SetNull()
        {
            m_bIsNull = true;
            m_bIsChanged = true;
        }

       

        protected internal int m_Value;
        protected internal bool m_bIsNull;
    }

    /*
    public class EnumerationContainer <T> : EnumerationContainerBase where T : struct
    {

        public override int Ordinal
        {
            get
            {
                if (IsNull())
                {
                    throw new NullException("Value is null");
                }
                return m_Value;
            }
            set
            {
                if (!System.Enum.IsDefined(typeof(T),value))  //TODO can this be done in a better way?
                {
                    throw new IllegalValueException("Value is not in the enumeration range");
                }
                m_bIsNull = false;
                m_bIsChanged = true;
                m_Value = value;
            }
        }

        public T Val
        {
            get
            {
                return (T)Ordinal;
            }

            set
            {
                Ordinal = (int)value;
            }
        }
    }*/
}
