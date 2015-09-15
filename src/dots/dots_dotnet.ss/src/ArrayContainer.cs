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

namespace Safir.Dob.Typesystem
{
    /// <summary>
    /// Array of DOB-containers.
    ///
    /// <para/>
    /// This generic class is used for arrays of containers in objects.
    /// The arrays cannot change size once they have been created.
    /// Apart from that they behave like a normal C# collection.
    ///
    /// </summary>
    public class ArrayContainer<T> : IList<T> where T : ContainerBase, new()
    {
        #region Constructors
        /// <summary>
        /// Constructor with size.
        ///
        /// Creates an array of the given size. Remember that once it has been created the size cannot be changed.
        /// </summary>
        /// <param name="size">The desired size of the array. Must be > 0.</param>
        public ArrayContainer(System.Int32 size)
        {
            m_Array = new List<T>(size);
            for (System.Int32 i = 0; i < size; ++i)
            {
                m_Array.Add(new T());
            }
        }

        #endregion

        #region Change flags>
        /// <summary>
        /// Check if any element has a change flag set on it.
        ///
        /// <para/>
        /// Note that if this array contains objects this call will be recursive.
        /// </summary>
        /// <returns>true if any element has changed.</returns>
        virtual public bool IsChanged()
        {
            foreach (ContainerBase cont in m_Array)
            {
                if (cont.IsChanged())
                {
                    return true;
                }
            }
            return false;
        }

        /// <summary>
        /// Set the change flag on all elements in the array.
        ///
        /// <para/>
        /// Note that if this array contains objects this call will be recursive.
        /// </summary>
        /// <param name="changed">The value to set the change flags to</param>
        virtual public void SetChanged(bool changed)
        {
            for (System.Int32 i = 0; i < m_Array.Count; ++i)
            {
                m_Array[i].SetChanged(changed);
            }
        }

        #endregion

        #region IList<T> Members

        /// <summary>
        /// </summary>
        /// <param name="item"></param>
        /// <returns></returns>
        public int IndexOf(T item)
        {
            return m_Array.IndexOf(item);
        }

        void IList<T>.Insert(int index, T item)
        {
            throw new SoftwareViolationException("Array containers cannot change size!");
        }

        void IList<T>.RemoveAt(int index)
        {
            throw new SoftwareViolationException("Array containers cannot change size!");
        }

        /// <summary>
        ///
        /// </summary>
        /// <param name="index"></param>
        /// <returns></returns>
        public T this[int index]
        {
            get
            {
                return m_Array[index];
            }
            set
            {
                m_Array[index] = value;
            }
        }

        #endregion

        #region ICollection<T> Members

        void ICollection<T>.Add(T item)
        {
            throw new SoftwareViolationException("Array containers cannot change size!");
        }

        void ICollection<T>.Clear()
        {
            throw new SoftwareViolationException("Array containers cannot change size!");
        }

        /// <summary>
        ///
        /// </summary>
        /// <param name="item"></param>
        /// <returns></returns>
        public bool Contains(T item)
        {
            return m_Array.Contains(item);
        }

        /// <summary>
        ///
        /// </summary>
        /// <param name="array"></param>
        /// <param name="arrayIndex"></param>
        public void CopyTo(T[] array, int arrayIndex)
        {
            if (array.Length != m_Array.Count)
            {
                throw new SoftwareViolationException("Array sizes must be the same!");
            }
            m_Array.CopyTo(array, arrayIndex);
        }

        /// <summary>
        ///
        /// </summary>
        public int Count
        {
            get { return m_Array.Count; }
        }

        /// <summary>
        ///
        /// </summary>
        public bool IsReadOnly
        {
            get { return false; }
        }

        /// <summary>
        ///
        /// </summary>
        /// <param name="item"></param>
        /// <returns></returns>
        public bool Remove(T item)
        {
            throw new SoftwareViolationException("Array containers cannot change size!");
        }

        #endregion

        #region IEnumerable<T> Members

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        public IEnumerator<T> GetEnumerator()
        {
            return m_Array.GetEnumerator();
        }

        #endregion

        #region IEnumerable Members

        System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
        {
            return m_Array.GetEnumerator();
        }

        #endregion

        #region Cloning

        /// <summary>
        /// Create a copy of the ArrayContainer
        /// <para>
        /// This method is deprecated.
        /// </para>
        /// </summary>
        public dynamic Clone()
        {
            return this.DeepClone();
        }

        #endregion

        #region Private data

        private System.Collections.Generic.List<T> m_Array;

        #endregion

    }


}
