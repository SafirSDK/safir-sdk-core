/* ****************************************************************************
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

namespace Safir.Dob.Typesystem
{
    /// <summary>
    /// Dictionary container.
    /// </summary>
    public class DictionaryContainer<KeyT, ValT> : ContainerBase, IDictionary<KeyT, ValT>
        where ValT : ContainerBase, new()
    {
        private SortedDictionary<KeyT, ValT> values = new SortedDictionary<KeyT, ValT> ();
        //private Dictionary<KeyT, ValT> values;

        /// <summary>
        /// Initializes a new instance of the class.
        /// </summary>
        public DictionaryContainer() : base()
        {
        }

        #region IDictionary implementation

        /// <summary>
        /// Add the specified key and value.
        /// </summary>
        /// <param name="key">Key.</param>
        /// <param name="value">Value.</param>
        public void Add (KeyT key, ValT value)
        {
            m_bIsChanged = true;
            value.SetChanged (true);
            values.Add (key, value);
        }

        /// <Docs>The item to add to the current collection.</Docs>
        /// <para>Adds an item to the current collection.</para>
        /// <remarks>To be added.</remarks>
        /// <exception cref="System.NotSupportedException">The current collection is read-only.</exception>
        /// <summary>
        /// Add the specified key.
        /// </summary>
        /// <param name="key">Key.</param>
        public ValT Add(KeyT key)
        {
            ValT value = new ValT ();
            Add (key, value);
            return value;
        }

        /// <Docs>The key to locate in the current instance.</Docs>
        /// <para>Determines whether the current instance contains an entry with the specified key.</para>
        /// <summary>
        /// Containses the key.
        /// </summary>
        /// <returns><c>true</c>, if key was containsed, <c>false</c> otherwise.</returns>
        /// <param name="key">Key.</param>
        public bool ContainsKey (KeyT key)
        {
            return values.ContainsKey (key);
        }

        /// <Docs>The item to remove from the current collection.</Docs>
        /// <para>Removes the first occurrence of an item from the current collection.</para>
        /// <summary>
        /// Remove the specified key.
        /// </summary>
        /// <param name="key">Key.</param>
        public bool Remove (KeyT key)
        {
            bool removed = values.Remove (key);
            if (removed)
                m_bIsChanged = true;

            return removed;
        }

        /// <Docs>To be added.</Docs>
        /// <summary>
        /// To be added.
        /// </summary>
        /// <remarks>To be added.</remarks>
        /// <returns><c>true</c>, if get value was tryed, <c>false</c> otherwise.</returns>
        /// <param name="key">Key.</param>
        /// <param name="value">Value.</param>
        public bool TryGetValue (KeyT key, out ValT value)
        {
            return values.TryGetValue (key, out value);
        }

        /// <summary>
        /// Set or get the value at the specified index.
        /// </summary>
        public ValT this [KeyT index] {
            get {
                return values [index];
            }
            set {
                m_bIsChanged = true;
                value.SetChanged (true);
                values [index] = value;
            }
        }

        /// <summary>
        /// Gets the keys.
        /// </summary>
        /// <value>The keys.</value>
        public ICollection<KeyT> Keys {
            get {
                return values.Keys;
            }
        }

        /// <summary>
        /// Gets the values.
        /// </summary>
        /// <value>The values.</value>
        public ICollection<ValT> Values {
            get {
                return values.Values;
            }
        }


        #endregion


        #region ICollection implementation

        /// <Docs>The item to add to the current collection.</Docs>
        /// <para>Adds an item to the current collection.</para>
        /// <remarks>To be added.</remarks>
        /// <exception cref="System.NotSupportedException">The current collection is read-only.</exception>
        /// <summary>
        /// Add the specified item.
        /// </summary>
        /// <param name="item">Item.</param>
        public void Add (KeyValuePair<KeyT, ValT> item)
        {
            m_bIsChanged = true;
            values.Add (item.Key, item.Value);
        }

        /// <summary>
        /// Clear this instance.
        /// </summary>
        public void Clear ()
        {
            m_bIsChanged = true;
            values.Clear ();
        }

        /// <Docs>The object to locate in the current collection.</Docs>
        /// <para>Determines whether the current collection contains a specific value.</para>
        /// <summary>
        /// Contains the specified item.
        /// </summary>
        /// <param name="item">Item.</param>
        public bool Contains (KeyValuePair<KeyT, ValT> item)
        {
            throw new NotImplementedException ();
        }

        /// <summary>
        /// Copies to.
        /// </summary>
        /// <param name="array">Array.</param>
        /// <param name="arrayIndex">Array index.</param>
        public void CopyTo (KeyValuePair<KeyT, ValT>[] array, int arrayIndex)
        {
            throw new NotImplementedException ();
        }

        /// <Docs>The item to remove from the current collection.</Docs>
        /// <para>Removes the first occurrence of an item from the current collection.</para>
        /// <summary>
        /// Remove the specified item.
        /// </summary>
        /// <param name="item">Item.</param>
        public bool Remove (KeyValuePair<KeyT, ValT> item)
        {
            m_bIsChanged = true;
            return Remove(item.Key);
        }

        /// <summary>
        /// Gets the count.
        /// </summary>
        /// <value>The count.</value>
        public int Count {
            get {
                return values.Count;
            }
        }

        /// <summary>
        /// Gets a value indicating whether this instance is read only.
        /// </summary>
        /// <value><c>true</c> if this instance is read only; otherwise, <c>false</c>.</value>
        public bool IsReadOnly {
            get {
                return false;
            }
        }


        #endregion


        #region IEnumerable implementation

        /// <summary>
        /// Gets the enumerator.
        /// </summary>
        /// <returns>The enumerator.</returns>
        public IEnumerator<KeyValuePair<KeyT, ValT>> GetEnumerator ()
        {
            return (values as IEnumerable<KeyValuePair<KeyT, ValT>>).GetEnumerator ();
        }


        #endregion


        #region IEnumerable implementation

        /// <summary>
        /// Gets the enumerator.
        /// </summary>
        /// <returns>The enumerator.</returns>
        System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator ()
        {
            return values.GetEnumerator ();
        }


        #endregion

        #region implemented abstract members of ContainerBase

        /// <summary>
        /// Is the container set to null?
        /// </summary>
        /// <returns>True if the container is set to null.</returns>
        public override bool IsNull ()
        {
            return false;
        }

        /// <summary>
        /// Set the container to null.
        /// </summary>
        public override void SetNull ()
        {
            throw new SoftwareViolationException("Dictionaries cannot be null!");
        }

        /// <summary>
        /// Determines whether this instance is changed.
        /// </summary>
        /// <returns><c>true</c> if this instance is changed; otherwise, <c>false</c>.</returns>
        public override bool IsChanged ()
        {
            if (m_bIsChanged)
                return m_bIsChanged;

            foreach (var kv in values)
            {
                if (kv.Value.IsChanged())
                {
                    return true;
                }
            }

            return false;
        }

        /// <summary>
        /// Sets the container changed.
        /// </summary>
        /// <param name="changed">If set to <c>true</c> changed.</param>
        public override void SetChanged (bool changed)
        {
            m_bIsChanged = changed;
            foreach (var kv in values)
            {
                kv.Value.SetChanged (changed);
            }
        }

        #endregion


    }

    /// <summary>
    /// Value dictionary container.
    /// </summary>
    public class ValueDictionaryContainer<KeyT, ContainerT, ValueT> : DictionaryContainer<KeyT, ContainerT>
        where ContainerT : ValueContainer<ValueT>, new()
    {
        /// <summary>
        /// Initializes a new instance of the class.
        /// </summary>
        public ValueDictionaryContainer() :base()
        {
        }

        /// <summary>
        /// Add the specified key and value.
        /// </summary>
        /// <param name="key">Key.</param>
        /// <param name="value">Value.</param>
        public void Add (KeyT key, ValueT value)
        {
            var container = new ContainerT ();
            container.Val = value;
            base.Add (key, container);
        }
    }

    /// <summary>
    /// Object dictionary container.
    /// </summary>
    public class ObjectDictionaryContainer<KeyT, ContainerT, ValueT> : DictionaryContainer<KeyT, ContainerT>
            where ValueT : Object
            where ContainerT : ObjectContainerImpl<ValueT>, new()
    {
        /// <summary>
        /// Initializes a new instance of the class.
        /// </summary>
        public ObjectDictionaryContainer() :base()
        {
        }

        /// <summary>
        /// Add the specified key and value.
        /// </summary>
        /// <param name="key">Key.</param>
        /// <param name="value">Value.</param>
        public void Add (KeyT key, ValueT value)
        {
            var container = new ContainerT ();
            container.Obj = value;
            base.Add (key, container);
        }
    }

    /// <summary>
    /// Enum dictionary container.
    /// </summary>
    public class EnumDictionaryContainer<KeyT, ContainerT, ValueT> : DictionaryContainer<KeyT, ContainerT>
        where ValueT : struct
        where ContainerT : EnumerationContainerImpl<ValueT>, new()
    {
        /// <summary>
        /// Initializes a new instance of the class.
        /// </summary>
        public EnumDictionaryContainer() :base()
        {
        }

        /// <summary>
        /// Add the specified key and value.
        /// </summary>
        /// <param name="key">Key.</param>
        /// <param name="value">Value.</param>
        public void Add (KeyT key, ValueT value)
        {
            var container = new ContainerT ();
            container.Val = value;
            base.Add (key, container);
        }
    }

    /// <summary>
    /// Enum dictionary container.
    /// </summary>
    public class StringDictionaryContainer<KeyT> : DictionaryContainer<KeyT, StringContainer>
    {
        /// <summary>
        /// Initializes a new instance of the class.
        /// </summary>
        public StringDictionaryContainer() :base()
        {
        }

        /// <summary>
        /// Add the specified key and value.
        /// </summary>
        /// <param name="key">Key.</param>
        /// <param name="value">Value.</param>
        public void Add (KeyT key, String value)
        {
            var container = new StringContainer ();
            container.Val = value;
            base.Add (key, container);
        }
    }

}
