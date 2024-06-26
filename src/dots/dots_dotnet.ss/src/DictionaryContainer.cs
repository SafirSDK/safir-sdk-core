/* ****************************************************************************
*
* Copyright Saab AB, 2005-2015 (http://safirsdkcore.com)
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
using System.Linq;
using System.Collections.Generic;

namespace Safir.Dob.Typesystem
{
    /// <summary>
    /// Dictionary container base.
    /// </summary>
    abstract public class DictionaryContainerBase: ContainerBase
    {
        /// <summary>
        /// Is the change flag in the container set?
        /// <para/>
        /// This method is like IsChanged without the recursion.
        /// </summary>
        /// <returns>True if the containers change flag is set.</returns>
        public bool IsChangedHere ()
        {
            return m_bIsChanged;
        }

        /// <summary>
        /// Set the change flag in the container.
        /// <para/>
        /// This method is like SetChanged without the recursion.
        /// </summary>
        /// <param name="changed">The value to set the change flag to.</param>
        public void SetChangedHere (bool changed)
        {
            m_bIsChanged = changed;
        }

        /// <summary>
        /// Gets the count.
        /// </summary>
        /// <value>The count.</value>
        public abstract int Count { get; }

        /// <summary>
        /// Adds a new key to the dictionary and return the value container.
        /// <para/>
        /// If the type of the key doesn't match the dictionaries key type an
        /// SoftwareViolation is thrown.
        /// <para/>
        /// </summary>
        /// <param name="key">The key to add.</param>
        /// <returns>The value container</returns>
        public abstract ContainerBase AddNull(object key);

        /// <summary>
        /// Get the key at a particular position in the dictionary.
        /// <para/>
        /// Note that the order of keys in the dictionary is not guaranteed.
        /// This is not a particularly "cheap" way of accessing the contents of a
        /// dictionary. Much better to use the iterators in the implementing class.
        /// <para/>
        /// For enumeration values, the returned value is an int ordinal.
        /// </summary>
        /// <param name="index"> an index between 0 and Count. </param>
        /// <returns>The key at a position in the dictionary.</returns>
        public abstract object GetKeyAt(int index);

        /// <summary>
        /// Get the container of the value at a particular position in the dictionary.
        /// <para/>
        /// Note that the order of keys in the dictionary is not guaranteed.
        /// This is not a particularly "cheap" way of accessing the contents of a
        /// dictionary. Much better to use normal iteration.
        /// </summary>
        /// <param name="index">An index between 0 and Count.</param>
        /// <returns>The container at a position in the dictionary.</returns>
        public abstract ContainerBase GetValueContainerAt(int index);
        
        /// <summary>
        /// Function needed by Utilities::MergeChanges to be able to merge
        /// dictionaries. Will in turn call Utilities::MergeChanges recursively if it
        /// needs to merge objects.
        /// </summary>
        internal abstract void Merge(DictionaryContainerBase other);
    };

    /// <summary>
    /// Dictionary container.
    /// </summary>
    public class DictionaryContainer<KeyT, ValT> : DictionaryContainerBase, IDictionary<KeyT, ValT>
        where ValT : ContainerBase, new()
    {
        /// <summary>
        /// Initializes a new instance of the class.
        /// </summary>
        public DictionaryContainer() : base()
        {
            //For string keys we need to specify the comparer, otherwise we get different order from C++.
            if (typeof(KeyT) == typeof(String))
            {
                m_values = new SortedDictionary<KeyT, ValT> ((System.Collections.Generic.IComparer<KeyT>)StringComparer.Ordinal);
            }
            else
            {
                m_values = new SortedDictionary<KeyT, ValT> ();
            }
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
            m_values.Add (key, value);
        }

        /// <Docs>The item to add to the current collection.</Docs>
        /// <para>Adds an item to the current collection.</para>
        /// <remarks>To be added.</remarks>
        /// <exception cref="System.NotSupportedException">The current collection is read-only.</exception>
        /// <summary>
        /// Add the specified key.
        /// </summary>
        /// <returns>The value container.</returns>
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
            return m_values.ContainsKey (key);
        }

        /// <Docs>The item to remove from the current collection.</Docs>
        /// <para>Removes the first occurrence of an item from the current collection.</para>
        /// <summary>
        /// Remove the specified key.
        /// </summary>
        /// <param name="key">Key.</param>
        public bool Remove (KeyT key)
        {
            bool removed = m_values.Remove (key);
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
            return m_values.TryGetValue (key, out value);
        }

        /// <summary>
        /// Set or get the value at the specified index.
        /// </summary>
        public ValT this [KeyT index] {
            get {
                return m_values [index];
            }
            set {
                m_bIsChanged = true;
                m_values [index] = value;
            }
        }

        /// <summary>
        /// Gets the keys.
        /// </summary>
        /// <value>The keys.</value>
        public ICollection<KeyT> Keys {
            get {
                return m_values.Keys;
            }
        }

        /// <summary>
        /// Gets the values.
        /// </summary>
        /// <value>The values.</value>
        public ICollection<ValT> Values {
            get {
                return m_values.Values;
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
            Add(item.Key, item.Value);
        }

        /// <summary>
        /// Clear this instance.
        /// </summary>
        public void Clear ()
        {
            m_bIsChanged = true;
            m_values.Clear ();
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
        public override int Count {
            get {
                return m_values.Count;
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
            return (m_values as IEnumerable<KeyValuePair<KeyT, ValT>>).GetEnumerator ();
        }


        #endregion


        #region IEnumerable implementation

        /// <summary>
        /// Gets the enumerator.
        /// </summary>
        /// <returns>The enumerator.</returns>
        System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator ()
        {
            return m_values.GetEnumerator ();
        }


        #endregion

        #region implemented abstract members of ContainerBase

        /// <summary>
        /// Is the container set to null?
        /// </summary>
        /// <returns>True if the container is set to null.</returns>
        public override bool IsNull ()
        {
            return Count == 0;
        }

        /// <summary>
        /// Set the container to null.
        /// </summary>
        public override void SetNull ()
        {
            Clear();
        }

        /// <summary>
        /// Determines whether this instance is changed.
        /// </summary>
        /// <returns><c>true</c> if this instance is changed; otherwise, <c>false</c>.</returns>
        public override bool IsChanged ()
        {
            if (m_bIsChanged)
                return m_bIsChanged;

            foreach (var kv in m_values)
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
            foreach (var kv in m_values)
            {
                kv.Value.SetChanged (changed);
            }
        }


        #endregion

        /// <summary>
        /// Adds a new key to the dictionary and return the value container.
        /// <para/>
        /// If the type of the key doesn't match the dictionaries key type an
        /// SoftwareViolation is thrown.
        /// <para/>
        /// </summary>
        /// <param name="key">The key to add.</param>
        /// <returns>The value container</returns>
        public override ContainerBase AddNull(object key)
        {
            if (key is KeyT)
            {
                return Add((KeyT)key);
            }

            throw new SoftwareViolationException($"DictionaryContainer.AddNull: The supplied key has wrong type! Expected {typeof(KeyT)}, got type {key.GetType()}");
        }

        /// <summary>
        /// Get the key at a particular position in the dictionary.
        /// <para/>
        /// Note that the order of keys in the dictionary is not guaranteed. Any insertion
        /// may reorder the contents.
        /// <para/>
        /// This is not a particularly "cheap" way of accessing the contents of a
        /// dictionary. Much better to use the iterators in the implementing class.
        /// <para/>
        /// For enumeration values, the returned value is an int ordinal.
        /// </summary>
        /// <param name="index"> an index between 0 and Count. </param>
        /// <returns>The key at a position in the dictionary.</returns>
        public override object GetKeyAt(int index)
        {
            return m_values.ElementAt(index).Key;
        }

        /// <summary>
        /// Get the container of the value at a particular position in the dictionary.
        /// <para/>
        /// Note that the order of keys in the dictionary is not guaranteed. Any insertion
        /// may reorder the contents.
        /// <para/>
        /// This is not a particularly "cheap" way of accessing the contents of a
        /// dictionary. Much better to use normal iteration.
        /// </summary>
        /// <param name="index">An index between 0 and Count.</param>
        /// <returns>The container at a position in the dictionary.</returns>
        public override ContainerBase GetValueContainerAt(int index)
        {
            return m_values.ElementAt(index).Value;
        }
        
        /// <summary>
        /// Copy.
        /// </summary>
        /// <param name="other">ContainerBase</param>
        public override void Copy(ContainerBase other)
        {
            ShallowCopy(other);
            DictionaryContainer<KeyT,ValT> that = (DictionaryContainer<KeyT,ValT>)other;
            m_values =  that.m_values.DeepClone();
        }

        internal override void ShallowCopy(ContainerBase other)
        {
            base.ShallowCopy(other);
            DictionaryContainer<KeyT,ValT> that = (DictionaryContainer<KeyT,ValT>)other;
            m_values = that.m_values;
        }

        internal override void Merge(DictionaryContainerBase that)
        {
            var other = (DictionaryContainer<KeyT, ValT>)that;
            foreach (var kv in other.m_values)
            {
                var fromContainerOB = kv.Value as ObjectContainerBase;
                //is it an object member?
                if (fromContainerOB != null)
                {
                    if (fromContainerOB.IsChangedHere())
                    {
                        ValT findVal;
                        if (!TryGetValue(kv.Key,out findVal))
                        {
                            throw new SoftwareViolationException
                                ("DictionaryContainer::Merge: Changed key not found in target!");
                        }

                        findVal.ShallowCopy(fromContainerOB);
                    }
                    else if (fromContainerOB.IsChanged())
                    {
                        ValT findVal;
                        if (!TryGetValue(kv.Key,out findVal))
                        {
                            throw new SoftwareViolationException
                                ("DictionaryContainer::Merge: Changed key not found in target!");
                        }

                        ObjectContainerBase intoContainerOB = findVal as ObjectContainerBase;

                        //recurse
                        Utilities.MergeChanges(intoContainerOB.InternalObj,fromContainerOB.InternalObj);
                    }
                }
                else
                {
                    if (kv.Value.IsChanged())
                    {
                        ValT findVal;
                        if (!TryGetValue(kv.Key,out findVal))
                        {
                            throw new SoftwareViolationException
                                ("DictionaryContainer::Merge: Changed key not found in target!");
                        }

                        findVal.ShallowCopy(kv.Value);
                    }
                }
            }
        }


        private System.Collections.Generic.SortedDictionary<KeyT, ValT> m_values;
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
    /// String dictionary container.
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
