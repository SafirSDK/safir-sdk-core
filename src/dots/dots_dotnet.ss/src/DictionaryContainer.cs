using System;
using System.Collections.Generic;

namespace Safir.Dob.Typesystem
{
    public class DictionaryContainer<KeyT, ValT> : ContainerBase, IDictionary<KeyT, ValT> 
        where ValT : ContainerBase, new()
    {
        private Dictionary<KeyT, ValT> values;

        public DictionaryContainer() : base()
        {
            values = new Dictionary<KeyT, ValT> ();
        }

        #region IDictionary implementation


        public void Add (KeyT key, ValT value)
        {
            m_bIsChanged = true;
            value.SetChanged (true);
            values.Add (key, value);
        }


        public bool ContainsKey (KeyT key)
        {
            return values.ContainsKey (key);
        }


        public bool Remove (KeyT key)
        {
            bool removed = values.Remove (key);
            if (removed)
                m_bIsChanged = true;

            return removed;
        }


        public bool TryGetValue (KeyT key, out ValT value)
        {
            return values.TryGetValue (key, out value);
        }

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

        public ICollection<KeyT> Keys {
            get {
                return values.Keys;
            }
        }

        public ICollection<ValT> Values {
            get {
                return values.Values;
            }
        }


        #endregion


        #region ICollection implementation


        public void Add (KeyValuePair<KeyT, ValT> item)
        {
            item.Value.SetChanged (true);
            values.Add (item.Key, item.Value);
        }


        public void Clear ()
        {
            values.Clear ();
        }


        public bool Contains (KeyValuePair<KeyT, ValT> item)
        {
            throw new NotImplementedException ();
        }


        public void CopyTo (KeyValuePair<KeyT, ValT>[] array, int arrayIndex)
        {
            throw new NotImplementedException ();
        }


        public bool Remove (KeyValuePair<KeyT, ValT> item)
        {
            return Remove(item.Key);
        }


        public int Count {
            get {
                return values.Count;
            }
        }


        public bool IsReadOnly {
            get {
                return false;
            }
        }


        #endregion


        #region IEnumerable implementation


        public IEnumerator<KeyValuePair<KeyT, ValT>> GetEnumerator ()
        {
            return (values as IEnumerable<KeyValuePair<KeyT, ValT>>).GetEnumerator ();
        }


        #endregion


        #region IEnumerable implementation


        System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator ()
        {
            return values.GetEnumerator ();
        }


        #endregion

        #region implemented abstract members of ContainerBase

        public override bool IsNull ()
        {
            return false;
        }

        public override void SetNull ()
        {
            throw new SoftwareViolationException("Dictionaries cannot be null!");
        }

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
}

