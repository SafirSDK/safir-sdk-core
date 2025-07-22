/*
****************************************************************************
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
using System.Collections.Generic;
using System.Text;

namespace Safir.Dob.Typesystem
{
    /// <summary>
    /// Container class for sequences of values.
    /// <para/>
    /// A sequence is a collection of values that can dynamically grow or shrink in
    /// size. The whole container has a change flag that will automatically be set when
    /// values are added, removed or changed. Values in a sequence cannot be null and
    /// does not have individual change flags.
    /// </summary>
    /// <typeparam name="T">The type to contain.</typeparam>
    public abstract class SequenceContainer<T> : ContainerBase, IList<T>
    {
        /// <summary>
        /// Default constructor
        /// <para/>
        /// Creates an empty/null and not changed sequence container.
        /// </summary>
        public SequenceContainer(): base()
        {
        }


        /// <summary>
        /// Returns the index of a particular item, if it is in the sequence.
        /// Returns -1 if the item isn't in the sequence.
        /// </summary>
        /// <param name="item">Value to look for</param>
        public int IndexOf (T item)
        {
            return m_values.IndexOf (item);
        }

        /// <summary>
        /// Inserts value into the sequence at position index.
        /// index must be non-negative and less than or equal to the 
        /// number of elements in the sequence.  If index equals the number
        /// of items in the sequence, then value is appended to the end.
        /// </summary>
        /// <param name="index">Index to insert at.</param>
        /// <param name="item">Value to insert.</param>
        public void Insert (int index, T item)
        {
            m_bIsChanged = true;
            m_values.Insert (index, item);
        }

        /// <summary>
        /// Removes the item at position index.
        /// </summary>
        /// <param name="index">Index of value to remove.</param>
        public void RemoveAt (int index)
        {
            m_bIsChanged = true;
            m_values.RemoveAt (index);
        }

        /// <summary>
        /// Gets or sets the value at the specified index.
        /// </summary>
        /// <param name="index">Index in sequence to get or set.</param>
        public T this [int index] {
            get {
                return m_values [index];
            }
            set {
                m_bIsChanged = true;
                m_values [index] = value;
            }
        }

        /// <summary>
        /// Add a value last in the sequence.
        /// </summary>
        /// <param name="item">Value to add</param>
        public void Add (T item)
        {
            m_bIsChanged = true;
            m_values.Add (item);
        }

        /// <summary>
        /// Remove all values from this instance.
        /// </summary>
        public void Clear ()
        {
            m_bIsChanged = true;
            m_values.Clear ();
        }

        /// <summary>
        /// Determine whether the sequence contains a specific value.
        /// </summary>
        /// <param name="item">Value to look for.</param>
        public bool Contains (T item)
        {
            return m_values.Contains (item);
        }

        /// <summary>
        /// Copies the elements of the sequence into an Array, starting at a particular index.
        /// </summary>
        /// <param name="array">The one-dimensional Array that is the destination of the
        /// elements copied from the sequence. The Array must have zero-based indexing</param>
        /// <param name="arrayIndex">The zero-based index in array at which copying begins.</param>
        public void CopyTo (T[] array, int arrayIndex)
        {
            m_bIsChanged = true;
            m_values.CopyTo (array, arrayIndex);
        }

        /// <summary>
        /// Removes the first occurrence of a specific object from the sequence.
        /// </summary>
        /// <param name="item">Value to remove from the sequence.</param>
        public bool Remove (T item)
        {
            bool removed=m_values.Remove (item);
            if (removed)
                m_bIsChanged = true;
            return removed;
        }

        /// <summary>
        /// Gets the number of elements contained in the sequence.
        /// </summary>
        public int Count {
            get {
                return m_values.Count;
            }
        }

        /// <summary>
        /// Determine whether this instance is read only. Dob sequences are never read only.
        /// </summary>
        public bool IsReadOnly {
            get {
                return false;
            }
        }

        /// <summary>
        /// Gets an enumerator that can be used to iterate through the sequence.
        /// </summary>
        /// <returns>The enumerator.</returns>
        public IEnumerator<T> GetEnumerator ()
        {
            return m_values.GetEnumerator ();
        }

        /// <summary>
        /// Gets an enumerator that can be used to iterate through the sequence.
        /// </summary>
        /// <returns>The enumerator.</returns>
        System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator ()
        {
            return (m_values as IEnumerable<T>).GetEnumerator ();
        }

        /// <summary>
        /// Is the container set to null. Null is the same as empty.
        /// </summary>
        /// <returns>True if the container is set to null.</returns>
        public override bool IsNull ()
        {
            return Count == 0;
        }

        /// <summary>
        /// Override ContainerBase.
        /// </summary>
        public override bool HasVal ()
        {
            return Count > 0;
        }

        /// <summary>
        /// Set the container to null. Same as clearing the contents of the sequence.
        /// </summary>
        public override void SetNull ()
        {
            Clear();
        }

        /// <summary>
        /// Copy.
        /// </summary>
        /// <param name="other">Other SequenceContainer</param>
        public override void Copy(ContainerBase other)
        {
            ShallowCopy(other);
            SequenceContainer<T> that = (SequenceContainer<T>)other;
            m_values =  that.m_values.DeepClone();
        }

        internal override void ShallowCopy(ContainerBase other)
        {
            base.ShallowCopy(other);
            SequenceContainer<T> that = (SequenceContainer<T>)other;
            m_values = that.m_values;
        }

        /// <summary>
        /// The internal list holding the values.
        /// </summary>
        private List<T> m_values = new List<T> ();
    }

    /// <summary>
    /// Base class for containers for sequences of enumeration values. It allows for
    /// reflection on enumeration values, using ordinal values.
    /// </summary>
    public interface EnumerationSequenceContainerBase
    {
        /// <summary>
        /// Gets the number of elements contained in the sequence.
        /// </summary>
        int Count
        {
            get;
        }

        /// <summary>
        /// Get the ordinal value of the value at the specified index.
        /// </summary>
        /// <param name="index">Index of value to get</param>
        int GetOrdinal(int index);

        /// <summary>
        /// Update a specific value, using ordinal value. Will not add new values to the sequence.
        /// </summary>
        /// <param name="index">The index of the value to change</param>
        /// <param name="value">Value to change to.</param>
        /// <exception cref="Safir.Dob.Typesystem.IllegalValueException">If the value is not in the enumeration.</exception>
        void SetOrdinal(int index, int value);

        /// <summary>
        /// Insert a new ordinal value last in the sequence.
        /// <para/>
        /// If the sequence was null before it will no longer be null after this call.
        /// </summary>
        /// <exception cref="Safir.Dob.Typesystem.IllegalValueException">If the value is not in the enumeration.</exception>
        void AddOrdinal (int item);

        /// <summary>
        /// Removes the item at position index.
        /// </summary>
        /// <param name="index">Index of value to remove.</param>
        void RemoveAt (int index);

        /// <summary>
        /// Remove all values from this instance.
        /// </summary>
        void Clear();

        /// <summary>
        /// Set the container to null. Same as clearing the contents of the sequence.
        /// </summary>
        void SetNull();

        /// <summary>
        /// Is the container set to null. Null is the same as empty.
        /// </summary>
        /// <returns>True if the container is set to null.</returns>
        bool IsNull();

        /// <summary>
        /// Is the change flag set on the container?
        /// </summary>
        /// <returns> True if the containers change flag is set.</returns>
        bool IsChanged();

        /// <summary>
        /// Set the containers change flag.
        /// <para/>
        /// It should be fairly unusual for an application to have to use this
        /// operation. There is nothing dangerous about it, but are you sure this
        /// is the operation you were after?
        /// <para/>
        /// The change flag is how receivers of objects can work out what the
        /// sender really wanted done on the object.
        /// </summary>
        /// <param name="changed">The value to set the change flag to.</param>
        void SetChanged (bool changed);
    };

    /// <summary>
    /// Container for sequences of enumeration values.
    /// </summary>
    public abstract class EnumerationSequenceContainer<TEnum>
        : SequenceContainer<TEnum>
        , EnumerationSequenceContainerBase
    {
        /// <summary>
        /// Get the ordinal value of the value at the specified index.
        /// </summary>
        /// <param name="index">Index of value to get</param>
        public int GetOrdinal(int index)
        {
            return ToInt(base[index]);
        }

        /// <summary>
        /// Update a specific value, using ordinal value. Will not add new values to the sequence.
        /// </summary>
        /// <param name="index">The index of the value to change</param>
        /// <param name="value">Value to change to.</param>
        /// <exception cref="Safir.Dob.Typesystem.IllegalValueException">If the value is not in the enumeration.</exception>
        public void SetOrdinal(int index, int value)
        {
            base[index] = ToEnum(value);
        }

        /// <summary>
        /// Insert a new ordinal value last in the sequence.
        /// <para/>
        /// If the sequence was null before it will no longer be null after this call.
        /// </summary>
        /// <exception cref="Safir.Dob.Typesystem.IllegalValueException">If the value is not in the enumeration.</exception>
        public void AddOrdinal (int value)
        {
            base.Add(ToEnum(value));
        }
        
        private static int ToInt(TEnum value)
        {
            return (int)(object)value;
        }

        private static TEnum ToEnum(int value)
        {
            if (!System.Enum.IsDefined(typeof(TEnum), value))
            {
                throw new Safir.Dob.Typesystem.IllegalValueException("Value is not in the enumeration range");
            }
            return (TEnum)(object)value;
        }

    }

    /// <summary>
    /// Base class for all object sequences. Needed for the reflection stuff.
    /// </summary>
    public interface GenericObjectSequenceContainerBase: IList<Object>
    {
        /// <summary>
        /// Is the change flag in the container set?
        /// <para/>
        /// This method is like IsChanged without the recursion.
        /// </summary>
        /// <returns>True if the containers change flag is set.</returns>
        bool IsChangedHere ();

        /// <summary>
        /// Set the change flag in the container.
        /// <para/>
        /// This method is like SetChanged without the recursion.
        /// </summary>
        /// <param name="changed">The value to set the change flag to.</param>
        void SetChangedHere (bool changed);

        /// <summary>
        /// Function needed by Utilities::MergeChanges to be able to merge
        /// dictionaries. Will in turn call Utilities::MergeChanges recursively if it
        /// needs to merge objects.
        /// NOTE: Don't Call This Function (TM)!
        /// </summary>
        void Merge(GenericObjectSequenceContainerBase other);
    }


    /// <summary>
    /// Generic SequenceContainer for Objects.
    ///
    /// This implements recursive SetChanged and IsChanged behaviour.
    /// </summary>
    public abstract class GenericObjectSequenceContainer<T>
        : SequenceContainer<T>
        , GenericObjectSequenceContainerBase
        where T:Safir.Dob.Typesystem.Object
    {
        /// <summary>
        /// Override of inherited method. See comment for parent class.
        /// </summary>
        /// <returns></returns>
        public override bool IsChanged ()
        {
            if (m_bIsChanged)
            {
                return true;
            }

            foreach (var val in this)
            {
                if (val.IsChanged())
                {
                    return true;
                }
            }

            return false;
        }

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
        /// Override of inherited method. See comment for parent class.
        /// </summary>
        /// <returns></returns>
        public override void SetChanged (bool changed)
        {
            m_bIsChanged = changed;

            foreach (var val in this)
            {
                val.SetChanged(changed);
            }
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
        /// Function needed by Utilities::MergeChanges to be able to merge
        /// dictionaries. Will in turn call Utilities::MergeChanges recursively if it
        /// needs to merge objects.
        /// NOTE: Don't Call This Function (TM)!
        /// </summary>
        public void Merge(GenericObjectSequenceContainerBase that)
        {
            if (GetType() != that.GetType())
            {
                throw new SoftwareViolationException("Invalid call to Merge, containers are not of same type");
            }

            var other = that as GenericObjectSequenceContainer<T>;

            //Note: this function only gets called when IsChangedHere() == false

            if (!other.IsChanged())
            {
                return;
            }

            if (Count != other.Count)
            {
                throw new SoftwareViolationException("It is not possible to merge two object sequences of different sizes.");
            }

            for (var i = 0; i < Count; ++i)
            {
                if (other[i].IsChanged())
                {
                    //recurse
                    Utilities.MergeChanges(this[i],other[i]);
                }
            }

        }

        #region IList<Object> implementation, from GenericObjectSequenceContainerBase

        IEnumerator<Safir.Dob.Typesystem.Object> IEnumerable<Object>.GetEnumerator()
        {
            return base.GetEnumerator();
        }

        void ICollection<Object>.Add (Object item)
        {
            base.Add ((T)item);
        }

        bool ICollection<Object>.Contains (Object item)
        {
            return base.Contains ((T)item);
        }

        void ICollection<Object>.CopyTo (Object[] array, int arrayIndex)
        {
            base.CopyTo ((T[])array, arrayIndex);
        }

        bool ICollection<Object>.Remove (Object item)
        {
            return base.Remove((T)item);
        }

        Object IList<Object>.this [int index]
        {
            get
            {
                return base[index];
            }
            set
            {
                base[index] = (T)value;
            }
        }

        int IList<Object>.IndexOf (Object item)
        {
            return base.IndexOf ((T)item);
        }

        void IList<Object>.Insert (int index, Object item)
        {
            base.Insert (index, (T)item);
        }
        #endregion
    }

    /// <summary>Generic SequenceContainer for non-Object members.</summary>
    public class ValueSequenceContainer<T> : SequenceContainer<T> { }

    /// <summary>String sequence container.</summary>
    public class StringSequenceContainer : ValueSequenceContainer<string> {}

    /// <summary>Boolean sequence container.</summary>
    public class BooleanSequenceContainer : ValueSequenceContainer<bool> {}

    /// <summary>Int32 sequence container.</summary>
    public class Int32SequenceContainer : ValueSequenceContainer<Int32> {}

    /// <summary>Int64 sequence container.</summary>
    public class Int64SequenceContainer : ValueSequenceContainer<Int64> {}

    /// <summary>Float32 sequence container.</summary>
    public class Float32SequenceContainer : ValueSequenceContainer<float> {}

    /// <summary>Float64 sequence container.</summary>
    public class Float64SequenceContainer : ValueSequenceContainer<double> {}

    /// <summary>TypeId sequence container.</summary>
    public class TypeIdSequenceContainer : ValueSequenceContainer<Int64> {}

    /// <summary>InstanceId sequence container.</summary>
    public class InstanceIdSequenceContainer : ValueSequenceContainer<InstanceId> {}

    /// <summary>ChannelId sequence container.</summary>
    public class ChannelIdSequenceContainer : ValueSequenceContainer<ChannelId> {}

    /// <summary>HandlerId sequence container.</summary>
    public class HandlerIdSequenceContainer : ValueSequenceContainer<HandlerId> {}

    /// <summary>EntityId sequence container.</summary>
    public class EntityIdSequenceContainer : ValueSequenceContainer<EntityId> {}

    /// <summary>Binary sequence container.</summary>
    public class BinarySequenceContainer : ValueSequenceContainer<byte[]> {}


    /// <summary>Object sequence container.</summary>
    public class ObjectSequenceContainer : GenericObjectSequenceContainer<Safir.Dob.Typesystem.Object> {}

    //SI32
    namespace Si32
    {

        /// <summary>Ampere sequence container.</summary>
        public class AmpereSequenceContainer : Float32SequenceContainer {}

        /// <summary>CubicMeter sequence container.</summary>
        public class CubicMeterSequenceContainer : Float32SequenceContainer {}

        /// <summary>Hertz sequence container.</summary>
        public class HertzSequenceContainer : Float32SequenceContainer {}

        /// <summary>Joule sequence container.</summary>
        public class JouleSequenceContainer : Float32SequenceContainer {}

        /// <summary>Kelvin sequence container.</summary>
        public class KelvinSequenceContainer : Float32SequenceContainer {}

        /// <summary>Kilogram sequence container.</summary>
        public class KilogramSequenceContainer : Float32SequenceContainer {}

        /// <summary>Meter sequence container.</summary>
        public class MeterSequenceContainer : Float32SequenceContainer {}

        /// <summary>MeterPerSecond sequence container.</summary>
        public class MeterPerSecondSequenceContainer : Float32SequenceContainer {}

        /// <summary>MeterPerSecondSquared sequence container.</summary>
        public class MeterPerSecondSquaredSequenceContainer : Float32SequenceContainer {}

        /// <summary>Newton sequence container.</summary>
        public class NewtonSequenceContainer : Float32SequenceContainer {}

        /// <summary>Pascal sequence container.</summary>
        public class PascalSequenceContainer : Float32SequenceContainer {}

        /// <summary>Radian sequence container.</summary>
        public class RadianSequenceContainer : Float32SequenceContainer {}

        /// <summary>RadianPerSecond sequence container.</summary>
        public class RadianPerSecondSequenceContainer : Float32SequenceContainer {}

        /// <summary>RadianPerSecondSquared sequence container.</summary>
        public class RadianPerSecondSquaredSequenceContainer : Float32SequenceContainer {}

        /// <summary>Second sequence container.</summary>
        public class SecondSequenceContainer : Float32SequenceContainer {}

        /// <summary>SquareMeter sequence container.</summary>
        public class SquareMeterSequenceContainer : Float32SequenceContainer {}

        /// <summary>Steradian sequence container.</summary>
        public class SteradianSequenceContainer : Float32SequenceContainer {}

        /// <summary>Volt sequence container.</summary>
        public class VoltSequenceContainer : Float32SequenceContainer {}

        /// <summary>Watt sequence container.</summary>
        public class WattSequenceContainer : Float32SequenceContainer {}
    }

    //SI64
    namespace Si64
    {

        /// <summary>Ampere sequence container.</summary>
        public class AmpereSequenceContainer : Float64SequenceContainer {}

        /// <summary>CubicMeter sequence container.</summary>
        public class CubicMeterSequenceContainer : Float64SequenceContainer {}

        /// <summary>Hertz sequence container.</summary>
        public class HertzSequenceContainer : Float64SequenceContainer {}

        /// <summary>Joule sequence container.</summary>
        public class JouleSequenceContainer : Float64SequenceContainer {}

        /// <summary>Kelvin sequence container.</summary>
        public class KelvinSequenceContainer : Float64SequenceContainer {}

        /// <summary>Kilogram sequence container.</summary>
        public class KilogramSequenceContainer : Float64SequenceContainer {}

        /// <summary>Meter sequence container.</summary>
        public class MeterSequenceContainer : Float64SequenceContainer {}

        /// <summary>MeterPerSecond sequence container.</summary>
        public class MeterPerSecondSequenceContainer : Float64SequenceContainer {}

        /// <summary>MeterPerSecondSquared sequence container.</summary>
        public class MeterPerSecondSquaredSequenceContainer : Float64SequenceContainer {}

        /// <summary>Newton sequence container.</summary>
        public class NewtonSequenceContainer : Float64SequenceContainer {}

        /// <summary>Pascal sequence container.</summary>
        public class PascalSequenceContainer : Float64SequenceContainer {}

        /// <summary>Radian sequence container.</summary>
        public class RadianSequenceContainer : Float64SequenceContainer {}

        /// <summary>RadianPerSecond sequence container.</summary>
        public class RadianPerSecondSequenceContainer : Float64SequenceContainer {}

        /// <summary>RadianPerSecondSquared sequence container.</summary>
        public class RadianPerSecondSquaredSequenceContainer : Float64SequenceContainer {}

        /// <summary>Second sequence container.</summary>
        public class SecondSequenceContainer : Float64SequenceContainer {}

        /// <summary>SquareMeter sequence container.</summary>
        public class SquareMeterSequenceContainer : Float64SequenceContainer {}

        /// <summary>Steradian sequence container.</summary>
        public class SteradianSequenceContainer : Float64SequenceContainer {}

        /// <summary>Volt sequence container.</summary>
        public class VoltSequenceContainer : Float64SequenceContainer {}

        /// <summary>Watt sequence container.</summary>
        public class WattSequenceContainer : Float64SequenceContainer {}
    }
}
