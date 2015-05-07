/*
****************************************************************************
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
    public abstract class SequenceContainer<T> : ContainerBase, IList<T>
    {
        /// <summary>
        /// Default constructor
        /// <para/>
        /// Creates a null and not changed container.
        /// </summary>
        public SequenceContainer(): base()
        {
        }

        #region IList implementation

        /// <Docs>To be added.</Docs>
        /// <para>Determines the index of a specific item in the current instance.</para>
        /// <summary>
        /// Indexs the of.
        /// </summary>
        /// <returns>The of.</returns>
        /// <param name="item">Item.</param>
        public int IndexOf (T item)
        {
            return m_values.IndexOf (item);
        }

        /// <summary>
        /// Insert the specified index and item.
        /// </summary>
        /// <param name="index">Index.</param>
        /// <param name="item">Item.</param>
        public void Insert (int index, T item)
        {
            m_bIsChanged = true;
            m_values.Insert (index, item);
        }

        /// <summary>
        /// Removes at index.
        /// </summary>
        /// <param name="index">Index.</param>
        public void RemoveAt (int index)
        {
            m_bIsChanged = true;
            m_values.RemoveAt (index);
        }

        /// <summary>
        /// Gets or sets the value at the specified index.
        /// </summary>
        /// <param name="index">Index.</param>
        public T this [int index] {
            get {
                return m_values [index];
            }
            set {
                m_bIsChanged = true;
                m_values [index] = value;
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
        public void Add (T item)
        {
            m_bIsChanged = true;
            m_values.Add (item);
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
        public bool Contains (T item)
        {
            return m_values.Contains (item);
        }

        /// <summary>
        /// Copies to.
        /// </summary>
        /// <param name="array">Array.</param>
        /// <param name="arrayIndex">Array index.</param>
        public void CopyTo (T[] array, int arrayIndex)
        {
            m_bIsChanged = true;
            m_values.CopyTo (array, arrayIndex);
        }

        /// <Docs>The item to remove from the current collection.</Docs>
        /// <para>Removes the first occurrence of an item from the current collection.</para>
        /// <summary>
        /// Remove the specified item.
        /// </summary>
        /// <param name="item">Item.</param>
        public bool Remove (T item)
        {
            bool removed=m_values.Remove (item);
            if (removed)
                m_bIsChanged = true;
            return removed;
        }

        /// <summary>
        /// Gets the count.
        /// </summary>
        /// <value>The count.</value>
        public int Count {
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
        public IEnumerator<T> GetEnumerator ()
        {
            return m_values.GetEnumerator ();
        }

        #endregion

        #region IEnumerable implementation

        /// <summary>
        /// Gets the enumerator.
        /// </summary>
        /// <returns>The enumerator.</returns>
        System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator ()
        {
            return (m_values as IEnumerable<T>).GetEnumerator ();
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
            throw new SoftwareViolationException("Sequences cannot be null!");
        }

        /// <summary>
        /// Determines whether this instance is changed.
        /// </summary>
        /// <returns><c>true</c> if this instance is changed; otherwise, <c>false</c>.</returns>
        public override bool IsChanged ()
        {
            return m_bIsChanged;
        }

        /// <summary>
        /// Sets the changed.
        /// </summary>
        /// <param name="changed">If set to <c>true</c> changed.</param>
        public override void SetChanged (bool changed)
        {
            m_bIsChanged = changed;
        }

        #endregion


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

    /// <summary>Generic SequenceContainer for Objects.</summary>
    public class GenericObjectSequenceContainer<T> : SequenceContainer<T> where T : Safir.Dob.Typesystem.Object { }

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
