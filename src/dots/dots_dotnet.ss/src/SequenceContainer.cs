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
    public abstract class SequenceContainer<T> : ContainerBase, IList<T>, ICloneable
    {
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
            return values.IndexOf (item);
        }

        /// <summary>
        /// Insert the specified index and item.
        /// </summary>
        /// <param name="index">Index.</param>
        /// <param name="item">Item.</param>
        public void Insert (int index, T item)
        {
            m_bIsChanged = true;
            values.Insert (index, item);
        }

        /// <summary>
        /// Removes at index.
        /// </summary>
        /// <param name="index">Index.</param>
        public void RemoveAt (int index)
        {
            m_bIsChanged = true;
            values.RemoveAt (index);
        }

        /// <summary>
        /// Gets or sets the value at the specified index.
        /// </summary>
        /// <param name="index">Index.</param>
        public T this [int index] {
            get {
                return values [index];
            }
            set {
                m_bIsChanged = true;
                values [index] = value;
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
            values.Add (item);
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
        public bool Contains (T item)
        {
            return values.Contains (item);
        }

        /// <summary>
        /// Copies to.
        /// </summary>
        /// <param name="array">Array.</param>
        /// <param name="arrayIndex">Array index.</param>
        public void CopyTo (T[] array, int arrayIndex)
        {
            m_bIsChanged = true;
            values.CopyTo (array, arrayIndex);
        }

        /// <Docs>The item to remove from the current collection.</Docs>
        /// <para>Removes the first occurrence of an item from the current collection.</para>
        /// <summary>
        /// Remove the specified item.
        /// </summary>
        /// <param name="item">Item.</param>
        public bool Remove (T item)
        {
            bool removed=values.Remove (item);
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
        public IEnumerator<T> GetEnumerator ()
        {
            return values.GetEnumerator ();
        }

        #endregion

        #region IEnumerable implementation

        /// <summary>
        /// Gets the enumerator.
        /// </summary>
        /// <returns>The enumerator.</returns>
        System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator ()
        {
            return (values as IEnumerable<T>).GetEnumerator ();
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

        /// <summary>
        /// The internal storage for the values in the sequence.
        /// </summary>
        protected internal List<T> values;

        /// <summary>
        /// Default constructor
        /// <para/>
        /// Creates a null and not changed container.
        /// </summary>
        public SequenceContainer(): base()
        {
            values = new List<T> ();
        }

        /// <summary>
        /// Initializes a new instance.
        /// </summary>
        /// <param name="other">Other.</param>
        protected SequenceContainer(SequenceContainer<T> other):
            base(other)
        {
        }
    }

    /// <summary>
    /// Generic SequenceContainer for Objects.
    /// </summary>
    public class GenericObjectSequenceContainer<T> : SequenceContainer<T>, ICloneable where T : Safir.Dob.Typesystem.Object
    {
        /// <summary>
        /// Initializes a new instance.
        /// </summary>
        public GenericObjectSequenceContainer(): base()
        {
        }

        /// <summary>
        /// Override ContainerBase.
        /// </summary>
        /// <param name="other"></param>
        public override void Copy(ContainerBase other)
        {
            base.Copy(other);
            this.values.Clear ();
            GenericObjectSequenceContainer<T> that = other as GenericObjectSequenceContainer<T>;
            foreach (var val in that.values) {

                this.values.Add(val.Clone() as T);
            }
        }

        #region Cloning

        /// <summary>
        /// Clone this instance.
        /// </summary>
        object ICloneable.Clone()
        {
            return new GenericObjectSequenceContainer<T>(this);
        }

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        public new GenericObjectSequenceContainer<T> Clone()
        {
            return (GenericObjectSequenceContainer<T>)((ICloneable)this).Clone();
        }

        /// <summary>
        /// Copy constructor for use by Clone
        /// </summary>
        /// <param name="other"></param>
        protected GenericObjectSequenceContainer(GenericObjectSequenceContainer<T> other):
            base(other)
        {
            foreach (var val in other.values) {
                this.values.Add(val.Clone() as T);
            }
        }

        #endregion
    }

    /// <summary>
    /// Generic SequenceContainer for non-Object members.
    /// </summary>
    public class MemberSequenceContainer<T> : SequenceContainer<T>, ICloneable
    {
        public MemberSequenceContainer(): base()
        {
        }

        /// <summary>
        /// Override ContainerBase.
        /// </summary>
        /// <param name="other"></param>
        public override void Copy(ContainerBase other)
        {
            base.Copy(other);
            this.values.Clear ();
            MemberSequenceContainer<T> that = other as MemberSequenceContainer<T>;
            foreach (var val in that.values) {
                this.values.Add(val);
            }
        }

        #region Cloning

        /// <summary>
        /// Clone this instance.
        /// </summary>
        object ICloneable.Clone()
        {
            return new MemberSequenceContainer<T>(this);
        }

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        public new MemberSequenceContainer<T> Clone()
        {
            return (MemberSequenceContainer<T>)((ICloneable)this).Clone();
        }

        /// <summary>
        /// Copy constructor for use by Clone
        /// </summary>
        /// <param name="other"></param>
        protected MemberSequenceContainer(MemberSequenceContainer<T> other):
            base(other)
        {
            foreach (var val in other.values) {
                this.values.Add(val);
            }
        }

        #endregion
    }

    /// <summary>SequenceContainer for String.</summary>
    public class StringSequenceContainer : MemberSequenceContainer<string> {}

    /// <summary>SequenceContainer for Boolean.</summary>
    public class BooleanSequenceContainer : MemberSequenceContainer<bool> {}

    /// <summary>SequenceContainer for Int32.</summary>
    public class Int32SequenceContainer : MemberSequenceContainer<Int32> {}

    /// <summary>SequenceContainer for Int64.</summary>
    public class Int64SequenceContainer : MemberSequenceContainer<Int64> {}

    /// <summary>SequenceContainer for Float32.</summary>
    public class Float32SequenceContainer : MemberSequenceContainer<float> {}

    /// <summary>SequenceContainer for Float64.</summary>
    public class Float64SequenceContainer : MemberSequenceContainer<double> {}

    /// <summary>SequenceContainer for TypeId.</summary>
    public class TypeIdSequenceContainer : MemberSequenceContainer<Int64> {}

    /// <summary>SequenceContainer for InstanceId.</summary>
    public class InstanceIdSequenceContainer : MemberSequenceContainer<InstanceId> {}

    /// <summary>SequenceContainer for ChannelId.</summary>
    public class ChannelIdSequenceContainer : MemberSequenceContainer<ChannelId> {}

    /// <summary>SequenceContainer for HandlerId.</summary>
    public class HandlerIdSequenceContainer : MemberSequenceContainer<HandlerId> {}

    /// <summary>SequenceContainer for EntityId.</summary>
    public class EntityIdSequenceContainer : MemberSequenceContainer<EntityId> {}

    /// <summary>SequenceContainer for Binary.</summary>
    public class BinarySequenceContainer : MemberSequenceContainer<byte[]> {}

    /// <summary>SequenceContainer for Object.</summary>
    public class ObjectSequenceContainer : GenericObjectSequenceContainer<Safir.Dob.Typesystem.Object> {}

    //SI32
    namespace Si32
    {
        /// <summary>SequenceContainer for Ampere.</summary>
        public class AmpereSequenceContainer : Float32SequenceContainer {}

        /// <summary>SequenceContainer for CubicMeter.</summary>
        public class CubicMeterSequenceContainer : Float32SequenceContainer {}

        /// <summary>SequenceContainer for Hertz.</summary>
        public class HertzSequenceContainer : Float32SequenceContainer {}

        /// <summary>SequenceContainer for Joule.</summary>
        public class JouleSequenceContainer : Float32SequenceContainer {}

        /// <summary>SequenceContainer for Kelvin.</summary>
        public class KelvinSequenceContainer : Float32SequenceContainer {}

        /// <summary>SequenceContainer for Kilogram.</summary>
        public class KilogramSequenceContainer : Float32SequenceContainer {}

        /// <summary>SequenceContainer for Meter.</summary>
        public class MeterSequenceContainer : Float32SequenceContainer {}

        /// <summary>SequenceContainer for MeterPerSecond.</summary>
        public class MeterPerSecondSequenceContainer : Float32SequenceContainer {}

        /// <summary>SequenceContainer for MeterPerSecondSquared.</summary>
        public class MeterPerSecondSquaredSequenceContainer : Float32SequenceContainer {}

        /// <summary>SequenceContainer for Newton.</summary>
        public class NewtonSequenceContainer : Float32SequenceContainer {}

        /// <summary>SequenceContainer for Pascal.</summary>
        public class PascalSequenceContainer : Float32SequenceContainer {}

        /// <summary>SequenceContainer for Radian.</summary>
        public class RadianSequenceContainer : Float32SequenceContainer {}

        /// <summary>SequenceContainer for RadianPerSecond.</summary>
        public class RadianPerSecondSequenceContainer : Float32SequenceContainer {}

        /// <summary>SequenceContainer for RadianPerSecondSquared.</summary>
        public class RadianPerSecondSquaredSequenceContainer : Float32SequenceContainer {}

        /// <summary>SequenceContainer for Second.</summary>
        public class SecondSequenceContainer : Float32SequenceContainer {}

        /// <summary>SequenceContainer for SquareMeter.</summary>
        public class SquareMeterSequenceContainer : Float32SequenceContainer {}

        /// <summary>SequenceContainer for Steradian.</summary>
        public class SteradianSequenceContainer : Float32SequenceContainer {}

        /// <summary>SequenceContainer for Volt.</summary>
        public class VoltSequenceContainer : Float32SequenceContainer {}

        /// <summary>SequenceContainer for Watt.</summary>
        public class WattSequenceContainer : Float32SequenceContainer {}
    }

    //SI64
    namespace Si64
    {
        /// <summary>SequenceContainer for Ampere.</summary>
        public class AmpereSequenceContainer : Float64SequenceContainer {}

        /// <summary>SequenceContainer for CubicMeter.</summary>
        public class CubicMeterSequenceContainer : Float64SequenceContainer {}

        /// <summary>SequenceContainer for Hertz.</summary>
        public class HertzSequenceContainer : Float64SequenceContainer {}

        /// <summary>SequenceContainer for Joule.</summary>
        public class JouleSequenceContainer : Float64SequenceContainer {}

        /// <summary>SequenceContainer for Kelvin.</summary>
        public class KelvinSequenceContainer : Float64SequenceContainer {}

        /// <summary>SequenceContainer for Kilogram.</summary>
        public class KilogramSequenceContainer : Float64SequenceContainer {}

        /// <summary>SequenceContainer for Meter.</summary>
        public class MeterSequenceContainer : Float64SequenceContainer {}

        /// <summary>SequenceContainer for MeterPerSecond.</summary>
        public class MeterPerSecondSequenceContainer : Float64SequenceContainer {}

        /// <summary>SequenceContainer for MeterPerSecondSquared.</summary>
        public class MeterPerSecondSquaredSequenceContainer : Float64SequenceContainer {}

        /// <summary>SequenceContainer for Newton.</summary>
        public class NewtonSequenceContainer : Float64SequenceContainer {}

        /// <summary>SequenceContainer for Pascal.</summary>
        public class PascalSequenceContainer : Float64SequenceContainer {}

        /// <summary>SequenceContainer for Radian.</summary>
        public class RadianSequenceContainer : Float64SequenceContainer {}

        /// <summary>SequenceContainer for RadianPerSecond.</summary>
        public class RadianPerSecondSequenceContainer : Float64SequenceContainer {}

        /// <summary>SequenceContainer for RadianPerSecondSquared.</summary>
        public class RadianPerSecondSquaredSequenceContainer : Float64SequenceContainer {}

        /// <summary>SequenceContainer for Second.</summary>
        public class SecondSequenceContainer : Float64SequenceContainer {}

        /// <summary>SequenceContainer for SquareMeter.</summary>
        public class SquareMeterSequenceContainer : Float64SequenceContainer {}

        /// <summary>SequenceContainer for Steradian.</summary>
        public class SteradianSequenceContainer : Float64SequenceContainer {}

        /// <summary>SequenceContainer for Volt.</summary>
        public class VoltSequenceContainer : Float64SequenceContainer {}

        /// <summary>SequenceContainer for Watt.</summary>
        public class WattSequenceContainer : Float64SequenceContainer {}
    }


}
