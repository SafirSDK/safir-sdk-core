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
        /// Gets or sets the <see cref="Safir.Dob.Typesystem.SequenceContainer`1"/> at the specified index.
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
        /// Initializes a new instance of the <see cref="Safir.Dob.Typesystem.SequenceContainer`1"/> class.
        /// </summary>
        /// <param name="other">Other.</param>
        protected SequenceContainer(SequenceContainer<T> other):
            base(other)
        {
        }
    }

    public class ObjectSequenceContainer<T> : SequenceContainer<T>, ICloneable where T : Safir.Dob.Typesystem.Object
    {
        /// <summary>
        /// Initializes a new instance of the <see cref="Safir.Dob.Typesystem.ObjectSequenceContainer`1"/> class.
        /// </summary>
        public ObjectSequenceContainer(): base()
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
            ObjectSequenceContainer<T> that = other as ObjectSequenceContainer<T>;
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
            return new ObjectSequenceContainer<T>(this);
        }

        /// <summary>
        /// 
        /// </summary>
        /// <returns></returns>
        public new ObjectSequenceContainer<T> Clone()
        {
            return (ObjectSequenceContainer<T>)((ICloneable)this).Clone(); 
        }

        /// <summary>
        /// Copy constructor for use by Clone
        /// </summary>
        /// <param name="other"></param>
        protected ObjectSequenceContainer(ObjectSequenceContainer<T> other):
            base(other)
        {
            foreach (var val in other.values) {
                this.values.Add(val.Clone() as T);
            }
        }

        #endregion
    }

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

    public class StringSequenceContainer : MemberSequenceContainer<string> {}
    public class BooleanSequenceContainer : MemberSequenceContainer<bool> {}
    public class Int32SequenceContainer : MemberSequenceContainer<Int32> {}
    public class Int64SequenceContainer : MemberSequenceContainer<Int64> {}
    public class Float32SequenceContainer : MemberSequenceContainer<float> {}
    public class Float64SequenceContainer : MemberSequenceContainer<double> {}
    public class TypeIdSequenceContainer : MemberSequenceContainer<Int64> {}
    public class InstanceIdSequenceContainer : MemberSequenceContainer<InstanceId> {}
    public class ChannelIdSequenceContainer : MemberSequenceContainer<ChannelId> {}
    public class HandlerIdSequenceContainer : MemberSequenceContainer<HandlerId> {}
    public class EntityIdSequenceContainer : MemberSequenceContainer<EntityId> {}
    public class BinarySequenceContainer : MemberSequenceContainer<byte[]> {}

    //SI32
    public class Ampere32SequenceContainer : Float32SequenceContainer {}
    public class CubicMeter32SequenceContainer : Float32SequenceContainer {}
    public class Hertz32SequenceContainer : Float32SequenceContainer {}
    public class Joule32SequenceContainer : Float32SequenceContainer {}
    public class Kelvin32SequenceContainer : Float32SequenceContainer {}
    public class Kilogram32SequenceContainer : Float32SequenceContainer {}
    public class Meter32SequenceContainer : Float32SequenceContainer {}
    public class MeterPerSecond32SequenceContainer : Float32SequenceContainer {}
    public class MeterPerSecondSquared32SequenceContainer : Float32SequenceContainer {}
    public class Newton32SequenceContainer : Float32SequenceContainer {}
    public class Pascal32SequenceContainer : Float32SequenceContainer {}
    public class Radian32SequenceContainer : Float32SequenceContainer {}
    public class RadianPerSecond32SequenceContainer : Float32SequenceContainer {}
    public class RadianPerSecondSquared32SequenceContainer : Float32SequenceContainer {}
    public class Second32SequenceContainer : Float32SequenceContainer {}
    public class SquareMeter32SequenceContainer : Float32SequenceContainer {}
    public class Steradian32SequenceContainer : Float32SequenceContainer {}
    public class Volt32SequenceContainer : Float32SequenceContainer {}
    public class Watt32SequenceContainer : Float32SequenceContainer {}

    //SI64
    public class Ampere64SequenceContainer : Float64SequenceContainer {}
    public class CubicMeter64SequenceContainer : Float64SequenceContainer {}
    public class Hertz64SequenceContainer : Float64SequenceContainer {}
    public class Joule64SequenceContainer : Float64SequenceContainer {}
    public class Kelvin64SequenceContainer : Float64SequenceContainer {}
    public class Kilogram64SequenceContainer : Float64SequenceContainer {}
    public class Meter64SequenceContainer : Float64SequenceContainer {}
    public class MeterPerSecond64SequenceContainer : Float64SequenceContainer {}
    public class MeterPerSecondSquared64SequenceContainer : Float64SequenceContainer {}
    public class Newton64SequenceContainer : Float64SequenceContainer {}
    public class Pascal64SequenceContainer : Float64SequenceContainer {}
    public class Radian64SequenceContainer : Float64SequenceContainer {}
    public class RadianPerSecond64SequenceContainer : Float64SequenceContainer {}
    public class RadianPerSecondSquared64SequenceContainer : Float64SequenceContainer {}
    public class Second64SequenceContainer : Float64SequenceContainer {}
    public class SquareMeter64SequenceContainer : Float64SequenceContainer {}
    public class Steradian64SequenceContainer : Float64SequenceContainer {}
    public class Volt64SequenceContainer : Float64SequenceContainer {}
    public class Watt64SequenceContainer : Float64SequenceContainer {}


}
