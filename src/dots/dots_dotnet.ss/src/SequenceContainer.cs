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

        public int IndexOf (T item)
        {
            return values.IndexOf (item);
        }

        public void Insert (int index, T item)
        {
            m_bIsChanged = true;
            values.Insert (index, item);
        }

        public void RemoveAt (int index)
        {
            m_bIsChanged = true;
            values.RemoveAt (index);
        }

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

        public void Add (T item)
        {
            m_bIsChanged = true;
            values.Add (item);
        }

        public void Clear ()
        {
            m_bIsChanged = true;
            values.Clear ();
        }

        public bool Contains (T item)
        {
            return values.Contains (item);
        }

        public void CopyTo (T[] array, int arrayIndex)
        {
            m_bIsChanged = true;
            values.CopyTo (array, arrayIndex);
        }

        public bool Remove (T item)
        {
            bool removed=values.Remove (item);
            if (removed)
                m_bIsChanged = true;
            return removed;
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

        public IEnumerator<T> GetEnumerator ()
        {
            return values.GetEnumerator ();
        }

        #endregion

        #region IEnumerable implementation

        System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator ()
        {
            return (values as IEnumerable<T>).GetEnumerator ();
        }

        #endregion

        #region implemented abstract members of ContainerBase

        public override bool IsNull ()
        {
            return false;
        }

        public override void SetNull ()
        {
            throw new SoftwareViolationException("Sequences cannot be null!");
        }

        public override bool IsChanged ()
        {
            return m_bIsChanged;
        }

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

        protected SequenceContainer(SequenceContainer<T> other):
            base(other)
        {
        }
    }

    public class ObjectSequenceContainer<T> : SequenceContainer<T>, ICloneable where T : Safir.Dob.Typesystem.Object
    {
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

    public class Int32SequenceContainer : MemberSequenceContainer<Int32> {}
    public class Int64SequenceContainer : MemberSequenceContainer<Int64> {}
    public class StringSequenceContainer : MemberSequenceContainer<string> {}
    public class Float32SequenceContainer : MemberSequenceContainer<float> {}
    public class Float64SequenceContainer : MemberSequenceContainer<double> {}
    public class TypeIdSequenceContainer : MemberSequenceContainer<Int64> {}
    public class InstanceIdSequenceContainer : MemberSequenceContainer<InstanceId> {}
    public class ChannelIdSequenceContainer : MemberSequenceContainer<ChannelId> {}
    public class HandlerIdSequenceContainer : MemberSequenceContainer<HandlerId> {}
    public class EntityIdSequenceContainer : MemberSequenceContainer<EntityId> {}

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
