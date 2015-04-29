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
        protected internal List<T> values = new List<T> ();

        /// <summary>
        /// Default constructor
        /// <para/>
        /// Creates a null and not changed container.
        /// </summary>
        public SequenceContainer(): base()
        {
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="Safir.Dob.Typesystem.SequenceContainer`1"/> class.
        /// </summary>
        /// <param name="other">Other.</param>
        protected SequenceContainer(SequenceContainer<T> other):
            base(other)
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
            return new GenericObjectSequenceContainer<T>(this);
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
    public class ValueSequenceContainer<T> : SequenceContainer<T>, ICloneable
    {
        public ValueSequenceContainer(): base()
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
            ValueSequenceContainer<T> that = other as ValueSequenceContainer<T>;
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
            return new ValueSequenceContainer<T>(this);
        }

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        public new ValueSequenceContainer<T> Clone()
        {
            return new ValueSequenceContainer<T>(this);
        }

        /// <summary>
        /// Copy constructor for use by Clone
        /// </summary>
        /// <param name="other"></param>
        protected ValueSequenceContainer(ValueSequenceContainer<T> other):
            base(other)
        {
            foreach (var val in other.values) {
                this.values.Add(val);
            }
        }

        #endregion
    }

    /// <summary>
    /// String sequence container.
    /// </summary>
    public class StringSequenceContainer : ValueSequenceContainer<string>, ICloneable
    {
        /// <summary>
        /// Initializes a new instance.
        /// </summary>
        public StringSequenceContainer() : base() { }

        /// <summary>
        /// Clone this instance.
        /// </summary>
        object ICloneable.Clone() {return new StringSequenceContainer(this);}

        /// <summary>
        /// Clone this instance.
        /// </summary>
        public new StringSequenceContainer Clone() {return new StringSequenceContainer(this);}

        /// <summary>
        /// Initializes a new copy.
        /// </summary>
        /// <param name="other">Other.</param>
        protected StringSequenceContainer(StringSequenceContainer other) : base(other) { }
    }

    /// <summary>
    /// Boolean sequence container.
    /// </summary>
    public class BooleanSequenceContainer : ValueSequenceContainer<bool>, ICloneable
    {
        /// <summary>
        /// Initializes a new instance.
        /// </summary>
        public BooleanSequenceContainer() : base() { }

        /// <summary>
        /// Clone this instance.
        /// </summary>
        object ICloneable.Clone() {return new BooleanSequenceContainer(this);}

        /// <summary>
        /// Clone this instance.
        /// </summary>
        public new BooleanSequenceContainer Clone() {return new BooleanSequenceContainer(this);}

        /// <summary>
        /// Initializes a new copy.
        /// </summary>
        /// <param name="other">Other.</param>
        protected BooleanSequenceContainer(BooleanSequenceContainer other) : base(other) { }
    }

    /// <summary>
    /// Int32 sequence container.
    /// </summary>
    public class Int32SequenceContainer : ValueSequenceContainer<Int32>, ICloneable
    {
        /// <summary>
        /// Initializes a new instance.
        /// </summary>
        public Int32SequenceContainer() : base() { }

        /// <summary>
        /// Clone this instance.
        /// </summary>
        object ICloneable.Clone() {return new Int32SequenceContainer(this);}

        /// <summary>
        /// Clone this instance.
        /// </summary>
        public new Int32SequenceContainer Clone() {return new Int32SequenceContainer(this);}

        /// <summary>
        /// Initializes a new copy.
        /// </summary>
        /// <param name="other">Other.</param>
        protected Int32SequenceContainer(Int32SequenceContainer other) : base(other) { }
    }


    /// <summary>
    /// Int64 sequence container.
    /// </summary>
    public class Int64SequenceContainer : ValueSequenceContainer<Int64>, ICloneable
    {
        /// <summary>
        /// Initializes a new instance.
        /// </summary>
        public Int64SequenceContainer() : base() { }

        /// <summary>
        /// Clone this instance.
        /// </summary>
        object ICloneable.Clone() {return new Int64SequenceContainer(this);}

        /// <summary>
        /// Clone this instance.
        /// </summary>
        public new Int64SequenceContainer Clone() {return new Int64SequenceContainer(this);}

        /// <summary>
        /// Initializes a new copy.
        /// </summary>
        /// <param name="other">Other.</param>
        protected Int64SequenceContainer(Int64SequenceContainer other) : base(other) { }
    }

    /// <summary>
    /// Float32 sequence container.
    /// </summary>
    public class Float32SequenceContainer : ValueSequenceContainer<float>, ICloneable
    {
        /// <summary>
        /// Initializes a new instance.
        /// </summary>
        public Float32SequenceContainer() : base() { }

        /// <summary>
        /// Clone this instance.
        /// </summary>
        object ICloneable.Clone() {return new Float32SequenceContainer(this);}

        /// <summary>
        /// Clone this instance.
        /// </summary>
        public new Float32SequenceContainer Clone() {return new Float32SequenceContainer(this);}

        /// <summary>
        /// Initializes a new copy.
        /// </summary>
        /// <param name="other">Other.</param>
        protected Float32SequenceContainer(Float32SequenceContainer other) : base(other) { }
    }

    /// <summary>
    /// Float64 sequence container.
    /// </summary>
    public class Float64SequenceContainer : ValueSequenceContainer<double>, ICloneable
    {
        /// <summary>
        /// Initializes a new instance.
        /// </summary>
        public Float64SequenceContainer() : base() { }

        /// <summary>
        /// Clone this instance.
        /// </summary>
        object ICloneable.Clone() {return new Float64SequenceContainer(this);}

        /// <summary>
        /// Clone this instance.
        /// </summary>
        public new Float64SequenceContainer Clone() {return new Float64SequenceContainer(this);}

        /// <summary>
        /// Initializes a new copy.
        /// </summary>
        /// <param name="other">Other.</param>
        protected Float64SequenceContainer(Float64SequenceContainer other) : base(other) { }
    }

    /// <summary>
    /// TypeId sequence container.
    /// </summary>
    public class TypeIdSequenceContainer : ValueSequenceContainer<Int64>, ICloneable
    {
        /// <summary>
        /// Initializes a new instance.
        /// </summary>
        public TypeIdSequenceContainer() : base() { }

        /// <summary>
        /// Clone this instance.
        /// </summary>
        object ICloneable.Clone() {return new TypeIdSequenceContainer(this);}

        /// <summary>
        /// Clone this instance.
        /// </summary>
        public new TypeIdSequenceContainer Clone() {return new TypeIdSequenceContainer(this);}

        /// <summary>
        /// Initializes a new copy.
        /// </summary>
        /// <param name="other">Other.</param>
        protected TypeIdSequenceContainer(TypeIdSequenceContainer other) : base(other) { }
    }

    /// <summary>
    /// InstanceId sequence container.
    /// </summary>
    public class InstanceIdSequenceContainer : ValueSequenceContainer<InstanceId>, ICloneable
    {
        /// <summary>
        /// Initializes a new instance.
        /// </summary>
        public InstanceIdSequenceContainer() : base() { }

        /// <summary>
        /// Clone this instance.
        /// </summary>
        object ICloneable.Clone() {return new InstanceIdSequenceContainer(this);}

        /// <summary>
        /// Clone this instance.
        /// </summary>
        public new InstanceIdSequenceContainer Clone() {return new InstanceIdSequenceContainer(this);}

        /// <summary>
        /// Initializes a new copy.
        /// </summary>
        /// <param name="other">Other.</param>
        protected InstanceIdSequenceContainer(InstanceIdSequenceContainer other) : base(other) { }
    }

    /// <summary>
    /// ChannelId sequence container.
    /// </summary>
    public class ChannelIdSequenceContainer : ValueSequenceContainer<ChannelId>, ICloneable
    {
        /// <summary>
        /// Initializes a new instance.
        /// </summary>
        public ChannelIdSequenceContainer() : base() { }

        /// <summary>
        /// Clone this instance.
        /// </summary>
        object ICloneable.Clone() {return new ChannelIdSequenceContainer(this);}

        /// <summary>
        /// Clone this instance.
        /// </summary>
        public new ChannelIdSequenceContainer Clone() {return new ChannelIdSequenceContainer(this);}

        /// <summary>
        /// Initializes a new copy.
        /// </summary>
        /// <param name="other">Other.</param>
        protected ChannelIdSequenceContainer(ChannelIdSequenceContainer other) : base(other) { }
    }

    /// <summary>
    /// HandlerId sequence container.
    /// </summary>
    public class HandlerIdSequenceContainer : ValueSequenceContainer<HandlerId>, ICloneable
    {
        /// <summary>
        /// Initializes a new instance.
        /// </summary>
        public HandlerIdSequenceContainer() : base() { }

        /// <summary>
        /// Clone this instance.
        /// </summary>
        object ICloneable.Clone() {return new HandlerIdSequenceContainer(this);}

        /// <summary>
        /// Clone this instance.
        /// </summary>
        public new HandlerIdSequenceContainer Clone() {return new HandlerIdSequenceContainer(this);}

        /// <summary>
        /// Initializes a new copy.
        /// </summary>
        /// <param name="other">Other.</param>
        protected HandlerIdSequenceContainer(HandlerIdSequenceContainer other) : base(other) { }
    }

    /// <summary>
    /// EntityId sequence container.
    /// </summary>
    public class EntityIdSequenceContainer : ValueSequenceContainer<EntityId>, ICloneable
    {
        /// <summary>
        /// Initializes a new instance.
        /// </summary>
        public EntityIdSequenceContainer() : base() { }

        /// <summary>
        /// Clone this instance.
        /// </summary>
        object ICloneable.Clone() {return new EntityIdSequenceContainer(this);}

        /// <summary>
        /// Clone this instance.
        /// </summary>
        public new EntityIdSequenceContainer Clone() {return new EntityIdSequenceContainer(this);}

        /// <summary>
        /// Initializes a new copy.
        /// </summary>
        /// <param name="other">Other.</param>
        protected EntityIdSequenceContainer(EntityIdSequenceContainer other) : base(other) { }
    }

    /// <summary>
    /// Binary sequence container.
    /// </summary>
    public class BinarySequenceContainer : ValueSequenceContainer<byte[]>, ICloneable
    {
        /// <summary>
        /// Initializes a new instance.
        /// </summary>
        public BinarySequenceContainer() : base() { }

        /// <summary>
        /// Clone this instance.
        /// </summary>
        object ICloneable.Clone() {return new BinarySequenceContainer(this);}

        /// <summary>
        /// Clone this instance.
        /// </summary>
        public new BinarySequenceContainer Clone() {return new BinarySequenceContainer(this);}

        /// <summary>
        /// Initializes a new copy.
        /// </summary>
        /// <param name="other">Other.</param>
        protected BinarySequenceContainer(BinarySequenceContainer other) : base(other) { }
    }

    public class ObjectSequenceContainer : GenericObjectSequenceContainer<Safir.Dob.Typesystem.Object>, ICloneable
    {
        /// <summary>
        /// Initializes a new instance.
        /// </summary>
        public ObjectSequenceContainer() : base() { }

        /// <summary>
        /// Clone this instance.
        /// </summary>
        object ICloneable.Clone() {return new ObjectSequenceContainer(this);}

        /// <summary>
        /// Clone this instance.
        /// </summary>
        public new ObjectSequenceContainer Clone() {return new ObjectSequenceContainer(this);}

        /// <summary>
        /// Initializes a new copy.
        /// </summary>
        /// <param name="other">Other.</param>
        protected ObjectSequenceContainer(ObjectSequenceContainer other) : base(other) { }
    }

    //SI32
    namespace Si32
    {

        /// <summary>
        /// Ampere sequence container.
        /// </summary>
        public class AmpereSequenceContainer : Float32SequenceContainer, ICloneable
        {
            /// <summary>
            /// Initializes a new instance.
            /// </summary>
            public AmpereSequenceContainer() : base() { }

            /// <summary>
            /// Clone this instance.
            /// </summary>
            object ICloneable.Clone() {return new AmpereSequenceContainer(this);}

            /// <summary>
            /// Clone this instance.
            /// </summary>
            public new AmpereSequenceContainer Clone() {return new AmpereSequenceContainer(this);}

            /// <summary>
            /// Initializes a new copy.
            /// </summary>
            /// <param name="other">Other.</param>
            protected AmpereSequenceContainer(AmpereSequenceContainer other) : base(other) { }
        }

        /// <summary>
        /// CubicMeter sequence container.
        /// </summary>
        public class CubicMeterSequenceContainer : Float32SequenceContainer, ICloneable
        {
            /// <summary>
            /// Initializes a new instance.
            /// </summary>
            public CubicMeterSequenceContainer() : base() { }

            /// <summary>
            /// Clone this instance.
            /// </summary>
            object ICloneable.Clone() {return new CubicMeterSequenceContainer(this);}

            /// <summary>
            /// Clone this instance.
            /// </summary>
            public new CubicMeterSequenceContainer Clone() {return new CubicMeterSequenceContainer(this);}

            /// <summary>
            /// Initializes a new copy.
            /// </summary>
            /// <param name="other">Other.</param>
            protected CubicMeterSequenceContainer(CubicMeterSequenceContainer other) : base(other) { }
        }

        /// <summary>
        /// Hertz sequence container.
        /// </summary>
        public class HertzSequenceContainer : Float32SequenceContainer, ICloneable
        {
            /// <summary>
            /// Initializes a new instance.
            /// </summary>
            public HertzSequenceContainer() : base() { }

            /// <summary>
            /// Clone this instance.
            /// </summary>
            object ICloneable.Clone() {return new HertzSequenceContainer(this);}

            /// <summary>
            /// Clone this instance.
            /// </summary>
            public new HertzSequenceContainer Clone() {return new HertzSequenceContainer(this);}

            /// <summary>
            /// Initializes a new copy.
            /// </summary>
            /// <param name="other">Other.</param>
            protected HertzSequenceContainer(HertzSequenceContainer other) : base(other) { }
        }

        /// <summary>
        /// Joule sequence container.
        /// </summary>
        public class JouleSequenceContainer : Float32SequenceContainer, ICloneable
        {
            /// <summary>
            /// Initializes a new instance.
            /// </summary>
            public JouleSequenceContainer() : base() { }

            /// <summary>
            /// Clone this instance.
            /// </summary>
            object ICloneable.Clone() {return new JouleSequenceContainer(this);}

            /// <summary>
            /// Clone this instance.
            /// </summary>
            public new JouleSequenceContainer Clone() {return new JouleSequenceContainer(this);}

            /// <summary>
            /// Initializes a new copy.
            /// </summary>
            /// <param name="other">Other.</param>
            protected JouleSequenceContainer(JouleSequenceContainer other) : base(other) { }
        }

        /// <summary>
        /// Kelvin sequence container.
        /// </summary>
        public class KelvinSequenceContainer : Float32SequenceContainer, ICloneable
        {
            /// <summary>
            /// Initializes a new instance.
            /// </summary>
            public KelvinSequenceContainer() : base() { }

            /// <summary>
            /// Clone this instance.
            /// </summary>
            object ICloneable.Clone() {return new KelvinSequenceContainer(this);}

            /// <summary>
            /// Clone this instance.
            /// </summary>
            public new KelvinSequenceContainer Clone() {return new KelvinSequenceContainer(this);}

            /// <summary>
            /// Initializes a new copy.
            /// </summary>
            /// <param name="other">Other.</param>
            protected KelvinSequenceContainer(KelvinSequenceContainer other) : base(other) { }
        }

        /// <summary>
        /// Kilogram sequence container.
        /// </summary>
        public class KilogramSequenceContainer : Float32SequenceContainer, ICloneable
        {
            /// <summary>
            /// Initializes a new instance.
            /// </summary>
            public KilogramSequenceContainer() : base() { }

            /// <summary>
            /// Clone this instance.
            /// </summary>
            object ICloneable.Clone() {return new KilogramSequenceContainer(this);}

            /// <summary>
            /// Clone this instance.
            /// </summary>
            public new KilogramSequenceContainer Clone() {return new KilogramSequenceContainer(this);}

            /// <summary>
            /// Initializes a new copy.
            /// </summary>
            /// <param name="other">Other.</param>
            protected KilogramSequenceContainer(KilogramSequenceContainer other) : base(other) { }
        }

        /// <summary>
        /// Meter sequence container.
        /// </summary>
        public class MeterSequenceContainer : Float32SequenceContainer, ICloneable
        {
            /// <summary>
            /// Initializes a new instance.
            /// </summary>
            public MeterSequenceContainer() : base() { }

            /// <summary>
            /// Clone this instance.
            /// </summary>
            object ICloneable.Clone() {return new MeterSequenceContainer(this);}

            /// <summary>
            /// Clone this instance.
            /// </summary>
            public new MeterSequenceContainer Clone() {return new MeterSequenceContainer(this);}

            /// <summary>
            /// Initializes a new copy.
            /// </summary>
            /// <param name="other">Other.</param>
            protected MeterSequenceContainer(MeterSequenceContainer other) : base(other) { }
        }

        /// <summary>
        /// MeterPerSecond sequence container.
        /// </summary>
        public class MeterPerSecondSequenceContainer : Float32SequenceContainer, ICloneable
        {
            /// <summary>
            /// Initializes a new instance.
            /// </summary>
            public MeterPerSecondSequenceContainer() : base() { }

            /// <summary>
            /// Clone this instance.
            /// </summary>
            object ICloneable.Clone() {return new MeterPerSecondSequenceContainer(this);}

            /// <summary>
            /// Clone this instance.
            /// </summary>
            public new MeterPerSecondSequenceContainer Clone() {return new MeterPerSecondSequenceContainer(this);}

            /// <summary>
            /// Initializes a new copy.
            /// </summary>
            /// <param name="other">Other.</param>
            protected MeterPerSecondSequenceContainer(MeterPerSecondSequenceContainer other) : base(other) { }
        }

        /// <summary>
        /// MeterPerSecondSquared sequence container.
        /// </summary>
        public class MeterPerSecondSquaredSequenceContainer : Float32SequenceContainer, ICloneable
        {
            /// <summary>
            /// Initializes a new instance.
            /// </summary>
            public MeterPerSecondSquaredSequenceContainer() : base() { }

            /// <summary>
            /// Clone this instance.
            /// </summary>
            object ICloneable.Clone() {return new MeterPerSecondSquaredSequenceContainer(this);}

            /// <summary>
            /// Clone this instance.
            /// </summary>
            public new MeterPerSecondSquaredSequenceContainer Clone() {return new MeterPerSecondSquaredSequenceContainer(this);}

            /// <summary>
            /// Initializes a new copy.
            /// </summary>
            /// <param name="other">Other.</param>
            protected MeterPerSecondSquaredSequenceContainer(MeterPerSecondSquaredSequenceContainer other) : base(other) { }
        }

        /// <summary>
        /// Newton sequence container.
        /// </summary>
        public class NewtonSequenceContainer : Float32SequenceContainer, ICloneable
        {
            /// <summary>
            /// Initializes a new instance.
            /// </summary>
            public NewtonSequenceContainer() : base() { }

            /// <summary>
            /// Clone this instance.
            /// </summary>
            object ICloneable.Clone() {return new NewtonSequenceContainer(this);}

            /// <summary>
            /// Clone this instance.
            /// </summary>
            public new NewtonSequenceContainer Clone() {return new NewtonSequenceContainer(this);}

            /// <summary>
            /// Initializes a new copy.
            /// </summary>
            /// <param name="other">Other.</param>
            protected NewtonSequenceContainer(NewtonSequenceContainer other) : base(other) { }
        }

        /// <summary>
        /// Pascal sequence container.
        /// </summary>
        public class PascalSequenceContainer : Float32SequenceContainer, ICloneable
        {
            /// <summary>
            /// Initializes a new instance.
            /// </summary>
            public PascalSequenceContainer() : base() { }

            /// <summary>
            /// Clone this instance.
            /// </summary>
            object ICloneable.Clone() {return new PascalSequenceContainer(this);}

            /// <summary>
            /// Clone this instance.
            /// </summary>
            public new PascalSequenceContainer Clone() {return new PascalSequenceContainer(this);}

            /// <summary>
            /// Initializes a new copy.
            /// </summary>
            /// <param name="other">Other.</param>
            protected PascalSequenceContainer(PascalSequenceContainer other) : base(other) { }
        }

        /// <summary>
        /// Radian sequence container.
        /// </summary>
        public class RadianSequenceContainer : Float32SequenceContainer, ICloneable
        {
            /// <summary>
            /// Initializes a new instance.
            /// </summary>
            public RadianSequenceContainer() : base() { }

            /// <summary>
            /// Clone this instance.
            /// </summary>
            object ICloneable.Clone() {return new RadianSequenceContainer(this);}

            /// <summary>
            /// Clone this instance.
            /// </summary>
            public new RadianSequenceContainer Clone() {return new RadianSequenceContainer(this);}

            /// <summary>
            /// Initializes a new copy.
            /// </summary>
            /// <param name="other">Other.</param>
            protected RadianSequenceContainer(RadianSequenceContainer other) : base(other) { }
        }

        /// <summary>
        /// RadianPerSecond sequence container.
        /// </summary>
        public class RadianPerSecondSequenceContainer : Float32SequenceContainer, ICloneable
        {
            /// <summary>
            /// Initializes a new instance.
            /// </summary>
            public RadianPerSecondSequenceContainer() : base() { }

            /// <summary>
            /// Clone this instance.
            /// </summary>
            object ICloneable.Clone() {return new RadianPerSecondSequenceContainer(this);}

            /// <summary>
            /// Clone this instance.
            /// </summary>
            public new RadianPerSecondSequenceContainer Clone() {return new RadianPerSecondSequenceContainer(this);}

            /// <summary>
            /// Initializes a new copy.
            /// </summary>
            /// <param name="other">Other.</param>
            protected RadianPerSecondSequenceContainer(RadianPerSecondSequenceContainer other) : base(other) { }
        }

        /// <summary>
        /// RadianPerSecondSquared sequence container.
        /// </summary>
        public class RadianPerSecondSquaredSequenceContainer : Float32SequenceContainer, ICloneable
        {
            /// <summary>
            /// Initializes a new instance.
            /// </summary>
            public RadianPerSecondSquaredSequenceContainer() : base() { }

            /// <summary>
            /// Clone this instance.
            /// </summary>
            object ICloneable.Clone() {return new RadianPerSecondSquaredSequenceContainer(this);}

            /// <summary>
            /// Clone this instance.
            /// </summary>
            public new RadianPerSecondSquaredSequenceContainer Clone() {return new RadianPerSecondSquaredSequenceContainer(this);}

            /// <summary>
            /// Initializes a new copy.
            /// </summary>
            /// <param name="other">Other.</param>
            protected RadianPerSecondSquaredSequenceContainer(RadianPerSecondSquaredSequenceContainer other) : base(other) { }
        }

        /// <summary>
        /// Second sequence container.
        /// </summary>
        public class SecondSequenceContainer : Float32SequenceContainer, ICloneable
        {
            /// <summary>
            /// Initializes a new instance.
            /// </summary>
            public SecondSequenceContainer() : base() { }

            /// <summary>
            /// Clone this instance.
            /// </summary>
            object ICloneable.Clone() {return new SecondSequenceContainer(this);}

            /// <summary>
            /// Clone this instance.
            /// </summary>
            public new SecondSequenceContainer Clone() {return new SecondSequenceContainer(this);}

            /// <summary>
            /// Initializes a new copy.
            /// </summary>
            /// <param name="other">Other.</param>
            protected SecondSequenceContainer(SecondSequenceContainer other) : base(other) { }
        }

        /// <summary>
        /// SquareMeter sequence container.
        /// </summary>
        public class SquareMeterSequenceContainer : Float32SequenceContainer, ICloneable
        {
            /// <summary>
            /// Initializes a new instance.
            /// </summary>
            public SquareMeterSequenceContainer() : base() { }

            /// <summary>
            /// Clone this instance.
            /// </summary>
            object ICloneable.Clone() {return new SquareMeterSequenceContainer(this);}

            /// <summary>
            /// Clone this instance.
            /// </summary>
            public new SquareMeterSequenceContainer Clone() {return new SquareMeterSequenceContainer(this);}

            /// <summary>
            /// Initializes a new copy.
            /// </summary>
            /// <param name="other">Other.</param>
            protected SquareMeterSequenceContainer(SquareMeterSequenceContainer other) : base(other) { }
        }

        /// <summary>
        /// Steradian sequence container.
        /// </summary>
        public class SteradianSequenceContainer : Float32SequenceContainer, ICloneable
        {
            /// <summary>
            /// Initializes a new instance.
            /// </summary>
            public SteradianSequenceContainer() : base() { }

            /// <summary>
            /// Clone this instance.
            /// </summary>
            object ICloneable.Clone() {return new SteradianSequenceContainer(this);}

            /// <summary>
            /// Clone this instance.
            /// </summary>
            public new SteradianSequenceContainer Clone() {return new SteradianSequenceContainer(this);}

            /// <summary>
            /// Initializes a new copy.
            /// </summary>
            /// <param name="other">Other.</param>
            protected SteradianSequenceContainer(SteradianSequenceContainer other) : base(other) { }
        }

        /// <summary>
        /// Volt sequence container.
        /// </summary>
        public class VoltSequenceContainer : Float32SequenceContainer, ICloneable
        {
            /// <summary>
            /// Initializes a new instance.
            /// </summary>
            public VoltSequenceContainer() : base() { }

            /// <summary>
            /// Clone this instance.
            /// </summary>
            object ICloneable.Clone() {return new VoltSequenceContainer(this);}

            /// <summary>
            /// Clone this instance.
            /// </summary>
            public new VoltSequenceContainer Clone() {return new VoltSequenceContainer(this);}

            /// <summary>
            /// Initializes a new copy.
            /// </summary>
            /// <param name="other">Other.</param>
            protected VoltSequenceContainer(VoltSequenceContainer other) : base(other) { }
        }

        /// <summary>
        /// Watt sequence container.
        /// </summary>
        public class WattSequenceContainer : Float32SequenceContainer, ICloneable
        {
            /// <summary>
            /// Initializes a new instance.
            /// </summary>
            public WattSequenceContainer() : base() { }

            /// <summary>
            /// Clone this instance.
            /// </summary>
            object ICloneable.Clone() {return new WattSequenceContainer(this);}

            /// <summary>
            /// Clone this instance.
            /// </summary>
            public new WattSequenceContainer Clone() {return new WattSequenceContainer(this);}

            /// <summary>
            /// Initializes a new copy.
            /// </summary>
            /// <param name="other">Other.</param>
            protected WattSequenceContainer(WattSequenceContainer other) : base(other) { }
        }
    }

    //SI64
    namespace Si64
    {

        /// <summary>
        /// Ampere sequence container.
        /// </summary>
        public class AmpereSequenceContainer : Float64SequenceContainer, ICloneable
        {
            /// <summary>
            /// Initializes a new instance.
            /// </summary>
            public AmpereSequenceContainer() : base() { }

            /// <summary>
            /// Clone this instance.
            /// </summary>
            object ICloneable.Clone() {return new AmpereSequenceContainer(this);}

            /// <summary>
            /// Clone this instance.
            /// </summary>
            public new AmpereSequenceContainer Clone() {return new AmpereSequenceContainer(this);}

            /// <summary>
            /// Initializes a new copy.
            /// </summary>
            /// <param name="other">Other.</param>
            protected AmpereSequenceContainer(AmpereSequenceContainer other) : base(other) { }
        }

        /// <summary>
        /// CubicMeter sequence container.
        /// </summary>
        public class CubicMeterSequenceContainer : Float64SequenceContainer, ICloneable
        {
            /// <summary>
            /// Initializes a new instance.
            /// </summary>
            public CubicMeterSequenceContainer() : base() { }

            /// <summary>
            /// Clone this instance.
            /// </summary>
            object ICloneable.Clone() {return new CubicMeterSequenceContainer(this);}

            /// <summary>
            /// Clone this instance.
            /// </summary>
            public new CubicMeterSequenceContainer Clone() {return new CubicMeterSequenceContainer(this);}

            /// <summary>
            /// Initializes a new copy.
            /// </summary>
            /// <param name="other">Other.</param>
            protected CubicMeterSequenceContainer(CubicMeterSequenceContainer other) : base(other) { }
        }

        /// <summary>
        /// Hertz sequence container.
        /// </summary>
        public class HertzSequenceContainer : Float64SequenceContainer, ICloneable
        {
            /// <summary>
            /// Initializes a new instance.
            /// </summary>
            public HertzSequenceContainer() : base() { }

            /// <summary>
            /// Clone this instance.
            /// </summary>
            object ICloneable.Clone() {return new HertzSequenceContainer(this);}

            /// <summary>
            /// Clone this instance.
            /// </summary>
            public new HertzSequenceContainer Clone() {return new HertzSequenceContainer(this);}

            /// <summary>
            /// Initializes a new copy.
            /// </summary>
            /// <param name="other">Other.</param>
            protected HertzSequenceContainer(HertzSequenceContainer other) : base(other) { }
        }

        /// <summary>
        /// Joule sequence container.
        /// </summary>
        public class JouleSequenceContainer : Float64SequenceContainer, ICloneable
        {
            /// <summary>
            /// Initializes a new instance.
            /// </summary>
            public JouleSequenceContainer() : base() { }

            /// <summary>
            /// Clone this instance.
            /// </summary>
            object ICloneable.Clone() {return new JouleSequenceContainer(this);}

            /// <summary>
            /// Clone this instance.
            /// </summary>
            public new JouleSequenceContainer Clone() {return new JouleSequenceContainer(this);}

            /// <summary>
            /// Initializes a new copy.
            /// </summary>
            /// <param name="other">Other.</param>
            protected JouleSequenceContainer(JouleSequenceContainer other) : base(other) { }
        }

        /// <summary>
        /// Kelvin sequence container.
        /// </summary>
        public class KelvinSequenceContainer : Float64SequenceContainer, ICloneable
        {
            /// <summary>
            /// Initializes a new instance.
            /// </summary>
            public KelvinSequenceContainer() : base() { }

            /// <summary>
            /// Clone this instance.
            /// </summary>
            object ICloneable.Clone() {return new KelvinSequenceContainer(this);}

            /// <summary>
            /// Clone this instance.
            /// </summary>
            public new KelvinSequenceContainer Clone() {return new KelvinSequenceContainer(this);}

            /// <summary>
            /// Initializes a new copy.
            /// </summary>
            /// <param name="other">Other.</param>
            protected KelvinSequenceContainer(KelvinSequenceContainer other) : base(other) { }
        }

        /// <summary>
        /// Kilogram sequence container.
        /// </summary>
        public class KilogramSequenceContainer : Float64SequenceContainer, ICloneable
        {
            /// <summary>
            /// Initializes a new instance.
            /// </summary>
            public KilogramSequenceContainer() : base() { }

            /// <summary>
            /// Clone this instance.
            /// </summary>
            object ICloneable.Clone() {return new KilogramSequenceContainer(this);}

            /// <summary>
            /// Clone this instance.
            /// </summary>
            public new KilogramSequenceContainer Clone() {return new KilogramSequenceContainer(this);}

            /// <summary>
            /// Initializes a new copy.
            /// </summary>
            /// <param name="other">Other.</param>
            protected KilogramSequenceContainer(KilogramSequenceContainer other) : base(other) { }
        }

        /// <summary>
        /// Meter sequence container.
        /// </summary>
        public class MeterSequenceContainer : Float64SequenceContainer, ICloneable
        {
            /// <summary>
            /// Initializes a new instance.
            /// </summary>
            public MeterSequenceContainer() : base() { }

            /// <summary>
            /// Clone this instance.
            /// </summary>
            object ICloneable.Clone() {return new MeterSequenceContainer(this);}

            /// <summary>
            /// Clone this instance.
            /// </summary>
            public new MeterSequenceContainer Clone() {return new MeterSequenceContainer(this);}

            /// <summary>
            /// Initializes a new copy.
            /// </summary>
            /// <param name="other">Other.</param>
            protected MeterSequenceContainer(MeterSequenceContainer other) : base(other) { }
        }

        /// <summary>
        /// MeterPerSecond sequence container.
        /// </summary>
        public class MeterPerSecondSequenceContainer : Float64SequenceContainer, ICloneable
        {
            /// <summary>
            /// Initializes a new instance.
            /// </summary>
            public MeterPerSecondSequenceContainer() : base() { }

            /// <summary>
            /// Clone this instance.
            /// </summary>
            object ICloneable.Clone() {return new MeterPerSecondSequenceContainer(this);}

            /// <summary>
            /// Clone this instance.
            /// </summary>
            public new MeterPerSecondSequenceContainer Clone() {return new MeterPerSecondSequenceContainer(this);}

            /// <summary>
            /// Initializes a new copy.
            /// </summary>
            /// <param name="other">Other.</param>
            protected MeterPerSecondSequenceContainer(MeterPerSecondSequenceContainer other) : base(other) { }
        }

        /// <summary>
        /// MeterPerSecondSquared sequence container.
        /// </summary>
        public class MeterPerSecondSquaredSequenceContainer : Float64SequenceContainer, ICloneable
        {
            /// <summary>
            /// Initializes a new instance.
            /// </summary>
            public MeterPerSecondSquaredSequenceContainer() : base() { }

            /// <summary>
            /// Clone this instance.
            /// </summary>
            object ICloneable.Clone() {return new MeterPerSecondSquaredSequenceContainer(this);}

            /// <summary>
            /// Clone this instance.
            /// </summary>
            public new MeterPerSecondSquaredSequenceContainer Clone() {return new MeterPerSecondSquaredSequenceContainer(this);}

            /// <summary>
            /// Initializes a new copy.
            /// </summary>
            /// <param name="other">Other.</param>
            protected MeterPerSecondSquaredSequenceContainer(MeterPerSecondSquaredSequenceContainer other) : base(other) { }
        }

        /// <summary>
        /// Newton sequence container.
        /// </summary>
        public class NewtonSequenceContainer : Float64SequenceContainer, ICloneable
        {
            /// <summary>
            /// Initializes a new instance.
            /// </summary>
            public NewtonSequenceContainer() : base() { }

            /// <summary>
            /// Clone this instance.
            /// </summary>
            object ICloneable.Clone() {return new NewtonSequenceContainer(this);}

            /// <summary>
            /// Clone this instance.
            /// </summary>
            public new NewtonSequenceContainer Clone() {return new NewtonSequenceContainer(this);}

            /// <summary>
            /// Initializes a new copy.
            /// </summary>
            /// <param name="other">Other.</param>
            protected NewtonSequenceContainer(NewtonSequenceContainer other) : base(other) { }
        }

        /// <summary>
        /// Pascal sequence container.
        /// </summary>
        public class PascalSequenceContainer : Float64SequenceContainer, ICloneable
        {
            /// <summary>
            /// Initializes a new instance.
            /// </summary>
            public PascalSequenceContainer() : base() { }

            /// <summary>
            /// Clone this instance.
            /// </summary>
            object ICloneable.Clone() {return new PascalSequenceContainer(this);}

            /// <summary>
            /// Clone this instance.
            /// </summary>
            public new PascalSequenceContainer Clone() {return new PascalSequenceContainer(this);}

            /// <summary>
            /// Initializes a new copy.
            /// </summary>
            /// <param name="other">Other.</param>
            protected PascalSequenceContainer(PascalSequenceContainer other) : base(other) { }
        }

        /// <summary>
        /// Radian sequence container.
        /// </summary>
        public class RadianSequenceContainer : Float64SequenceContainer, ICloneable
        {
            /// <summary>
            /// Initializes a new instance.
            /// </summary>
            public RadianSequenceContainer() : base() { }

            /// <summary>
            /// Clone this instance.
            /// </summary>
            object ICloneable.Clone() {return new RadianSequenceContainer(this);}

            /// <summary>
            /// Clone this instance.
            /// </summary>
            public new RadianSequenceContainer Clone() {return new RadianSequenceContainer(this);}

            /// <summary>
            /// Initializes a new copy.
            /// </summary>
            /// <param name="other">Other.</param>
            protected RadianSequenceContainer(RadianSequenceContainer other) : base(other) { }
        }

        /// <summary>
        /// RadianPerSecond sequence container.
        /// </summary>
        public class RadianPerSecondSequenceContainer : Float64SequenceContainer, ICloneable
        {
            /// <summary>
            /// Initializes a new instance.
            /// </summary>
            public RadianPerSecondSequenceContainer() : base() { }

            /// <summary>
            /// Clone this instance.
            /// </summary>
            object ICloneable.Clone() {return new RadianPerSecondSequenceContainer(this);}

            /// <summary>
            /// Clone this instance.
            /// </summary>
            public new RadianPerSecondSequenceContainer Clone() {return new RadianPerSecondSequenceContainer(this);}

            /// <summary>
            /// Initializes a new copy.
            /// </summary>
            /// <param name="other">Other.</param>
            protected RadianPerSecondSequenceContainer(RadianPerSecondSequenceContainer other) : base(other) { }
        }

        /// <summary>
        /// RadianPerSecondSquared sequence container.
        /// </summary>
        public class RadianPerSecondSquaredSequenceContainer : Float64SequenceContainer, ICloneable
        {
            /// <summary>
            /// Initializes a new instance.
            /// </summary>
            public RadianPerSecondSquaredSequenceContainer() : base() { }

            /// <summary>
            /// Clone this instance.
            /// </summary>
            object ICloneable.Clone() {return new RadianPerSecondSquaredSequenceContainer(this);}

            /// <summary>
            /// Clone this instance.
            /// </summary>
            public new RadianPerSecondSquaredSequenceContainer Clone() {return new RadianPerSecondSquaredSequenceContainer(this);}

            /// <summary>
            /// Initializes a new copy.
            /// </summary>
            /// <param name="other">Other.</param>
            protected RadianPerSecondSquaredSequenceContainer(RadianPerSecondSquaredSequenceContainer other) : base(other) { }
        }

        /// <summary>
        /// Second sequence container.
        /// </summary>
        public class SecondSequenceContainer : Float64SequenceContainer, ICloneable
        {
            /// <summary>
            /// Initializes a new instance.
            /// </summary>
            public SecondSequenceContainer() : base() { }

            /// <summary>
            /// Clone this instance.
            /// </summary>
            object ICloneable.Clone() {return new SecondSequenceContainer(this);}

            /// <summary>
            /// Clone this instance.
            /// </summary>
            public new SecondSequenceContainer Clone() {return new SecondSequenceContainer(this);}

            /// <summary>
            /// Initializes a new copy.
            /// </summary>
            /// <param name="other">Other.</param>
            protected SecondSequenceContainer(SecondSequenceContainer other) : base(other) { }
        }

        /// <summary>
        /// SquareMeter sequence container.
        /// </summary>
        public class SquareMeterSequenceContainer : Float64SequenceContainer, ICloneable
        {
            /// <summary>
            /// Initializes a new instance.
            /// </summary>
            public SquareMeterSequenceContainer() : base() { }

            /// <summary>
            /// Clone this instance.
            /// </summary>
            object ICloneable.Clone() {return new SquareMeterSequenceContainer(this);}

            /// <summary>
            /// Clone this instance.
            /// </summary>
            public new SquareMeterSequenceContainer Clone() {return new SquareMeterSequenceContainer(this);}

            /// <summary>
            /// Initializes a new copy.
            /// </summary>
            /// <param name="other">Other.</param>
            protected SquareMeterSequenceContainer(SquareMeterSequenceContainer other) : base(other) { }
        }

        /// <summary>
        /// Steradian sequence container.
        /// </summary>
        public class SteradianSequenceContainer : Float64SequenceContainer, ICloneable
        {
            /// <summary>
            /// Initializes a new instance.
            /// </summary>
            public SteradianSequenceContainer() : base() { }

            /// <summary>
            /// Clone this instance.
            /// </summary>
            object ICloneable.Clone() {return new SteradianSequenceContainer(this);}

            /// <summary>
            /// Clone this instance.
            /// </summary>
            public new SteradianSequenceContainer Clone() {return new SteradianSequenceContainer(this);}

            /// <summary>
            /// Initializes a new copy.
            /// </summary>
            /// <param name="other">Other.</param>
            protected SteradianSequenceContainer(SteradianSequenceContainer other) : base(other) { }
        }

        /// <summary>
        /// Volt sequence container.
        /// </summary>
        public class VoltSequenceContainer : Float64SequenceContainer, ICloneable
        {
            /// <summary>
            /// Initializes a new instance.
            /// </summary>
            public VoltSequenceContainer() : base() { }

            /// <summary>
            /// Clone this instance.
            /// </summary>
            object ICloneable.Clone() {return new VoltSequenceContainer(this);}

            /// <summary>
            /// Clone this instance.
            /// </summary>
            public new VoltSequenceContainer Clone() {return new VoltSequenceContainer(this);}

            /// <summary>
            /// Initializes a new copy.
            /// </summary>
            /// <param name="other">Other.</param>
            protected VoltSequenceContainer(VoltSequenceContainer other) : base(other) { }
        }

        /// <summary>
        /// Watt sequence container.
        /// </summary>
        public class WattSequenceContainer : Float64SequenceContainer, ICloneable
        {
            /// <summary>
            /// Initializes a new instance.
            /// </summary>
            public WattSequenceContainer() : base() { }

            /// <summary>
            /// Clone this instance.
            /// </summary>
            object ICloneable.Clone() {return new WattSequenceContainer(this);}

            /// <summary>
            /// Clone this instance.
            /// </summary>
            public new WattSequenceContainer Clone() {return new WattSequenceContainer(this);}

            /// <summary>
            /// Initializes a new copy.
            /// </summary>
            /// <param name="other">Other.</param>
            protected WattSequenceContainer(WattSequenceContainer other) : base(other) { }
        }
    }
}
