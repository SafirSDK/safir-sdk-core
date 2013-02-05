/******************************************************************************
*
* Copyright Saab AB, 2005-2008 (http://www.safirsdk.com)
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
using System.Text;

namespace Safir.Dob.Typesystem
{

    #region Container defintions

    /// <summary>
    /// BooleanContainer
    /// </summary>
    public class BooleanContainer : ValueContainer<bool>, ICloneable
    {
        /// <summary>
        /// Default constructor.
        /// </summary>
        public BooleanContainer() : base() { }

        #region Cloning

        object ICloneable.Clone()
        {
            return new BooleanContainer(this);
        }

        /// <summary>
        /// Clone.
        /// </summary>
        /// <returns>BooleanContainer</returns>
        public new BooleanContainer Clone()
        {
            return (BooleanContainer)((ICloneable)this).Clone();
        }

        /// <summary>
        /// Copy constructor.
        /// </summary>
        /// <param name="other"></param>
        protected BooleanContainer(BooleanContainer other) : base(other) { }

        #endregion
    }

    /// <summary>
    /// Int32Container
    /// </summary>
    public class Int32Container : ValueContainer<int>, ICloneable
    {
        /// <summary>
        /// Default constructor.
        /// </summary>
        public Int32Container() : base() { }

        #region Cloning

        object ICloneable.Clone()
        {
            return new Int32Container(this);
        }

        /// <summary>
        /// Clone.
        /// </summary>
        /// <returns>Int32Container</returns>
        public new Int32Container Clone()
        {
            return (Int32Container)((ICloneable)this).Clone();
        }

        /// <summary>
        /// Copy constructor.
        /// </summary>
        /// <param name="other">Int32Container</param>
        protected Int32Container(Int32Container other) : base(other) { }

        #endregion
    }

    /// <summary>
    /// Int64Container
    /// </summary>
    public class Int64Container : ValueContainer<System.Int64>, ICloneable
    {
        /// <summary>
        /// Default constructor.
        /// </summary>
        public Int64Container() : base() { }

        #region Cloning

        object ICloneable.Clone()
        {
            return new Int64Container(this);
        }

        /// <summary>
        /// Clone.
        /// </summary>
        /// <returns>Int64Container</returns>
        public new Int64Container Clone()
        {
            return (Int64Container)((ICloneable)this).Clone();
        }

        /// <summary>
        /// Copy constructor.
        /// </summary>
        /// <param name="other">Int64Container</param>
        protected Int64Container(Int64Container other) : base(other) { }

        #endregion
    }

    /// <summary>
    /// Float32Container
    /// </summary>
    public class Float32Container : ValueContainer<float>, ICloneable
    {
        /// <summary>
        /// Default constructor.
        /// </summary>
        public Float32Container() : base() { }

        #region Cloning

        object ICloneable.Clone()
        {
            return new Float32Container(this);
        }

        /// <summary>
        /// Clone.
        /// </summary>
        /// <returns>Float32Container</returns>
        public new Float32Container Clone()
        {
            return (Float32Container)((ICloneable)this).Clone();
        }

        /// <summary>
        /// Copy constructor.
        /// </summary>
        /// <param name="other">Float32Container</param>
        protected Float32Container(Float32Container other) : base(other) { }

        #endregion
    }

    /// <summary>
    /// Float64Container
    /// </summary>
    public class Float64Container : ValueContainer<double>, ICloneable
    {
        /// <summary>
        /// Default constructor.
        /// </summary>
        public Float64Container() : base() { }

        #region Cloning

        object ICloneable.Clone()
        {
            return new Float64Container(this);
        }

        /// <summary>
        /// Clone.
        /// </summary>
        /// <returns>Float64Container</returns>
        public new Float64Container Clone()
        {
            return (Float64Container)((ICloneable)this).Clone();
        }

        /// <summary>
        /// Copy constructor.
        /// </summary>
        /// <param name="other">Float64Container</param>
        protected Float64Container(Float64Container other) : base(other) { }

        #endregion
    }

    /// <summary>
    /// TypeIdContainer
    /// </summary>
    public class TypeIdContainer : Int64Container, ICloneable
    {
        /// <summary>
        /// Constructor
        /// </summary>
        public TypeIdContainer() : base() { }

        #region Cloning

        object ICloneable.Clone()
        {
            return new TypeIdContainer(this);
        }

        /// <summary>
        /// Clone.
        /// </summary>
        /// <returns></returns>
        public new TypeIdContainer Clone()
        {
            return (TypeIdContainer)((ICloneable)this).Clone();
        }

        /// <summary>
        /// Copy constructor for use by Clone
        /// </summary>
        /// <param name="other"></param>
        protected TypeIdContainer(TypeIdContainer other) : base(other) { }

        #endregion
    }

    /// <summary>
    /// InstanceIdContainer
    /// </summary>
    public class InstanceIdContainer : ValueContainer<InstanceId>, ICloneable
    {
        /// <summary>
        /// Default constructor.
        /// </summary>
        public InstanceIdContainer() : base() { m_Value = new InstanceId(); }

        #region Cloning

        object ICloneable.Clone()
        {
            return new InstanceIdContainer(this);
        }

        /// <summary>
        /// Clone.
        /// </summary>
        /// <returns>InstanceIdContainer</returns>
        public new InstanceIdContainer Clone()
        {
            return (InstanceIdContainer)((ICloneable)this).Clone();
        }

        /// <summary>
        /// Copy constructor.
        /// </summary>
        /// <param name="other">InstanceIdContainer</param>
        protected InstanceIdContainer(InstanceIdContainer other) : base(other) { }

        #endregion
    }

    /// <summary>
    /// EntityIdContainer
    /// </summary>
    public class EntityIdContainer : ValueContainer<EntityId>, ICloneable
    {
        /// <summary>
        /// Default constructor.
        /// </summary>
        public EntityIdContainer() : base() { }

        #region Cloning

        object ICloneable.Clone()
        {
            return new EntityIdContainer(this);
        }


        /// <summary>
        /// Clone.
        /// </summary>
        /// <returns>EntityIdContainer</returns>
        public new EntityIdContainer Clone()
        {
            return (EntityIdContainer)((ICloneable)this).Clone();
        }

        /// <summary>
        /// Copy constructor.
        /// </summary>
        /// <param name="other">EntityIdContainer</param>
        protected EntityIdContainer(EntityIdContainer other) : base(other) { }

        #endregion
    }

    /// <summary>
    /// ChannelIdContainer
    /// </summary>
    public class ChannelIdContainer : ValueContainer<ChannelId>, ICloneable
    {
        /// <summary>
        /// Default constructor.
        /// </summary>
        public ChannelIdContainer() : base() { m_Value = new ChannelId(); }

        #region Cloning

        object ICloneable.Clone()
        {
            return new ChannelIdContainer(this);
        }


        /// <summary>
        /// Clone.
        /// </summary>
        /// <returns>ChannelIdContainer</returns>
        public new ChannelIdContainer Clone()
        {
            return (ChannelIdContainer)((ICloneable)this).Clone();
        }

        /// <summary>
        /// Copy constructor.
        /// </summary>
        /// <param name="other">ChannelIdContainer</param>
        protected ChannelIdContainer(ChannelIdContainer other) : base(other) { }

        #endregion
    }

    /// <summary>
    /// HandlerIdContainer
    /// </summary>
    public class HandlerIdContainer : ValueContainer<HandlerId>, ICloneable
    {
        /// <summary>
        /// Default constructor.
        /// </summary>
        public HandlerIdContainer() : base() { m_Value = new HandlerId(); }

        #region Cloning

        object ICloneable.Clone()
        {
            return new HandlerIdContainer(this);
        }


        /// <summary>
        /// Clone.
        /// </summary>
        /// <returns>HandlerIdContainer</returns>
        public new HandlerIdContainer Clone()
        {
            return (HandlerIdContainer)((ICloneable)this).Clone();
        }

        /// <summary>
        /// Copy constructor.
        /// </summary>
        /// <param name="other">HandlerIdContainer</param>
        protected HandlerIdContainer(HandlerIdContainer other) : base(other) { }

        #endregion
    }

    /// <summary>
    /// ObjectContainer
    /// </summary>
    public class ObjectContainer : ObjectContainerImpl<Object>, ICloneable
    {
        /// <summary>
        /// Default constructor.
        /// </summary>
        public ObjectContainer() : base() { }

        #region Cloning

        object ICloneable.Clone()
        {
            return new ObjectContainer(this);
        }

        /// <summary>
        /// Clone.
        /// </summary>
        /// <returns>ObjectContainer</returns>
        public new ObjectContainer Clone()
        {
            return (ObjectContainer)((ICloneable)this).Clone();
        }

        /// <summary>
        /// Copy constructor.
        /// </summary>
        /// <param name="other">ObjectContainer</param>
        protected ObjectContainer(ObjectContainer other) : base(other) { }

        #endregion
    }

    /// <summary>
    /// BinaryContainer
    /// </summary>
    public class BinaryContainer : ValueContainer<byte[]>, ICloneable
    {
        /// <summary>
        /// Default constructor.
        /// </summary>
        public BinaryContainer() : base() { }

        #region Cloning

        object ICloneable.Clone()
        {
            return new BinaryContainer(this);
        }

        /// <summary>
        /// Clone.
        /// </summary>
        /// <returns>BinaryContainer</returns>
        public new BinaryContainer Clone()
        {
            return (BinaryContainer)((ICloneable)this).Clone();
        }

        /// <summary>
        /// Copy constructor.
        /// </summary>
        /// <param name="other">BinaryContainer</param>
        protected BinaryContainer(BinaryContainer other) : base(other) 
        {
            //m_Value now points to the same binary as other.m_Value.
            //So we clone it so we get a copy for ourselves
            m_Value = (byte[])m_Value.Clone();
        }

        #endregion
    }

    //--------------------------------------------------
    // SI-types (32-bits)
    //--------------------------------------------------
    namespace Si32
    {

        /// <summary>
        /// AmpereContainer
        /// </summary>
        public class AmpereContainer : Float32Container, ICloneable
        {
            /// <summary>
            /// Default constructor.
            /// </summary>
            public AmpereContainer() : base() { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new AmpereContainer(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>AmpereContainer</returns>
            public new AmpereContainer Clone()
            {
                return (AmpereContainer)((ICloneable)this).Clone();
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">AmpereContainer</param>
            protected AmpereContainer(AmpereContainer other) : base(other) { }

            #endregion
        }


        /// <summary>
        /// CubicMeterContainer
        /// </summary>
        public class CubicMeterContainer : Float32Container, ICloneable
        {
            /// <summary>
            /// Default constructor.
            /// </summary>
            public CubicMeterContainer() : base() { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new CubicMeterContainer(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>CubicMeterContainer</returns>
            public new CubicMeterContainer Clone()
            {
                return (CubicMeterContainer)((ICloneable)this).Clone();
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">CubicMeterContainer</param>
            protected CubicMeterContainer(CubicMeterContainer other) : base(other) { }

            #endregion
        }


        /// <summary>
        /// HertzContainer
        /// </summary>
        public class HertzContainer : Float32Container, ICloneable
        {
            /// <summary>
            /// Default constructor.
            /// </summary>
            public HertzContainer() : base() { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new HertzContainer(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>HertzContainer</returns>
            public new HertzContainer Clone()
            {
                return (HertzContainer)((ICloneable)this).Clone();
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">HertzContainer</param>
            protected HertzContainer(HertzContainer other) : base(other) { }

            #endregion
        }


        /// <summary>
        /// JouleContainer
        /// </summary>
        public class JouleContainer : Float32Container, ICloneable
        {
            /// <summary>
            /// Default constructor.
            /// </summary>
            public JouleContainer() : base() { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new JouleContainer(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>JouleContainer</returns>
            public new JouleContainer Clone()
            {
                return (JouleContainer)((ICloneable)this).Clone();
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">JouleContainer</param>
            protected JouleContainer(JouleContainer other) : base(other) { }

            #endregion
        }


        /// <summary>
        /// KelvinContainer
        /// </summary>
        public class KelvinContainer : Float32Container, ICloneable
        {
            /// <summary>
            /// Default constructor.
            /// </summary>
            public KelvinContainer() : base() { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new KelvinContainer(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>KelvinContainer</returns>
            public new KelvinContainer Clone()
            {
                return (KelvinContainer)((ICloneable)this).Clone();
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">KelvinContainer</param>
            protected KelvinContainer(KelvinContainer other) : base(other) { }

            #endregion
        }


        /// <summary>
        /// KilogramContainer
        /// </summary>
        public class KilogramContainer : Float32Container, ICloneable
        {
            /// <summary>
            /// Default constructor.
            /// </summary>
            public KilogramContainer() : base() { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new KilogramContainer(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>KilogramContainer</returns>
            public new KilogramContainer Clone()
            {
                return (KilogramContainer)((ICloneable)this).Clone();
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">KilogramContainer</param>
            protected KilogramContainer(KilogramContainer other) : base(other) { }

            #endregion
        }


        /// <summary>
        /// MeterContainer
        /// </summary>
        public class MeterContainer : Float32Container, ICloneable
        {
            /// <summary>
            /// Default constructor.
            /// </summary>
            public MeterContainer() : base() { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new MeterContainer(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>MeterContainer</returns>
            public new MeterContainer Clone()
            {
                return (MeterContainer)((ICloneable)this).Clone();
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">MeterContainer</param>
            protected MeterContainer(MeterContainer other) : base(other) { }

            #endregion
        }


        /// <summary>
        /// MeterPerSecondContainer
        /// </summary>
        public class MeterPerSecondContainer : Float32Container, ICloneable
        {
            /// <summary>
            /// Default constructor.
            /// </summary>
            public MeterPerSecondContainer() : base() { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new MeterPerSecondContainer(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>MeterPerSecondContainer</returns>
            public new MeterPerSecondContainer Clone()
            {
                return (MeterPerSecondContainer)((ICloneable)this).Clone();
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">MeterPerSecondContainer</param>
            protected MeterPerSecondContainer(MeterPerSecondContainer other) : base(other) { }

            #endregion
        }


        /// <summary>
        /// MeterPerSecondSquaredContainer
        /// </summary>
        public class MeterPerSecondSquaredContainer : Float32Container, ICloneable
        {
            /// <summary>
            /// Default constructor.
            /// </summary>
            public MeterPerSecondSquaredContainer() : base() { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new MeterPerSecondSquaredContainer(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>MeterPerSecondSquaredContainer</returns>
            public new MeterPerSecondSquaredContainer Clone()
            {
                return (MeterPerSecondSquaredContainer)((ICloneable)this).Clone();
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">MeterPerSecondSquaredContainer</param>
            protected MeterPerSecondSquaredContainer(MeterPerSecondSquaredContainer other) : base(other) { }

            #endregion
        }


        /// <summary>
        /// NewtonContainer
        /// </summary>
        public class NewtonContainer : Float32Container, ICloneable
        {
            /// <summary>
            /// Default constructor.
            /// </summary>
            public NewtonContainer() : base() { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new NewtonContainer(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>NewtonContainer</returns>
            public new NewtonContainer Clone()
            {
                return (NewtonContainer)((ICloneable)this).Clone();
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">NewtonContainer</param>
            protected NewtonContainer(NewtonContainer other) : base(other) { }

            #endregion
        }


        /// <summary>
        /// PascalContainer
        /// </summary>
        public class PascalContainer : Float32Container, ICloneable
        {
            /// <summary>
            /// Default constructor.
            /// </summary>
            public PascalContainer() : base() { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new PascalContainer(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>PascalContainer</returns>
            public new PascalContainer Clone()
            {
                return (PascalContainer)((ICloneable)this).Clone();
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">PascalContainer</param>
            protected PascalContainer(PascalContainer other) : base(other) { }

            #endregion
        }


        /// <summary>
        /// RadianContainer
        /// </summary>
        public class RadianContainer : Float32Container, ICloneable
        {
            /// <summary>
            /// Default constructor.
            /// </summary>
            public RadianContainer() : base() { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new RadianContainer(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>RadianContainer</returns>
            public new RadianContainer Clone()
            {
                return (RadianContainer)((ICloneable)this).Clone();
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">RadianContainer</param>
            protected RadianContainer(RadianContainer other) : base(other) { }

            #endregion
        }


        /// <summary>
        /// RadianPerSecondContainer
        /// </summary>
        public class RadianPerSecondContainer : Float32Container, ICloneable
        {
            /// <summary>
            /// Default constructor.
            /// </summary>
            public RadianPerSecondContainer() : base() { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new RadianPerSecondContainer(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>RadianPerSecondContainer</returns>
            public new RadianPerSecondContainer Clone()
            {
                return (RadianPerSecondContainer)((ICloneable)this).Clone();
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">RadianPerSecondContainer</param>
            protected RadianPerSecondContainer(RadianPerSecondContainer other) : base(other) { }

            #endregion
        }


        /// <summary>
        /// RadianPerSecondSquaredContainer
        /// </summary>
        public class RadianPerSecondSquaredContainer : Float32Container, ICloneable
        {
            /// <summary>
            /// Default constructor.
            /// </summary>
            public RadianPerSecondSquaredContainer() : base() { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new RadianPerSecondSquaredContainer(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>RadianPerSecondSquaredContainer</returns>
            public new RadianPerSecondSquaredContainer Clone()
            {
                return (RadianPerSecondSquaredContainer)((ICloneable)this).Clone();
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">RadianPerSecondSquaredContainer</param>
            protected RadianPerSecondSquaredContainer(RadianPerSecondSquaredContainer other) : base(other) { }

            #endregion
        }


        /// <summary>
        /// SecondContainer
        /// </summary>
        public class SecondContainer : Float32Container, ICloneable
        {
            /// <summary>
            /// Default constructor.
            /// </summary>
            public SecondContainer() : base() { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new SecondContainer(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>SecondContainer</returns>
            public new SecondContainer Clone()
            {
                return (SecondContainer)((ICloneable)this).Clone();
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">SecondContainer</param>
            protected SecondContainer(SecondContainer other) : base(other) { }

            #endregion
        }


        /// <summary>
        /// SquareMeterContainer
        /// </summary>
        public class SquareMeterContainer : Float32Container, ICloneable
        {
            /// <summary>
            /// Default constructor.
            /// </summary>
            public SquareMeterContainer() : base() { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new SquareMeterContainer(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>SquareMeterContainer</returns>
            public new SquareMeterContainer Clone()
            {
                return (SquareMeterContainer)((ICloneable)this).Clone();
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">SquareMeterContainer</param>
            protected SquareMeterContainer(SquareMeterContainer other) : base(other) { }

            #endregion
        }


        /// <summary>
        /// SteradianContainer
        /// </summary>
        public class SteradianContainer : Float32Container, ICloneable
        {
            /// <summary>
            /// Default constructor.
            /// </summary>
            public SteradianContainer() : base() { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new SteradianContainer(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>SteradianContainer</returns>
            public new SteradianContainer Clone()
            {
                return (SteradianContainer)((ICloneable)this).Clone();
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">SteradianContainer</param>
            protected SteradianContainer(SteradianContainer other) : base(other) { }

            #endregion
        }


        /// <summary>
        /// VoltContainer
        /// </summary>
        public class VoltContainer : Float32Container, ICloneable
        {
            /// <summary>
            /// Default constructor.
            /// </summary>
            public VoltContainer() : base() { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new VoltContainer(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>VoltContainer</returns>
            public new VoltContainer Clone()
            {
                return (VoltContainer)((ICloneable)this).Clone();
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">VoltContainer</param>
            protected VoltContainer(VoltContainer other) : base(other) { }

            #endregion
        }


        /// <summary>
        /// WattContainer
        /// </summary>
        public class WattContainer : Float32Container, ICloneable
        {
            /// <summary>
            /// Default constructor.
            /// </summary>
            public WattContainer() : base() { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new WattContainer(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>WattContainer</returns>
            public new WattContainer Clone()
            {
                return (WattContainer)((ICloneable)this).Clone();
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">WattContainer</param>
            protected WattContainer(WattContainer other) : base(other) { }

            #endregion
        }
    }


    //--------------------------------------------------
    // SI-types (64-bits)
    //--------------------------------------------------
    namespace Si64
    {

        /// <summary>
        /// AmpereContainer
        /// </summary>
        public class AmpereContainer : Float64Container, ICloneable
        {
            /// <summary>
            /// Default constructor.
            /// </summary>
            public AmpereContainer() : base() { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new AmpereContainer(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>AmpereContainer</returns>
            public new AmpereContainer Clone()
            {
                return (AmpereContainer)((ICloneable)this).Clone();
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">AmpereContainer</param>
            protected AmpereContainer(AmpereContainer other) : base(other) { }

            #endregion
        }


        /// <summary>
        /// CubicMeterContainer
        /// </summary>
        public class CubicMeterContainer : Float64Container, ICloneable
        {
            /// <summary>
            /// Default constructor.
            /// </summary>
            public CubicMeterContainer() : base() { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new CubicMeterContainer(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>CubicMeterContainer</returns>
            public new CubicMeterContainer Clone()
            {
                return (CubicMeterContainer)((ICloneable)this).Clone();
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">CubicMeterContainer</param>
            protected CubicMeterContainer(CubicMeterContainer other) : base(other) { }

            #endregion
        }


        /// <summary>
        /// HertzContainer
        /// </summary>
        public class HertzContainer : Float64Container, ICloneable
        {
            /// <summary>
            /// Default constructor.
            /// </summary>
            public HertzContainer() : base() { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new HertzContainer(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>HertzContainer</returns>
            public new HertzContainer Clone()
            {
                return (HertzContainer)((ICloneable)this).Clone();
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">HertzContainer</param>
            protected HertzContainer(HertzContainer other) : base(other) { }

            #endregion
        }


        /// <summary>
        /// JouleContainer
        /// </summary>
        public class JouleContainer : Float64Container, ICloneable
        {
            /// <summary>
            /// Default constructor.
            /// </summary>
            public JouleContainer() : base() { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new JouleContainer(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>JouleContainer</returns>
            public new JouleContainer Clone()
            {
                return (JouleContainer)((ICloneable)this).Clone();
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">JouleContainer</param>
            protected JouleContainer(JouleContainer other) : base(other) { }

            #endregion
        }


        /// <summary>
        /// KelvinContainer
        /// </summary>
        public class KelvinContainer : Float64Container, ICloneable
        {
            /// <summary>
            /// Default constructor.
            /// </summary>
            public KelvinContainer() : base() { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new KelvinContainer(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>KelvinContainer</returns>
            public new KelvinContainer Clone()
            {
                return (KelvinContainer)((ICloneable)this).Clone();
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">KelvinContainer</param>
            protected KelvinContainer(KelvinContainer other) : base(other) { }

            #endregion
        }


        /// <summary>
        /// KilogramContainer
        /// </summary>
        public class KilogramContainer : Float64Container, ICloneable
        {
            /// <summary>
            /// Default constructor.
            /// </summary>
            public KilogramContainer() : base() { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new KilogramContainer(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>KilogramContainer</returns>
            public new KilogramContainer Clone()
            {
                return (KilogramContainer)((ICloneable)this).Clone();
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">KilogramContainer</param>
            protected KilogramContainer(KilogramContainer other) : base(other) { }

            #endregion
        }


        /// <summary>
        /// MeterContainer
        /// </summary>
        public class MeterContainer : Float64Container, ICloneable
        {
            /// <summary>
            /// Default constructor.
            /// </summary>
            public MeterContainer() : base() { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new MeterContainer(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>MeterContainer</returns>
            public new MeterContainer Clone()
            {
                return (MeterContainer)((ICloneable)this).Clone();
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">MeterContainer</param>
            protected MeterContainer(MeterContainer other) : base(other) { }

            #endregion
        }


        /// <summary>
        /// MeterPerSecondContainer
        /// </summary>
        public class MeterPerSecondContainer : Float64Container, ICloneable
        {
            /// <summary>
            /// Default constructor.
            /// </summary>
            public MeterPerSecondContainer() : base() { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new MeterPerSecondContainer(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>MeterPerSecondContainer</returns>
            public new MeterPerSecondContainer Clone()
            {
                return (MeterPerSecondContainer)((ICloneable)this).Clone();
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">MeterPerSecondContainer</param>
            protected MeterPerSecondContainer(MeterPerSecondContainer other) : base(other) { }

            #endregion
        }


        /// <summary>
        /// MeterPerSecondSquaredContainer
        /// </summary>
        public class MeterPerSecondSquaredContainer : Float64Container, ICloneable
        {
            /// <summary>
            /// Default constructor.
            /// </summary>
            public MeterPerSecondSquaredContainer() : base() { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new MeterPerSecondSquaredContainer(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>MeterPerSecondSquaredContainer</returns>
            public new MeterPerSecondSquaredContainer Clone()
            {
                return (MeterPerSecondSquaredContainer)((ICloneable)this).Clone();
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">MeterPerSecondSquaredContainer</param>
            protected MeterPerSecondSquaredContainer(MeterPerSecondSquaredContainer other) : base(other) { }

            #endregion
        }


        /// <summary>
        /// NewtonContainer
        /// </summary>
        public class NewtonContainer : Float64Container, ICloneable
        {
            /// <summary>
            /// Default constructor.
            /// </summary>
            public NewtonContainer() : base() { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new NewtonContainer(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>NewtonContainer</returns>
            public new NewtonContainer Clone()
            {
                return (NewtonContainer)((ICloneable)this).Clone();
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">NewtonContainer</param>
            protected NewtonContainer(NewtonContainer other) : base(other) { }

            #endregion
        }


        /// <summary>
        /// PascalContainer
        /// </summary>
        public class PascalContainer : Float64Container, ICloneable
        {
            /// <summary>
            /// Default constructor.
            /// </summary>
            public PascalContainer() : base() { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new PascalContainer(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>PascalContainer</returns>
            public new PascalContainer Clone()
            {
                return (PascalContainer)((ICloneable)this).Clone();
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">PascalContainer</param>
            protected PascalContainer(PascalContainer other) : base(other) { }

            #endregion
        }


        /// <summary>
        /// RadianContainer
        /// </summary>
        public class RadianContainer : Float64Container, ICloneable
        {
            /// <summary>
            /// Default constructor.
            /// </summary>
            public RadianContainer() : base() { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new RadianContainer(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>RadianContainer</returns>
            public new RadianContainer Clone()
            {
                return (RadianContainer)((ICloneable)this).Clone();
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">RadianContainer</param>
            protected RadianContainer(RadianContainer other) : base(other) { }

            #endregion
        }


        /// <summary>
        /// RadianPerSecondContainer
        /// </summary>
        public class RadianPerSecondContainer : Float64Container, ICloneable
        {
            /// <summary>
            /// Default constructor.
            /// </summary>
            public RadianPerSecondContainer() : base() { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new RadianPerSecondContainer(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>RadianPerSecondContainer</returns>
            public new RadianPerSecondContainer Clone()
            {
                return (RadianPerSecondContainer)((ICloneable)this).Clone();
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">RadianPerSecondContainer</param>
            protected RadianPerSecondContainer(RadianPerSecondContainer other) : base(other) { }

            #endregion
        }


        /// <summary>
        /// RadianPerSecondSquaredContainer
        /// </summary>
        public class RadianPerSecondSquaredContainer : Float64Container, ICloneable
        {
            /// <summary>
            /// Default constructor.
            /// </summary>
            public RadianPerSecondSquaredContainer() : base() { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new RadianPerSecondSquaredContainer(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>RadianPerSecondSquaredContainer</returns>
            public new RadianPerSecondSquaredContainer Clone()
            {
                return (RadianPerSecondSquaredContainer)((ICloneable)this).Clone();
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">RadianPerSecondSquaredContainer</param>
            protected RadianPerSecondSquaredContainer(RadianPerSecondSquaredContainer other) : base(other) { }

            #endregion
        }


        /// <summary>
        /// SecondContainer
        /// </summary>
        public class SecondContainer : Float64Container, ICloneable
        {
            /// <summary>
            /// Default constructor.
            /// </summary>
            public SecondContainer() : base() { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new SecondContainer(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>SecondContainer</returns>
            public new SecondContainer Clone()
            {
                return (SecondContainer)((ICloneable)this).Clone();
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">SecondContainer</param>
            protected SecondContainer(SecondContainer other) : base(other) { }

            #endregion
        }


        /// <summary>
        /// SquareMeterContainer
        /// </summary>
        public class SquareMeterContainer : Float64Container, ICloneable
        {
            /// <summary>
            /// Default constructor.
            /// </summary>
            public SquareMeterContainer() : base() { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new SquareMeterContainer(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>SquareMeterContainer</returns>
            public new SquareMeterContainer Clone()
            {
                return (SquareMeterContainer)((ICloneable)this).Clone();
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">SquareMeterContainer</param>
            protected SquareMeterContainer(SquareMeterContainer other) : base(other) { }

            #endregion
        }


        /// <summary>
        /// SteradianContainer
        /// </summary>
        public class SteradianContainer : Float64Container, ICloneable
        {
            /// <summary>
            /// Default constructor.
            /// </summary>
            public SteradianContainer() : base() { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new SteradianContainer(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>SteradianContainer</returns>
            public new SteradianContainer Clone()
            {
                return (SteradianContainer)((ICloneable)this).Clone();
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">SteradianContainer</param>
            protected SteradianContainer(SteradianContainer other) : base(other) { }

            #endregion
        }


        /// <summary>
        /// VoltContainer
        /// </summary>
        public class VoltContainer : Float64Container, ICloneable
        {
            /// <summary>
            /// Default constructor.
            /// </summary>
            public VoltContainer() : base() { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new VoltContainer(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>VoltContainer</returns>
            public new VoltContainer Clone()
            {
                return (VoltContainer)((ICloneable)this).Clone();
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">VoltContainer</param>
            protected VoltContainer(VoltContainer other) : base(other) { }

            #endregion
        }


        /// <summary>
        /// WattContainer
        /// </summary>
        public class WattContainer : Float64Container, ICloneable
        {
            /// <summary>
            /// Default constructor.
            /// </summary>
            public WattContainer() : base() { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new WattContainer(this);
            }

            /// <summary>
            /// Clone
            /// </summary>
            /// <returns>WattContainer</returns>
            public new WattContainer Clone()
            {
                return (WattContainer)((ICloneable)this).Clone();
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">WattContainer</param>
            protected WattContainer(WattContainer other) : base(other) { }

            #endregion
        }
    }

    #endregion

    #region Array Container definitions


    /// <summary>
    /// BooleanContainerArray
    /// </summary>
    public class BooleanContainerArray : ArrayContainer<BooleanContainer>, ICloneable
    {
        /// <summary>
        /// Constructor.
        /// </summary>
        /// <param name="size">Array size.</param>
        public BooleanContainerArray(System.Int32 size) : base(size) { }

        #region Cloning

        object ICloneable.Clone()
        {
            return new BooleanContainerArray(this);
        }

        /// <summary>
        /// Clone.
        /// </summary>
        /// <returns>BooleanContainerArray</returns>
        public new BooleanContainerArray Clone()
        {
            return (BooleanContainerArray)(((ICloneable)this).Clone());
        }

        /// <summary>
        /// Copy constructor.
        /// </summary>
        /// <param name="other">BooleanContainerArray</param>
        protected BooleanContainerArray(BooleanContainerArray other) : base(other) { }

        #endregion
    }

    
    /// <summary>
    /// Int32ContainerArray
    /// </summary>
    public class Int32ContainerArray : ArrayContainer<Int32Container>, ICloneable
    {
        /// <summary>
        /// Constructor.
        /// </summary>
        /// <param name="size">Array size.</param>
        public Int32ContainerArray(System.Int32 size) : base(size) { }

        #region Cloning

        object ICloneable.Clone()
        {
            return new Int32ContainerArray(this);
        }

        /// <summary>
        /// Clone.
        /// </summary>
        /// <returns>Int32ContainerArray</returns>
        public new Int32ContainerArray Clone()
        {
            return (Int32ContainerArray)(((ICloneable)this).Clone());
        }

        /// <summary>
        /// Copy constructor.
        /// </summary>
        /// <param name="other">Int32ContainerArray</param>
        protected Int32ContainerArray(Int32ContainerArray other) : base(other) { }

        #endregion
    }

    
    /// <summary>
    /// Int64ContainerArray
    /// </summary>
    public class Int64ContainerArray : ArrayContainer<Int64Container>, ICloneable
    {
        /// <summary>
        /// Constructor.
        /// </summary>
        /// <param name="size">Array size.</param>
        public Int64ContainerArray(System.Int32 size) : base(size) { }

        #region Cloning

        object ICloneable.Clone()
        {
            return new Int64ContainerArray(this);
        }

        /// <summary>
        /// Clone.
        /// </summary>
        /// <returns>Int64ContainerArray</returns>
        public new Int64ContainerArray Clone()
        {
            return (Int64ContainerArray)(((ICloneable)this).Clone());
        }

        /// <summary>
        /// Copy constructor.
        /// </summary>
        /// <param name="other">Int64ContainerArray</param>
        protected Int64ContainerArray(Int64ContainerArray other) : base(other) { }

        #endregion
    }

    
    /// <summary>
    /// Float32ContainerArray
    /// </summary>
    public class Float32ContainerArray : ArrayContainer<Float32Container>, ICloneable
    {
        /// <summary>
        /// Constructor.
        /// </summary>
        /// <param name="size">Array size.</param>
        public Float32ContainerArray(System.Int32 size) : base(size) { }

        #region Cloning

        object ICloneable.Clone()
        {
            return new Float32ContainerArray(this);
        }

        /// <summary>
        /// Clone.
        /// </summary>
        /// <returns>Float32ContainerArray</returns>
        public new Float32ContainerArray Clone()
        {
            return (Float32ContainerArray)(((ICloneable)this).Clone());
        }

        /// <summary>
        /// Copy constructor.
        /// </summary>
        /// <param name="other">Float32ContainerArray</param>
        protected Float32ContainerArray(Float32ContainerArray other) : base(other) { }

        #endregion
    }

    
    /// <summary>
    /// Float64ContainerArray
    /// </summary>
    public class Float64ContainerArray : ArrayContainer<Float64Container>, ICloneable
    {
        /// <summary>
        /// Constructor.
        /// </summary>
        /// <param name="size">Array size.</param>
        public Float64ContainerArray(System.Int32 size) : base(size) { }

        #region Cloning

        object ICloneable.Clone()
        {
            return new Float64ContainerArray(this);
        }

        /// <summary>
        /// Clone.
        /// </summary>
        /// <returns>Float64ContainerArray</returns>
        public new Float64ContainerArray Clone()
        {
            return (Float64ContainerArray)(((ICloneable)this).Clone());
        }

        /// <summary>
        /// Copy constructor.
        /// </summary>
        /// <param name="other">Float64ContainerArray</param>
        protected Float64ContainerArray(Float64ContainerArray other) : base(other) { }

        #endregion
    }

    /// <summary>
    /// TypeIdContainerArray
    /// </summary>
    public class TypeIdContainerArray : ArrayContainer<TypeIdContainer>, ICloneable
    {
        /// <summary>
        /// Constructor.
        /// <para/>
        /// Creates an array of the given size. Remember that once it has been created the size cannot be changed.
        /// 
        /// </summary>
        /// <param name="size">The desired size of the array. Must be > 0.</param>
        public TypeIdContainerArray(System.Int32 size) : base(size) { }

        #region Cloning

        object ICloneable.Clone()
        {
            return new TypeIdContainerArray(this);
        }

        /// <summary>
        /// Clone.
        /// </summary>
        /// <returns></returns>
        public new TypeIdContainerArray Clone()
        {
            return (TypeIdContainerArray)(((ICloneable)this).Clone());
        }

        /// <summary>
        /// Copy constructor for use by Clone
        /// </summary>
        /// <param name="other"></param>
        protected TypeIdContainerArray(TypeIdContainerArray other) : base(other) { }

        #endregion
    }

    
    /// <summary>
    /// InstanceIdContainerArray
    /// </summary>
    public class InstanceIdContainerArray : ArrayContainer<InstanceIdContainer>, ICloneable
    {
        /// <summary>
        /// Constructor.
        /// </summary>
        /// <param name="size">Array size.</param>
        public InstanceIdContainerArray(System.Int32 size) : base(size) { }

        #region Cloning

        object ICloneable.Clone()
        {
            return new InstanceIdContainerArray(this);
        }

        /// <summary>
        /// Clone.
        /// </summary>
        /// <returns>InstanceIdContainerArray</returns>
        public new InstanceIdContainerArray Clone()
        {
            return (InstanceIdContainerArray)(((ICloneable)this).Clone());
        }

        /// <summary>
        /// Copy constructor.
        /// </summary>
        /// <param name="other">InstanceIdContainerArray</param>
        protected InstanceIdContainerArray(InstanceIdContainerArray other) : base(other) { }

        #endregion
    }

    
    /// <summary>
    /// EntityIdContainerArray
    /// </summary>
    public class EntityIdContainerArray : ArrayContainer<EntityIdContainer>, ICloneable
    {
        /// <summary>
        /// Constructor.
        /// </summary>
        /// <param name="size">Array size.</param>
        public EntityIdContainerArray(System.Int32 size) : base(size) { }

        #region Cloning

        object ICloneable.Clone()
        {
            return new EntityIdContainerArray(this);
        }

        /// <summary>
        /// Clone.
        /// </summary>
        /// <returns>EntityIdContainerArray</returns>
        public new EntityIdContainerArray Clone()
        {
            return (EntityIdContainerArray)(((ICloneable)this).Clone());
        }

        /// <summary>
        /// Copy constructor.
        /// </summary>
        /// <param name="other">EntityIdContainerArray</param>
        protected EntityIdContainerArray(EntityIdContainerArray other) : base(other) { }

        #endregion
    }

    
    /// <summary>
    /// ChannelIdContainerArray
    /// </summary>
    public class ChannelIdContainerArray : ArrayContainer<ChannelIdContainer>, ICloneable
    {
        /// <summary>
        /// Constructor.
        /// </summary>
        /// <param name="size">Array size.</param>
        public ChannelIdContainerArray(System.Int32 size) : base(size) { }

        #region Cloning

        object ICloneable.Clone()
        {
            return new ChannelIdContainerArray(this);
        }

        /// <summary>
        /// Clone.
        /// </summary>
        /// <returns>ChannelIdContainerArray</returns>
        public new ChannelIdContainerArray Clone()
        {
            return (ChannelIdContainerArray)(((ICloneable)this).Clone());
        }

        /// <summary>
        /// Copy constructor.
        /// </summary>
        /// <param name="other">ChannelIdContainerArray</param>
        protected ChannelIdContainerArray(ChannelIdContainerArray other) : base(other) { }

        #endregion
    }

    
    /// <summary>
    /// HandlerIdContainerArray
    /// </summary>
    public class HandlerIdContainerArray : ArrayContainer<HandlerIdContainer>, ICloneable
    {
        /// <summary>
        /// Constructor.
        /// </summary>
        /// <param name="size">Array size.</param>
        public HandlerIdContainerArray(System.Int32 size) : base(size) { }

        #region Cloning

        object ICloneable.Clone()
        {
            return new HandlerIdContainerArray(this);
        }

        /// <summary>
        /// Clone.
        /// </summary>
        /// <returns>HandlerIdContainerArray</returns>
        public new HandlerIdContainerArray Clone()
        {
            return (HandlerIdContainerArray)(((ICloneable)this).Clone());
        }

        /// <summary>
        /// Copy constructor.
        /// </summary>
        /// <param name="other">HandlerIdContainerArray</param>
        protected HandlerIdContainerArray(HandlerIdContainerArray other) : base(other) { }

        #endregion
    }

    /// <summary>
    /// ObjectContainerArray
    /// </summary>
    public class ObjectContainerArray : Safir.Dob.Typesystem.ArrayContainer<ObjectContainer>, ICloneable
    {
        /// <summary>
        /// Constructor.
        /// </summary>
        /// <param name="size">Array size.</param>
        public ObjectContainerArray(System.Int32 size) : base(size) { }

        #region Cloning

        object ICloneable.Clone()
        {
            return new ObjectContainerArray(this);
        }

        /// <summary>
        /// Clone.
        /// </summary>
        /// <returns>ObjectContainerArray</returns>
        public new ObjectContainerArray Clone()
        {
            return (ObjectContainerArray)(((ICloneable)this).Clone());
        }

        /// <summary>
        /// Copy constructor.
        /// </summary>
        /// <param name="other">ObjectContainerArray</param>
        protected ObjectContainerArray(ObjectContainerArray other) : base(other) { }

        #endregion
    }

    /// <summary>
    /// StringContainerArray
    /// </summary>
    public class StringContainerArray : Safir.Dob.Typesystem.ArrayContainer<StringContainer>, ICloneable
    {
        /// <summary>
        /// Constructor with size.
        /// </summary>
        /// <param name="size">Array size.</param>
        public StringContainerArray(System.Int32 size) : base(size) { }

        #region Cloning

        object ICloneable.Clone()
        {
            return new StringContainerArray(this);
        }

        /// <summary>
        /// Clone.
        /// </summary>
        /// <returns></returns>
        public new StringContainerArray Clone()
        {
            return (StringContainerArray)(((ICloneable)this).Clone());
        }

        /// <summary>
        /// Copy constructor for use by Clone
        /// </summary>
        /// <param name="other"></param>
        protected StringContainerArray(StringContainerArray other) : base(other) { }

        #endregion
    }

    /// <summary>
    /// BinaryContainerArray
    /// </summary>
    public class BinaryContainerArray : Safir.Dob.Typesystem.ArrayContainer<BinaryContainer>, ICloneable
    {
        /// <summary>
        /// Constructor.
        /// </summary>
        /// <param name="size">Array size.</param>
        public BinaryContainerArray(System.Int32 size) : base(size) { }

        #region Cloning

        object ICloneable.Clone()
        {
            return new BinaryContainerArray(this);
        }

        /// <summary>
        /// Clone.
        /// </summary>
        /// <returns>BinaryContainerArray</returns>
        public new BinaryContainerArray Clone()
        {
            return (BinaryContainerArray)(((ICloneable)this).Clone());
        }

        /// <summary>
        /// Copy constructor.
        /// </summary>
        /// <param name="other">BinaryContainerArray</param>
        protected BinaryContainerArray(BinaryContainerArray other) : base(other) { }

        #endregion
    }

    //--------------------------------------------------
    // SI-types (32-bits)
    //--------------------------------------------------
    namespace Si32
    {

        
        /// <summary>
        /// AmpereContainerArray
        /// </summary>
        public class AmpereContainerArray : ArrayContainer<AmpereContainer>, ICloneable
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public AmpereContainerArray(System.Int32 size) : base(size) { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new AmpereContainerArray(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>AmpereContainerArray</returns>
            public new AmpereContainerArray Clone()
            {
                return (AmpereContainerArray)(((ICloneable)this).Clone());
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">AmpereContainerArray</param>
            protected AmpereContainerArray(AmpereContainerArray other) : base(other) { }

            #endregion
        }


        
        /// <summary>
        /// CubicMeterContainerArray
        /// </summary>
        public class CubicMeterContainerArray : ArrayContainer<CubicMeterContainer>, ICloneable
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public CubicMeterContainerArray(System.Int32 size) : base(size) { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new CubicMeterContainerArray(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>CubicMeterContainerArray</returns>
            public new CubicMeterContainerArray Clone()
            {
                return (CubicMeterContainerArray)(((ICloneable)this).Clone());
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">CubicMeterContainerArray</param>
            protected CubicMeterContainerArray(CubicMeterContainerArray other) : base(other) { }

            #endregion
        }


        
        /// <summary>
        /// HertzContainerArray
        /// </summary>
        public class HertzContainerArray : ArrayContainer<HertzContainer>, ICloneable
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public HertzContainerArray(System.Int32 size) : base(size) { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new HertzContainerArray(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>HertzContainerArray</returns>
            public new HertzContainerArray Clone()
            {
                return (HertzContainerArray)(((ICloneable)this).Clone());
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">ay</param>
            protected HertzContainerArray(HertzContainerArray other) : base(other) { }

            #endregion
        }


        
        /// <summary>
        /// JouleContainerArray
        /// </summary>
        public class JouleContainerArray : ArrayContainer<JouleContainer>, ICloneable
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public JouleContainerArray(System.Int32 size) : base(size) { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new JouleContainerArray(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>JouleContainerArray</returns>
            public new JouleContainerArray Clone()
            {
                return (JouleContainerArray)(((ICloneable)this).Clone());
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">JouleContainerArray</param>
            protected JouleContainerArray(JouleContainerArray other) : base(other) { }

            #endregion
        }


        
        /// <summary>
        /// KelvinContainerArray
        /// </summary>
        public class KelvinContainerArray : ArrayContainer<KelvinContainer>, ICloneable
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public KelvinContainerArray(System.Int32 size) : base(size) { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new KelvinContainerArray(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>KelvinContainerArray</returns>
            public new KelvinContainerArray Clone()
            {
                return (KelvinContainerArray)(((ICloneable)this).Clone());
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">KelvinContainerArray</param>
            protected KelvinContainerArray(KelvinContainerArray other) : base(other) { }

            #endregion
        }


        
        /// <summary>
        /// KilogramContainerArray
        /// </summary>
        public class KilogramContainerArray : ArrayContainer<KilogramContainer>, ICloneable
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public KilogramContainerArray(System.Int32 size) : base(size) { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new KilogramContainerArray(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>KilogramContainerArray</returns>
            public new KilogramContainerArray Clone()
            {
                return (KilogramContainerArray)(((ICloneable)this).Clone());
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">KilogramContainerArray</param>
            protected KilogramContainerArray(KilogramContainerArray other) : base(other) { }

            #endregion
        }


        
        /// <summary>
        /// MeterContainerArray
        /// </summary>
        public class MeterContainerArray : ArrayContainer<MeterContainer>, ICloneable
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public MeterContainerArray(System.Int32 size) : base(size) { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new MeterContainerArray(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>MeterContainerArray</returns>
            public new MeterContainerArray Clone()
            {
                return (MeterContainerArray)(((ICloneable)this).Clone());
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">MeterContainerArray</param>
            protected MeterContainerArray(MeterContainerArray other) : base(other) { }

            #endregion
        }


        
        /// <summary>
        /// MeterPerSecondContainerArray
        /// </summary>
        public class MeterPerSecondContainerArray : ArrayContainer<MeterPerSecondContainer>, ICloneable
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public MeterPerSecondContainerArray(System.Int32 size) : base(size) { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new MeterPerSecondContainerArray(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>MeterPerSecondContainerArray</returns>
            public new MeterPerSecondContainerArray Clone()
            {
                return (MeterPerSecondContainerArray)(((ICloneable)this).Clone());
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">MeterPerSecondContainerArray</param>
            protected MeterPerSecondContainerArray(MeterPerSecondContainerArray other) : base(other) { }

            #endregion
        }


        
        /// <summary>
        /// MeterPerSecondSquaredContainerArray
        /// </summary>
        public class MeterPerSecondSquaredContainerArray : ArrayContainer<MeterPerSecondSquaredContainer>, ICloneable
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public MeterPerSecondSquaredContainerArray(System.Int32 size) : base(size) { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new MeterPerSecondSquaredContainerArray(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>MeterPerSecondSquaredContainerArray</returns>
            public new MeterPerSecondSquaredContainerArray Clone()
            {
                return (MeterPerSecondSquaredContainerArray)(((ICloneable)this).Clone());
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">MeterPerSecondSquaredContainerArray</param>
            protected MeterPerSecondSquaredContainerArray(MeterPerSecondSquaredContainerArray other) : base(other) { }

            #endregion
        }


        
        /// <summary>
        /// NewtonContainerArray
        /// </summary>
        public class NewtonContainerArray : ArrayContainer<NewtonContainer>, ICloneable
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public NewtonContainerArray(System.Int32 size) : base(size) { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new NewtonContainerArray(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>NewtonContainerArray</returns>
            public new NewtonContainerArray Clone()
            {
                return (NewtonContainerArray)(((ICloneable)this).Clone());
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">NewtonContainerArray</param>
            protected NewtonContainerArray(NewtonContainerArray other) : base(other) { }

            #endregion
        }


        
        /// <summary>
        /// PascalContainerArray
        /// </summary>
        public class PascalContainerArray : ArrayContainer<PascalContainer>, ICloneable
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public PascalContainerArray(System.Int32 size) : base(size) { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new PascalContainerArray(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>PascalContainerArray</returns>
            public new PascalContainerArray Clone()
            {
                return (PascalContainerArray)(((ICloneable)this).Clone());
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">PascalContainerArray</param>
            protected PascalContainerArray(PascalContainerArray other) : base(other) { }

            #endregion
        }


        
        /// <summary>
        /// RadianContainerArray
        /// </summary>
        public class RadianContainerArray : ArrayContainer<RadianContainer>, ICloneable
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public RadianContainerArray(System.Int32 size) : base(size) { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new RadianContainerArray(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>RadianContainerArray</returns>
            public new RadianContainerArray Clone()
            {
                return (RadianContainerArray)(((ICloneable)this).Clone());
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">RadianContainerArray</param>
            protected RadianContainerArray(RadianContainerArray other) : base(other) { }

            #endregion
        }


        
        /// <summary>
        /// RadianPerSecondContainerArray
        /// </summary>
        public class RadianPerSecondContainerArray : ArrayContainer<RadianPerSecondContainer>, ICloneable
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public RadianPerSecondContainerArray(System.Int32 size) : base(size) { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new RadianPerSecondContainerArray(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>RadianPerSecondContainerArray</returns>
            public new RadianPerSecondContainerArray Clone()
            {
                return (RadianPerSecondContainerArray)(((ICloneable)this).Clone());
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">RadianPerSecondContainerArray</param>
            protected RadianPerSecondContainerArray(RadianPerSecondContainerArray other) : base(other) { }

            #endregion
        }


        
        /// <summary>
        /// RadianPerSecondSquaredContainerArray
        /// </summary>
        public class RadianPerSecondSquaredContainerArray : ArrayContainer<RadianPerSecondSquaredContainer>, ICloneable
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public RadianPerSecondSquaredContainerArray(System.Int32 size) : base(size) { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new RadianPerSecondSquaredContainerArray(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>RadianPerSecondSquaredContainerArray</returns>
            public new RadianPerSecondSquaredContainerArray Clone()
            {
                return (RadianPerSecondSquaredContainerArray)(((ICloneable)this).Clone());
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">RadianPerSecondSquaredContainerArray</param>
            protected RadianPerSecondSquaredContainerArray(RadianPerSecondSquaredContainerArray other) : base(other) { }

            #endregion
        }


        
        /// <summary>
        /// SecondContainerArray
        /// </summary>
        public class SecondContainerArray : ArrayContainer<SecondContainer>, ICloneable
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public SecondContainerArray(System.Int32 size) : base(size) { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new SecondContainerArray(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>SecondContainerArray</returns>
            public new SecondContainerArray Clone()
            {
                return (SecondContainerArray)(((ICloneable)this).Clone());
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">SecondContainerArray</param>
            protected SecondContainerArray(SecondContainerArray other) : base(other) { }

            #endregion
        }


        
        /// <summary>
        /// SquareMeterContainerArray
        /// </summary>
        public class SquareMeterContainerArray : ArrayContainer<SquareMeterContainer>, ICloneable
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public SquareMeterContainerArray(System.Int32 size) : base(size) { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new SquareMeterContainerArray(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>SquareMeterContainerArray</returns>
            public new SquareMeterContainerArray Clone()
            {
                return (SquareMeterContainerArray)(((ICloneable)this).Clone());
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">SquareMeterContainerArray</param>
            protected SquareMeterContainerArray(SquareMeterContainerArray other) : base(other) { }

            #endregion
        }


        
        /// <summary>
        /// SteradianContainerArray
        /// </summary>
        public class SteradianContainerArray : ArrayContainer<SteradianContainer>, ICloneable
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public SteradianContainerArray(System.Int32 size) : base(size) { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new SteradianContainerArray(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>SteradianContainerArray</returns>
            public new SteradianContainerArray Clone()
            {
                return (SteradianContainerArray)(((ICloneable)this).Clone());
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">SteradianContainerArray</param>
            protected SteradianContainerArray(SteradianContainerArray other) : base(other) { }

            #endregion
        }


        
        /// <summary>
        /// VoltContainerArray
        /// </summary>
        public class VoltContainerArray : ArrayContainer<VoltContainer>, ICloneable
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public VoltContainerArray(System.Int32 size) : base(size) { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new VoltContainerArray(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>VoltContainerArray</returns>
            public new VoltContainerArray Clone()
            {
                return (VoltContainerArray)(((ICloneable)this).Clone());
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">VoltContainerArray</param>
            protected VoltContainerArray(VoltContainerArray other) : base(other) { }

            #endregion
        }


        
        /// <summary>
        /// WattContainerArray
        /// </summary>
        public class WattContainerArray : ArrayContainer<WattContainer>, ICloneable
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public WattContainerArray(System.Int32 size) : base(size) { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new WattContainerArray(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>WattContainerArray</returns>
            public new WattContainerArray Clone()
            {
                return (WattContainerArray)(((ICloneable)this).Clone());
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">WattContainerArray</param>
            protected WattContainerArray(WattContainerArray other) : base(other) { }

            #endregion
        }
    }


    //--------------------------------------------------
    // SI-types (64-bits)
    //--------------------------------------------------
    namespace Si64
    {

        
        /// <summary>
        /// AmpereContainerArray
        /// </summary>
        public class AmpereContainerArray : ArrayContainer<AmpereContainer>, ICloneable
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public AmpereContainerArray(System.Int32 size) : base(size) { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new AmpereContainerArray(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>AmpereContainerArray</returns>
            public new AmpereContainerArray Clone()
            {
                return (AmpereContainerArray)(((ICloneable)this).Clone());
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">AmpereContainerArray</param>
            protected AmpereContainerArray(AmpereContainerArray other) : base(other) { }

            #endregion
        }


        
        /// <summary>
        /// CubicMeterContainerArray
        /// </summary>
        public class CubicMeterContainerArray : ArrayContainer<CubicMeterContainer>, ICloneable
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public CubicMeterContainerArray(System.Int32 size) : base(size) { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new CubicMeterContainerArray(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>CubicMeterContainerArray</returns>
            public new CubicMeterContainerArray Clone()
            {
                return (CubicMeterContainerArray)(((ICloneable)this).Clone());
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">CubicMeterContainerArray</param>
            protected CubicMeterContainerArray(CubicMeterContainerArray other) : base(other) { }

            #endregion
        }


        
        /// <summary>
        /// HertzContainerArray
        /// </summary>
        public class HertzContainerArray : ArrayContainer<HertzContainer>, ICloneable
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public HertzContainerArray(System.Int32 size) : base(size) { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new HertzContainerArray(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>HertzContainerArray</returns>
            public new HertzContainerArray Clone()
            {
                return (HertzContainerArray)(((ICloneable)this).Clone());
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">HertzContainerArray</param>
            protected HertzContainerArray(HertzContainerArray other) : base(other) { }

            #endregion
        }


        
        /// <summary>
        /// JouleContainerArray
        /// </summary>
        public class JouleContainerArray : ArrayContainer<JouleContainer>, ICloneable
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public JouleContainerArray(System.Int32 size) : base(size) { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new JouleContainerArray(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>JouleContainerArray</returns>
            public new JouleContainerArray Clone()
            {
                return (JouleContainerArray)(((ICloneable)this).Clone());
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">JouleContainerArray</param>
            protected JouleContainerArray(JouleContainerArray other) : base(other) { }

            #endregion
        }


        
        /// <summary>
        /// KelvinContainerArray
        /// </summary>
        public class KelvinContainerArray : ArrayContainer<KelvinContainer>, ICloneable
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public KelvinContainerArray(System.Int32 size) : base(size) { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new KelvinContainerArray(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>KelvinContainerArray</returns>
            public new KelvinContainerArray Clone()
            {
                return (KelvinContainerArray)(((ICloneable)this).Clone());
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">KelvinContainerArray</param>
            protected KelvinContainerArray(KelvinContainerArray other) : base(other) { }

            #endregion
        }


        
        /// <summary>
        /// KilogramContainerArray
        /// </summary>
        public class KilogramContainerArray : ArrayContainer<KilogramContainer>, ICloneable
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public KilogramContainerArray(System.Int32 size) : base(size) { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new KilogramContainerArray(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>KilogramContainerArray</returns>
            public new KilogramContainerArray Clone()
            {
                return (KilogramContainerArray)(((ICloneable)this).Clone());
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">KilogramContainerArray</param>
            protected KilogramContainerArray(KilogramContainerArray other) : base(other) { }

            #endregion
        }


        
        /// <summary>
        /// MeterContainerArray
        /// </summary>
        public class MeterContainerArray : ArrayContainer<MeterContainer>, ICloneable
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public MeterContainerArray(System.Int32 size) : base(size) { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new MeterContainerArray(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>MeterContainerArray</returns>
            public new MeterContainerArray Clone()
            {
                return (MeterContainerArray)(((ICloneable)this).Clone());
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">MeterContainerArray</param>
            protected MeterContainerArray(MeterContainerArray other) : base(other) { }

            #endregion
        }


        
        /// <summary>
        /// MeterPerSecondContainerArray
        /// </summary>
        public class MeterPerSecondContainerArray : ArrayContainer<MeterPerSecondContainer>, ICloneable
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public MeterPerSecondContainerArray(System.Int32 size) : base(size) { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new MeterPerSecondContainerArray(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>MeterPerSecondContainerArray</returns>
            public new MeterPerSecondContainerArray Clone()
            {
                return (MeterPerSecondContainerArray)(((ICloneable)this).Clone());
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">MeterPerSecondContainerArray</param>
            protected MeterPerSecondContainerArray(MeterPerSecondContainerArray other) : base(other) { }

            #endregion
        }


        
        /// <summary>
        /// MeterPerSecondSquaredContainerArray
        /// </summary>
        public class MeterPerSecondSquaredContainerArray : ArrayContainer<MeterPerSecondSquaredContainer>, ICloneable
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public MeterPerSecondSquaredContainerArray(System.Int32 size) : base(size) { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new MeterPerSecondSquaredContainerArray(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>MeterPerSecondSquaredContainerArray</returns>
            public new MeterPerSecondSquaredContainerArray Clone()
            {
                return (MeterPerSecondSquaredContainerArray)(((ICloneable)this).Clone());
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">MeterPerSecondSquaredContainerArray</param>
            protected MeterPerSecondSquaredContainerArray(MeterPerSecondSquaredContainerArray other) : base(other) { }

            #endregion
        }


        
        /// <summary>
        /// NewtonContainerArray
        /// </summary>
        public class NewtonContainerArray : ArrayContainer<NewtonContainer>, ICloneable
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public NewtonContainerArray(System.Int32 size) : base(size) { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new NewtonContainerArray(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>NewtonContainerArray</returns>
            public new NewtonContainerArray Clone()
            {
                return (NewtonContainerArray)(((ICloneable)this).Clone());
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">NewtonContainerArray</param>
            protected NewtonContainerArray(NewtonContainerArray other) : base(other) { }

            #endregion
        }


        
        /// <summary>
        /// PascalContainerArray
        /// </summary>
        public class PascalContainerArray : ArrayContainer<PascalContainer>, ICloneable
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public PascalContainerArray(System.Int32 size) : base(size) { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new PascalContainerArray(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>PascalContainerArray</returns>
            public new PascalContainerArray Clone()
            {
                return (PascalContainerArray)(((ICloneable)this).Clone());
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">PascalContainerArray</param>
            protected PascalContainerArray(PascalContainerArray other) : base(other) { }

            #endregion
        }


        
        /// <summary>
        /// RadianContainerArray
        /// </summary>
        public class RadianContainerArray : ArrayContainer<RadianContainer>, ICloneable
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public RadianContainerArray(System.Int32 size) : base(size) { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new RadianContainerArray(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>RadianContainerArray</returns>
            public new RadianContainerArray Clone()
            {
                return (RadianContainerArray)(((ICloneable)this).Clone());
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">RadianContainerArray</param>
            protected RadianContainerArray(RadianContainerArray other) : base(other) { }

            #endregion
        }


        
        /// <summary>
        /// RadianPerSecondContainerArray
        /// </summary>
        public class RadianPerSecondContainerArray : ArrayContainer<RadianPerSecondContainer>, ICloneable
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public RadianPerSecondContainerArray(System.Int32 size) : base(size) { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new RadianPerSecondContainerArray(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>RadianPerSecondContainerArray</returns>
            public new RadianPerSecondContainerArray Clone()
            {
                return (RadianPerSecondContainerArray)(((ICloneable)this).Clone());
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">RadianPerSecondContainerArray</param>
            protected RadianPerSecondContainerArray(RadianPerSecondContainerArray other) : base(other) { }

            #endregion
        }


        
        /// <summary>
        /// RadianPerSecondSquaredContainerArray
        /// </summary>
        public class RadianPerSecondSquaredContainerArray : ArrayContainer<RadianPerSecondSquaredContainer>, ICloneable
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public RadianPerSecondSquaredContainerArray(System.Int32 size) : base(size) { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new RadianPerSecondSquaredContainerArray(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>RadianPerSecondSquaredContainerArray</returns>
            public new RadianPerSecondSquaredContainerArray Clone()
            {
                return (RadianPerSecondSquaredContainerArray)(((ICloneable)this).Clone());
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">RadianPerSecondSquaredContainerArray</param>
            protected RadianPerSecondSquaredContainerArray(RadianPerSecondSquaredContainerArray other) : base(other) { }

            #endregion
        }


        
        /// <summary>
        /// SecondContainerArray
        /// </summary>
        public class SecondContainerArray : ArrayContainer<SecondContainer>, ICloneable
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public SecondContainerArray(System.Int32 size) : base(size) { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new SecondContainerArray(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>SecondContainerArray</returns>
            public new SecondContainerArray Clone()
            {
                return (SecondContainerArray)(((ICloneable)this).Clone());
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">SecondContainerArray</param>
            protected SecondContainerArray(SecondContainerArray other) : base(other) { }

            #endregion
        }


        
        /// <summary>
        /// SquareMeterContainerArray
        /// </summary>
        public class SquareMeterContainerArray : ArrayContainer<SquareMeterContainer>, ICloneable
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public SquareMeterContainerArray(System.Int32 size) : base(size) { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new SquareMeterContainerArray(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>SquareMeterContainerArray</returns>
            public new SquareMeterContainerArray Clone()
            {
                return (SquareMeterContainerArray)(((ICloneable)this).Clone());
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">ontainerArray</param>
            protected SquareMeterContainerArray(SquareMeterContainerArray other) : base(other) { }

            #endregion
        }


        
        /// <summary>
        /// SteradianContainerArray
        /// </summary>
        public class SteradianContainerArray : ArrayContainer<SteradianContainer>, ICloneable
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public SteradianContainerArray(System.Int32 size) : base(size) { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new SteradianContainerArray(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>SteradianContainerArray</returns>
            public new SteradianContainerArray Clone()
            {
                return (SteradianContainerArray)(((ICloneable)this).Clone());
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">SteradianContainerArray</param>
            protected SteradianContainerArray(SteradianContainerArray other) : base(other) { }

            #endregion
        }


        
        /// <summary>
        /// VoltContainerArray
        /// </summary>
        public class VoltContainerArray : ArrayContainer<VoltContainer>, ICloneable
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public VoltContainerArray(System.Int32 size) : base(size) { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new VoltContainerArray(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>VoltContainerArray</returns>
            public new VoltContainerArray Clone()
            {
                return (VoltContainerArray)(((ICloneable)this).Clone());
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">VoltContainerArray</param>
            protected VoltContainerArray(VoltContainerArray other) : base(other) { }

            #endregion
        }


        
        /// <summary>
        /// WattContainerArray
        /// </summary>
        public class WattContainerArray : ArrayContainer<WattContainer>, ICloneable
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public WattContainerArray(System.Int32 size) : base(size) { }

            #region Cloning

            object ICloneable.Clone()
            {
                return new WattContainerArray(this);
            }

            /// <summary>
            /// Clone.
            /// </summary>
            /// <returns>WattContainerArray</returns>
            public new WattContainerArray Clone()
            {
                return (WattContainerArray)(((ICloneable)this).Clone());
            }

            /// <summary>
            /// Copy constructor.
            /// </summary>
            /// <param name="other">WattContainerArray</param>
            protected WattContainerArray(WattContainerArray other) : base(other) { }

            #endregion
        }
    }

    #endregion
}
