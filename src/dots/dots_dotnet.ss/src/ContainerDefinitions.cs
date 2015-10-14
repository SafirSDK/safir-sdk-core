/* ****************************************************************************
*
* Copyright Saab AB, 2005-2013 (http://safirsdkcore.com)
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

    /// <summary>BooleanContainer</summary>
    public class BooleanContainer : ValueContainer<bool> {}

    /// <summary>Int32Container</summary>
    public class Int32Container : ValueContainer<int> {}

    /// <summary>Int64Container</summary>
    public class Int64Container : ValueContainer<System.Int64> {}

    /// <summary>Float32Container</summary>
    public class Float32Container : ValueContainer<float> {}

    /// <summary>Float64Container</summary>
    public class Float64Container : ValueContainer<double> {}

    /// <summary>TypeIdContainer</summary>
    public class TypeIdContainer : Int64Container
    {
        /// <summary>
        /// Constructor
        /// </summary>
        public TypeIdContainer() : base() { }


    }

    /// <summary>InstanceIdContainer</summary>
    public class InstanceIdContainer : ValueContainer<InstanceId>
    {
        /// <summary>
        /// Default constructor.
        /// </summary>
        public InstanceIdContainer() : base() { m_Value = new InstanceId(); }


    }

    /// <summary>EntityIdContainer</summary>
    public class EntityIdContainer : ValueContainer<EntityId> {}

    /// <summary>ChannelIdContainer</summary>
    public class ChannelIdContainer : ValueContainer<ChannelId>
    {
        /// <summary>
        /// Default constructor.
        /// </summary>
        public ChannelIdContainer() : base() { m_Value = new ChannelId(); }


    }

    /// <summary>HandlerIdContainer</summary>
    public class HandlerIdContainer : ValueContainer<HandlerId>
    {
        /// <summary>
        /// Default constructor.
        /// </summary>
        public HandlerIdContainer() : base() { m_Value = new HandlerId(); }


    }

    /// <summary>ObjectContainer</summary>
    public class ObjectContainer : ObjectContainerImpl<Object> {}

    /// <summary>BinaryContainer</summary>
    public class BinaryContainer : ValueContainer<byte[]> {}

    //--------------------------------------------------
    // SI-types (32-bits)
    //--------------------------------------------------
    namespace Si32
    {
        /// <summary>AmpereContainer</summary>
        public class AmpereContainer : Float32Container {}

        /// <summary>CubicMeterContainer</summary>
        public class CubicMeterContainer : Float32Container {}

        /// <summary>HertzContainer</summary>
        public class HertzContainer : Float32Container {}

        /// <summary>JouleContainer</summary>
        public class JouleContainer : Float32Container {}

        /// <summary>KelvinContainer</summary>
        public class KelvinContainer : Float32Container {}

        /// <summary>KilogramContainer</summary>
        public class KilogramContainer : Float32Container {}

        /// <summary>MeterContainer</summary>
        public class MeterContainer : Float32Container {}

        /// <summary>MeterPerSecondContainer</summary>
        public class MeterPerSecondContainer : Float32Container {}

        /// <summary>MeterPerSecondSquaredContainer</summary>
        public class MeterPerSecondSquaredContainer : Float32Container {}

        /// <summary>NewtonContainer</summary>
        public class NewtonContainer : Float32Container {}

        /// <summary>PascalContainer</summary>
        public class PascalContainer : Float32Container {}

        /// <summary>RadianContainer</summary>
        public class RadianContainer : Float32Container {}

        /// <summary>RadianPerSecondContainer</summary>
        public class RadianPerSecondContainer : Float32Container {}

        /// <summary>RadianPerSecondSquaredContainer</summary>
        public class RadianPerSecondSquaredContainer : Float32Container {}

        /// <summary>SecondContainer</summary>
        public class SecondContainer : Float32Container {}

        /// <summary>SquareMeterContainer</summary>
        public class SquareMeterContainer : Float32Container {}

        /// <summary>SteradianContainer</summary>
        public class SteradianContainer : Float32Container {}

        /// <summary>VoltContainer</summary>
        public class VoltContainer : Float32Container {}

        /// <summary>WattContainer</summary>
        public class WattContainer : Float32Container {}
    }


    //--------------------------------------------------
    // SI-types (64-bits)
    //--------------------------------------------------
    namespace Si64
    {
        /// <summary>AmpereContainer</summary>
        public class AmpereContainer : Float64Container {}

        /// <summary>CubicMeterContainer</summary>
        public class CubicMeterContainer : Float64Container {}

        /// <summary>HertzContainer</summary>
        public class HertzContainer : Float64Container {}

        /// <summary>JouleContainer</summary>
        public class JouleContainer : Float64Container {}

        /// <summary>KelvinContainer</summary>
        public class KelvinContainer : Float64Container {}

        /// <summary>KilogramContainer</summary>
        public class KilogramContainer : Float64Container {}

        /// <summary>MeterContainer</summary>
        public class MeterContainer : Float64Container {}

        /// <summary>MeterPerSecondContainer</summary>
        public class MeterPerSecondContainer : Float64Container {}

        /// <summary>MeterPerSecondSquaredContainer</summary>
        public class MeterPerSecondSquaredContainer : Float64Container {}

        /// <summary>NewtonContainer</summary>
        public class NewtonContainer : Float64Container {}

        /// <summary>PascalContainer</summary>
        public class PascalContainer : Float64Container {}

        /// <summary>RadianContainer</summary>
        public class RadianContainer : Float64Container {}

        /// <summary>RadianPerSecondContainer</summary>
        public class RadianPerSecondContainer : Float64Container {}

        /// <summary>RadianPerSecondSquaredContainer</summary>
        public class RadianPerSecondSquaredContainer : Float64Container {}

        /// <summary>SecondContainer</summary>
        public class SecondContainer : Float64Container {}

        /// <summary>SquareMeterContainer</summary>
        public class SquareMeterContainer : Float64Container {}

        /// <summary>SteradianContainer</summary>
        public class SteradianContainer : Float64Container {}

        /// <summary>VoltContainer</summary>
        public class VoltContainer : Float64Container {}

        /// <summary>WattContainer</summary>
        public class WattContainer : Float64Container {}
    }

    #endregion

    #region Array Container definitions


    /// <summary>BooleanContainerArray</summary>
    public class BooleanContainerArray : ArrayContainer<BooleanContainer>
    {
        /// <summary>
        /// Constructor.
        /// </summary>
        /// <param name="size">Array size.</param>
        public BooleanContainerArray(System.Int32 size) : base(size) { }
    }

    /// <summary>Int32ContainerArray</summary>
    public class Int32ContainerArray : ArrayContainer<Int32Container>
    {
        /// <summary>
        /// Constructor.
        /// </summary>
        /// <param name="size">Array size.</param>
        public Int32ContainerArray(System.Int32 size) : base(size) { }
    }

    /// <summary>Int64ContainerArray</summary>
    public class Int64ContainerArray : ArrayContainer<Int64Container>
    {
        /// <summary>
        /// Constructor.
        /// </summary>
        /// <param name="size">Array size.</param>
        public Int64ContainerArray(System.Int32 size) : base(size) { }
    }

    /// <summary>Float32ContainerArray</summary>
    public class Float32ContainerArray : ArrayContainer<Float32Container>
    {
        /// <summary>
        /// Constructor.
        /// </summary>
        /// <param name="size">Array size.</param>
        public Float32ContainerArray(System.Int32 size) : base(size) { }
    }

    /// <summary>Float64ContainerArray</summary>
    public class Float64ContainerArray : ArrayContainer<Float64Container>
    {
        /// <summary>
        /// Constructor.
        /// </summary>
        /// <param name="size">Array size.</param>
        public Float64ContainerArray(System.Int32 size) : base(size) { }
    }
    /// <summary>TypeIdContainerArray</summary>
    public class TypeIdContainerArray : ArrayContainer<TypeIdContainer>
    {
        /// <summary>
        /// Constructor.
        /// <para/>
        /// Creates an array of the given size. Remember that once it has been created the size cannot be changed.
        ///
        /// </summary>
        /// <param name="size">The desired size of the array. Must be > 0.</param>
        public TypeIdContainerArray(System.Int32 size) : base(size) { }
    }

    /// <summary>InstanceIdContainerArray</summary>
    public class InstanceIdContainerArray : ArrayContainer<InstanceIdContainer>
    {
        /// <summary>
        /// Constructor.
        /// </summary>
        /// <param name="size">Array size.</param>
        public InstanceIdContainerArray(System.Int32 size) : base(size) { }
    }

    /// <summary>EntityIdContainerArray</summary>
    public class EntityIdContainerArray : ArrayContainer<EntityIdContainer>
    {
        /// <summary>
        /// Constructor.
        /// </summary>
        /// <param name="size">Array size.</param>
        public EntityIdContainerArray(System.Int32 size) : base(size) { }
    }

    /// <summary>ChannelIdContainerArray</summary>
    public class ChannelIdContainerArray : ArrayContainer<ChannelIdContainer>
    {
        /// <summary>
        /// Constructor.
        /// </summary>
        /// <param name="size">Array size.</param>
        public ChannelIdContainerArray(System.Int32 size) : base(size) { }
    }

    /// <summary>HandlerIdContainerArray</summary>
    public class HandlerIdContainerArray : ArrayContainer<HandlerIdContainer>
    {
        /// <summary>
        /// Constructor.
        /// </summary>
        /// <param name="size">Array size.</param>
        public HandlerIdContainerArray(System.Int32 size) : base(size) { }
    }
    /// <summary>ObjectContainerArray</summary>
    public class ObjectContainerArray : Safir.Dob.Typesystem.ArrayContainer<ObjectContainer>
    {
        /// <summary>
        /// Constructor.
        /// </summary>
        /// <param name="size">Array size.</param>
        public ObjectContainerArray(System.Int32 size) : base(size) { }
    }
    /// <summary>StringContainerArray</summary>
    public class StringContainerArray : Safir.Dob.Typesystem.ArrayContainer<StringContainer>
    {
        /// <summary>
        /// Constructor with size.
        /// </summary>
        /// <param name="size">Array size.</param>
        public StringContainerArray(System.Int32 size) : base(size) { }
    }
    /// <summary>BinaryContainerArray</summary>
    public class BinaryContainerArray : Safir.Dob.Typesystem.ArrayContainer<BinaryContainer>
    {
        /// <summary>
        /// Constructor.
        /// </summary>
        /// <param name="size">Array size.</param>
        public BinaryContainerArray(System.Int32 size) : base(size) { }
    }

    //--------------------------------------------------
    // SI-types (32-bits)
    //--------------------------------------------------
    namespace Si32
    {
        /// <summary>AmpereContainerArray</summary>
        public class AmpereContainerArray : ArrayContainer<AmpereContainer>
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public AmpereContainerArray(System.Int32 size) : base(size) { }
        }

        /// <summary>CubicMeterContainerArray</summary>
        public class CubicMeterContainerArray : ArrayContainer<CubicMeterContainer>
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public CubicMeterContainerArray(System.Int32 size) : base(size) { }
        }

        /// <summary>HertzContainerArray</summary>
        public class HertzContainerArray : ArrayContainer<HertzContainer>
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public HertzContainerArray(System.Int32 size) : base(size) { }
        }

        /// <summary>JouleContainerArray</summary>
        public class JouleContainerArray : ArrayContainer<JouleContainer>
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public JouleContainerArray(System.Int32 size) : base(size) { }
        }

        /// <summary>KelvinContainerArray</summary>
        public class KelvinContainerArray : ArrayContainer<KelvinContainer>
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public KelvinContainerArray(System.Int32 size) : base(size) { }
        }

        /// <summary>KilogramContainerArray</summary>
        public class KilogramContainerArray : ArrayContainer<KilogramContainer>
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public KilogramContainerArray(System.Int32 size) : base(size) { }
        }

        /// <summary>MeterContainerArray</summary>
        public class MeterContainerArray : ArrayContainer<MeterContainer>
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public MeterContainerArray(System.Int32 size) : base(size) { }
        }

        /// <summary>MeterPerSecondContainerArray</summary>
        public class MeterPerSecondContainerArray : ArrayContainer<MeterPerSecondContainer>
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public MeterPerSecondContainerArray(System.Int32 size) : base(size) { }
        }

        /// <summary>MeterPerSecondSquaredContainerArray</summary>
        public class MeterPerSecondSquaredContainerArray : ArrayContainer<MeterPerSecondSquaredContainer>
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public MeterPerSecondSquaredContainerArray(System.Int32 size) : base(size) { }
        }

        /// <summary>NewtonContainerArray</summary>
        public class NewtonContainerArray : ArrayContainer<NewtonContainer>
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public NewtonContainerArray(System.Int32 size) : base(size) { }
        }

        /// <summary>PascalContainerArray</summary>
        public class PascalContainerArray : ArrayContainer<PascalContainer>
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public PascalContainerArray(System.Int32 size) : base(size) { }
        }

        /// <summary>RadianContainerArray</summary>
        public class RadianContainerArray : ArrayContainer<RadianContainer>
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public RadianContainerArray(System.Int32 size) : base(size) { }
        }

        /// <summary>RadianPerSecondContainerArray</summary>
        public class RadianPerSecondContainerArray : ArrayContainer<RadianPerSecondContainer>
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public RadianPerSecondContainerArray(System.Int32 size) : base(size) { }
        }

        /// <summary>RadianPerSecondSquaredContainerArray</summary>
        public class RadianPerSecondSquaredContainerArray : ArrayContainer<RadianPerSecondSquaredContainer>
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public RadianPerSecondSquaredContainerArray(System.Int32 size) : base(size) { }
        }

        /// <summary>SecondContainerArray</summary>
        public class SecondContainerArray : ArrayContainer<SecondContainer>
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public SecondContainerArray(System.Int32 size) : base(size) { }
        }

        /// <summary>SquareMeterContainerArray</summary>
        public class SquareMeterContainerArray : ArrayContainer<SquareMeterContainer>
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public SquareMeterContainerArray(System.Int32 size) : base(size) { }
        }

        /// <summary>SteradianContainerArray</summary>
        public class SteradianContainerArray : ArrayContainer<SteradianContainer>
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public SteradianContainerArray(System.Int32 size) : base(size) { }
        }

        /// <summary>VoltContainerArray</summary>
        public class VoltContainerArray : ArrayContainer<VoltContainer>
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public VoltContainerArray(System.Int32 size) : base(size) { }
        }

        /// <summary>WattContainerArray</summary>
        public class WattContainerArray : ArrayContainer<WattContainer>
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public WattContainerArray(System.Int32 size) : base(size) { }


        }
    }


    //--------------------------------------------------
    // SI-types (64-bits)
    //--------------------------------------------------
    namespace Si64
    {


        /// <summary>AmpereContainerArray</summary>
        public class AmpereContainerArray : ArrayContainer<AmpereContainer>
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public AmpereContainerArray(System.Int32 size) : base(size) { }
        }

        /// <summary>CubicMeterContainerArray</summary>
        public class CubicMeterContainerArray : ArrayContainer<CubicMeterContainer>
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public CubicMeterContainerArray(System.Int32 size) : base(size) { }
        }

        /// <summary>HertzContainerArray</summary>
        public class HertzContainerArray : ArrayContainer<HertzContainer>
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public HertzContainerArray(System.Int32 size) : base(size) { }
        }

        /// <summary>JouleContainerArray</summary>
        public class JouleContainerArray : ArrayContainer<JouleContainer>
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public JouleContainerArray(System.Int32 size) : base(size) { }
        }

        /// <summary>KelvinContainerArray</summary>
        public class KelvinContainerArray : ArrayContainer<KelvinContainer>
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public KelvinContainerArray(System.Int32 size) : base(size) { }
        }

        /// <summary>KilogramContainerArray</summary>
        public class KilogramContainerArray : ArrayContainer<KilogramContainer>
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public KilogramContainerArray(System.Int32 size) : base(size) { }
        }

        /// <summary>MeterContainerArray</summary>
        public class MeterContainerArray : ArrayContainer<MeterContainer>
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public MeterContainerArray(System.Int32 size) : base(size) { }
        }

        /// <summary>MeterPerSecondContainerArray</summary>
        public class MeterPerSecondContainerArray : ArrayContainer<MeterPerSecondContainer>
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public MeterPerSecondContainerArray(System.Int32 size) : base(size) { }
        }

        /// <summary>MeterPerSecondSquaredContainerArray</summary>
        public class MeterPerSecondSquaredContainerArray : ArrayContainer<MeterPerSecondSquaredContainer>
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public MeterPerSecondSquaredContainerArray(System.Int32 size) : base(size) { }
        }

        /// <summary>NewtonContainerArray</summary>
        public class NewtonContainerArray : ArrayContainer<NewtonContainer>
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public NewtonContainerArray(System.Int32 size) : base(size) { }
        }

        /// <summary>PascalContainerArray</summary>
        public class PascalContainerArray : ArrayContainer<PascalContainer>
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public PascalContainerArray(System.Int32 size) : base(size) { }
        }

        /// <summary>RadianContainerArray</summary>
        public class RadianContainerArray : ArrayContainer<RadianContainer>
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public RadianContainerArray(System.Int32 size) : base(size) { }
        }

        /// <summary>RadianPerSecondContainerArray</summary>
        public class RadianPerSecondContainerArray : ArrayContainer<RadianPerSecondContainer>
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public RadianPerSecondContainerArray(System.Int32 size) : base(size) { }
        }

        /// <summary>RadianPerSecondSquaredContainerArray</summary>
        public class RadianPerSecondSquaredContainerArray : ArrayContainer<RadianPerSecondSquaredContainer>
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public RadianPerSecondSquaredContainerArray(System.Int32 size) : base(size) { }
        }

        /// <summary>SecondContainerArray</summary>
        public class SecondContainerArray : ArrayContainer<SecondContainer>
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public SecondContainerArray(System.Int32 size) : base(size) { }
        }

        /// <summary>SquareMeterContainerArray</summary>
        public class SquareMeterContainerArray : ArrayContainer<SquareMeterContainer>
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public SquareMeterContainerArray(System.Int32 size) : base(size) { }
        }

        /// <summary>SteradianContainerArray</summary>
        public class SteradianContainerArray : ArrayContainer<SteradianContainer>
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public SteradianContainerArray(System.Int32 size) : base(size) { }
        }

        /// <summary>VoltContainerArray</summary>
        public class VoltContainerArray : ArrayContainer<VoltContainer>
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public VoltContainerArray(System.Int32 size) : base(size) { }
        }

        /// <summary>WattContainerArray</summary>
        public class WattContainerArray : ArrayContainer<WattContainer>
        {
            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="size">Array size.</param>
            public WattContainerArray(System.Int32 size) : base(size) { }


        }
    }

    #endregion
}
