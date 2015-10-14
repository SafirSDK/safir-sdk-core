/******************************************************************************
*
* Copyright Saab AB, 2006-2013 (http://safirsdkcore.com)
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

#ifndef __DOTS_VALUE_CONTAINERS_H__
#define __DOTS_VALUE_CONTAINERS_H__

#include <Safir/Dob/Typesystem/Defs.h>
#include <vector>

#include <Safir/Dob/Typesystem/ContainerBase.h>
#include <Safir/Dob/Typesystem/Exceptions.h>
#include <Safir/Dob/Typesystem/EntityId.h>
#include <Safir/Dob/Typesystem/Utilities.h>
#include <Safir/Dob/Typesystem/ChannelId.h>
#include <Safir/Dob/Typesystem/HandlerId.h>
#include <typeinfo>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
    /**
     * Container for base types.
     *
     * This class holds a value of the template argument type and a null flag.
     * The operations that modify the value update the null flag and the change flag
     * (which is inherited from ContainerBase).
     *
     * This container is intended for the simple types of the DOB typesystem.
     * There should be no need to use this type in a definition, since all the
     * relevant instances of this template are defined with typedefs
     * (e.g. Int32Container, BooleanContainer, EntityIdContainer, etc).
     *
     * @param T The type to contain.
     */
    template <class T>
    class  ValueContainer : public ContainerBase
    {
    public:
        typedef T ContainedType;

        /**
         * Default constructor.
         *
         * Creates a null and not changed container.
         */
        ValueContainer():ContainerBase(),m_bIsNull(true), m_Value() {}

        /**
         * Set the value of the container.
         *
         * Null and change flags are updated accordingly.
         *
         * @param value [in] - The new value.
         */
        void SetVal(const T value)  {m_Value = value; m_bIsNull = false; m_bIsChanged = true;}

        /**
         * Get the value of the container.
         *
         * @return The value of the container.
         * @throws NullException The container is null.
         */
        T GetVal() const    {if (m_bIsNull) throw NullException(L"value is null",__WFILE__,__LINE__); return m_Value;}

        //implementation of pure virtual in ContainerBase.
        virtual bool IsNull() const {return m_bIsNull;}

        //implementation of pure virtual in ContainerBase.
        virtual void SetNull()
        {
            m_bIsNull = true;
            m_bIsChanged = true;
        }

        //implementation of pure virtual in ContainerBase.
        virtual void Copy(const ContainerBase & that)
        {
            if (this != &that)
            {
                if (typeid(*this) != typeid(that))
                {
                    throw SoftwareViolationException(L"Invalid call to Copy, containers are not of same type",__WFILE__,__LINE__);
                }
                *this = static_cast<const ValueContainer<T> &>(that);
            }
        }

    private:
        bool m_bIsNull;
        T m_Value;

        friend class Safir::Dob::Typesystem::Internal::BlobOperations;
    };

    /**
     * Container for strings (std::wstring).
     *
     * This is a container for strings. It differs from the ordinary ValueContainer
     * in that it has methods for converting to UTF8 strings. These
     * are really only meant for blob serialization to use.
     */
    class StringContainer : public ContainerBase
    {
    public:
        typedef std::wstring ContainedType;

        /**
         * Default constructor.
         *
         * Creates a null and not changed container.
         */
        StringContainer():ContainerBase(),m_bIsNull(true), m_Value(),m_CachedUtf8String() {}

        /**
         * Set the value of the container.
         *
         * Null and change flags are updated accordingly.
         *
         * @param value [in] - The new value.
         */
        void SetVal(const std::wstring & value)  {m_Value = value; m_CachedUtf8String.clear(); m_bIsNull = false; m_bIsChanged = true;}

        /**
         * Get the value of the container.
         *
         * @return The value of the container.
         * @throws NullException The container is null.
         */
        const std::wstring & GetVal() const    {if (m_bIsNull) throw NullException(L"value is null",__WFILE__,__LINE__); return m_Value;}

        //implementation of pure virtual in ContainerBase.
        virtual bool IsNull() const {return m_bIsNull;}

        //implementation of pure virtual in ContainerBase.
        virtual void SetNull()
        {
            m_bIsNull = true;
            m_bIsChanged = true;
            m_CachedUtf8String.clear();
        }

        //implementation of pure virtual in ContainerBase.
        virtual void Copy(const ContainerBase & that)
        {
            if (this != &that)
            {
                if (typeid(*this) != typeid(that))
                {
                    throw SoftwareViolationException(L"Invalid call to Copy, containers are not of same type",__WFILE__,__LINE__);
                }
                *this = static_cast<const StringContainer &>(that);
            }
        }

        /**
         * @name UTF8 encoding methods.
         * These methods are really only meant for the blob serialization and deserialization,
         * but it is quite safe to use them if you really need them.
         */

        /** @{ */

        /**
         * Calculate the length needed for this string in UTF8 encoding.
         *
         * This method converts the string to utf8 (and caches it) and returns the
         * length of the converted string.
         *
         * The returned value includes space for null termination (adds 1 to string length).
         *
         * @return The length of the string when converted to UTF8, or 0 if the container is null.
         */
        Int32 Utf8StringLength() const
        {
            if (IsNull())
            {
                return 0;
            }

            if (m_Value.empty())
            {
                return 1;
            }

            if (m_CachedUtf8String.empty())
            {
                m_CachedUtf8String = Utilities::ToUtf8(m_Value);
            }

            return static_cast<Int32>(m_CachedUtf8String.length() + 1);
        }

        /**
         * Convert the string to a UTF8 encoded std::string.
         *
         * This method converts the string to utf8 (and caches it) and returns the
         * result.
         *
         * @return UTF8 string
         * @throws NullException The container is null.
         */
        const std::string & Utf8String() const
        {
            if (IsNull())
            {
                throw NullException(L"The string is null, cannot convert!",__WFILE__,__LINE__);
            }
            if (!m_Value.empty() && m_CachedUtf8String.empty())
            {
                m_CachedUtf8String = Utilities::ToUtf8(m_Value);
            }
            return m_CachedUtf8String;
        }

        /** @} */

    private:
        friend class Safir::Dob::Typesystem::Internal::BlobOperations;

        bool m_bIsNull;
        std::wstring m_Value;
        mutable std::string m_CachedUtf8String;
    };

    /**
     * Container for Binary
     *
     * This is a container for Binary. It differs from the ordinary ValueContainer
     * in that its GetVal-method returns a const reference instead of a copy of the content. These
     * are really only meant for blob serialization to use.
     */
    class BinaryContainer : public ContainerBase
    {
    public:
        typedef Binary ContainedType;
        /**
         * Default constructor.
         *
         * Creates a null and not changed container.
         */
        BinaryContainer():ContainerBase(),m_bIsNull(true), m_Value() {}

        /**
         * Get the value of the container.
         *
         * @return The value of the container.
         * @throws NullException The container is null.
         */
        const Binary& GetVal() const    {if (m_bIsNull) throw NullException(L"value is null",__WFILE__,__LINE__); return m_Value;}

        /**
         * Set the value of the container.
         *
         * Null and change flags are updated accordingly.
         *
         * @param value [in] - The new value.
         */
        void SetVal(const Binary & value)  {m_Value = value; m_bIsNull = false; m_bIsChanged = true;}


        //implementation of pure virtual in ContainerBase.
        virtual bool IsNull() const {return m_bIsNull;}

        //implementation of pure virtual in ContainerBase.
        virtual void SetNull()
        {
            m_bIsNull = true;
            m_bIsChanged = true;
        }

        //implementation of pure virtual in ContainerBase.
        virtual void Copy(const ContainerBase & that)
        {
            if (this != &that)
            {
                if (typeid(*this) != typeid(that))
                {
                    throw SoftwareViolationException(L"Invalid call to Copy, containers are not of same type",__WFILE__,__LINE__);
                }
                *this = static_cast<const BinaryContainer &>(that);
            }
        }

    private:
        friend class Safir::Dob::Typesystem::Internal::BlobOperations;

        bool m_bIsNull;
        Binary m_Value;
    };

    /**
     * @name Basic type container typedefs
     */
    /** @{ */

    /** A container containing boolean values */
    typedef ValueContainer<bool> BooleanContainer;

    /** A container containing 32 bit integer values */
    typedef ValueContainer<Int32> Int32Container;

    /** A container containing 64 bit integer values */
    typedef ValueContainer<Int64> Int64Container;

    /** A container containing 32 bit floating point values */
    typedef ValueContainer<Float32> Float32Container;

    /** A container containing 64 bit floating point values */
    typedef ValueContainer<Float64> Float64Container;

    /** A container containing TypeId values */
    typedef ValueContainer<TypeId> TypeIdContainer;

    /** A container containing InstanceId values */
    typedef ValueContainer<InstanceId> InstanceIdContainer;

    /** A container containing EntityId values */
    typedef ValueContainer<EntityId> EntityIdContainer;

    /** A container containing ChannelId values */
    typedef ValueContainer<ChannelId> ChannelIdContainer;

    /** A container containing HandlerId values */
    typedef ValueContainer<HandlerId> HandlerIdContainer;


    /** @} */

    //--------------------------------------------------
    // SI-types (32-bits)
    //--------------------------------------------------
    namespace Si32
    {
        /** A container containing 32 bit Ampere values */
        typedef ValueContainer<Ampere> AmpereContainer;

        /** A container containing 32 bit CubicMeter values */
        typedef ValueContainer<CubicMeter> CubicMeterContainer;

        /** A container containing 32 bit Hertz values */
        typedef ValueContainer<Hertz> HertzContainer;

        /** A container containing 32 bit Joule values */
        typedef ValueContainer<Joule> JouleContainer;

        /** A container containing 32 bit Kelvin values */
        typedef ValueContainer<Kelvin> KelvinContainer;

        /** A container containing 32 bit Kilogram values */
        typedef ValueContainer<Kilogram> KilogramContainer;

        /** A container containing 32 bit Meter values */
        typedef ValueContainer<Meter> MeterContainer;

        /** A container containing 32 bit MeterPerSecond values */
        typedef ValueContainer<MeterPerSecond> MeterPerSecondContainer;

        /** A container containing 32 bit MeterPerSecondSquared values */
        typedef ValueContainer<MeterPerSecondSquared> MeterPerSecondSquaredContainer;

        /** A container containing 32 bit Newton values */
        typedef ValueContainer<Newton> NewtonContainer;

        /** A container containing 32 bit Pascal values */
        typedef ValueContainer<Pascal> PascalContainer;

        /** A container containing 32 bit Radian values */
        typedef ValueContainer<Radian> RadianContainer;

        /** A container containing 32 bit RadianPerSecond values */
        typedef ValueContainer<RadianPerSecond> RadianPerSecondContainer;

        /** A container containing 32 bit RadianPerSecondSquared values */
        typedef ValueContainer<RadianPerSecondSquared> RadianPerSecondSquaredContainer;

        /** A container containing 32 bit Second values */
        typedef ValueContainer<Second> SecondContainer;

        /** A container containing 32 bit SquareMeter values */
        typedef ValueContainer<SquareMeter> SquareMeterContainer;

        /** A container containing 32 bit Steradian values */
        typedef ValueContainer<Steradian> SteradianContainer;

        /** A container containing 32 bit Volt values */
        typedef ValueContainer<Volt> VoltContainer;

        /** A container containing 32 bit Watt values */
        typedef ValueContainer<Watt> WattContainer;
    }

    //--------------------------------------------------
    // SI-types (64-bits)
    //--------------------------------------------------
    namespace Si64
    {
        /** A container containing 64 bit Ampere values */
        typedef ValueContainer<Ampere> AmpereContainer;

        /** A container containing 64 bit CubicMeter values */
        typedef ValueContainer<CubicMeter> CubicMeterContainer;

        /** A container containing 64 bit Hertz values */
        typedef ValueContainer<Hertz> HertzContainer;

        /** A container containing 64 bit Joule values */
        typedef ValueContainer<Joule> JouleContainer;

        /** A container containing 64 bit Kelvin values */
        typedef ValueContainer<Kelvin> KelvinContainer;

        /** A container containing 64 bit Kilogram values */
        typedef ValueContainer<Kilogram> KilogramContainer;

        /** A container containing 64 bit Meter values */
        typedef ValueContainer<Meter> MeterContainer;

        /** A container containing 64 bit MeterPerSecond values */
        typedef ValueContainer<MeterPerSecond> MeterPerSecondContainer;

        /** A container containing 64 bit MeterPerSecondSquared values */
        typedef ValueContainer<MeterPerSecondSquared> MeterPerSecondSquaredContainer;

        /** A container containing 64 bit Newton values */
        typedef ValueContainer<Newton> NewtonContainer;

        /** A container containing 64 bit Pascal values */
        typedef ValueContainer<Pascal> PascalContainer;

        /** A container containing 64 bit Radian values */
        typedef ValueContainer<Radian> RadianContainer;

        /** A container containing 64 bit RadianPerSecond values */
        typedef ValueContainer<RadianPerSecond> RadianPerSecondContainer;

        /** A container containing 64 bit RadianPerSecondSquared values */
        typedef ValueContainer<RadianPerSecondSquared> RadianPerSecondSquaredContainer;

        /** A container containing 64 bit Second values */
        typedef ValueContainer<Second> SecondContainer;

        /** A container containing 64 bit SquareMeter values */
        typedef ValueContainer<SquareMeter> SquareMeterContainer;

        /** A container containing 64 bit Steradian values */
        typedef ValueContainer<Steradian> SteradianContainer;

        /** A container containing 64 bit Volt values */
        typedef ValueContainer<Volt> VoltContainer;

        /** A container containing 64 bit Watt values */
        typedef ValueContainer<Watt> WattContainer;
    }


}
}
}
#endif

