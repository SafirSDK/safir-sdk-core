/******************************************************************************
*
* Copyright Saab AB, 2004-2015 (http://safirsdkcore.com)
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
* GNU General Public License for more Internals.
*
* You should have received a copy of the GNU General Public License
* along with Safir SDK Core.  If not, see <http://www.gnu.org/licenses/>.
*
******************************************************************************/
#pragma once

#include <typeinfo>
#include <vector>
#include <boost/container/vector.hpp>
#include <Safir/Dob/Typesystem/EntityId.h>
#include <Safir/Dob/Typesystem/Utilities.h>
#include <Safir/Dob/Typesystem/ChannelId.h>
#include <Safir/Dob/Typesystem/HandlerId.h>
#include <Safir/Dob/Typesystem/ContainerBase.h>
#include <Safir/Dob/Typesystem/Object.h>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
    /**
     * Container class for sequences of values. A sequence is a collection of values that can dynamically
     * grow or shrink in size. The whole container has a change flag that will automatically
     * be set when values are added, removed or changed. Values in a sequence cannot be null and does not
     * have change flags.
     */
    template <class T>
    class SequenceContainerBase : public ContainerBase
    {
    public:

        typedef T ContainedType;
        typedef boost::container::vector<T> StorageType;  //we use boost version instead of std because we want to be able to use vector<bool> without warnings and errors.
        typedef typename StorageType::const_iterator const_iterator;

        /**
         * Default Constructor.
         *
         * Construct a container that is not changed and not null.
         */
        SequenceContainerBase()
            :ContainerBase()
            ,m_values()
        {
        }

        virtual bool IsNull() const {return empty();}

        virtual void SetNull()
        {
            clear();
        }


        /**
         * @brief size - Get the size of the sequence, i.e number of contained values.
         * @return The number of values in the sequence.
         */
        size_t size() const {return m_values.size();}

        /**
         * @brief empty - Check if sequence is empty.
         * @return True if sequence is empty, else false.
         */
        bool empty() const {return m_values.empty();}

        /**
         * @brief front - Get a const reference to the first value in the sequence.
         * @return Reference to first value.
         */
        const ContainedType& front() const {return m_values.front();}

        /**
         * @brief back - Get a const reference to the last value in the sequence.
         * @return Reference to last value.
         */
        const ContainedType& back() const {return m_values.back();}

        /**
         * @brief begin - Get const_iterator pointing to the first element in the sequence.
         * @return const_iterator.
         */
        const_iterator begin() const {return m_values.begin();}

        /**
         * @brief end - Get const_iterator pointing past the last element in the sequence.
         * @return const_iterator.
         */
        const_iterator end() const {return m_values.end();}

        /**
         * @brief clear - Clear the sequence, i.e remove all values. After a call to clear
         * the sequence will be empty but not automatically set to null.
         */
        void clear()
        {
            m_bIsChanged=true;
            m_values.clear();
        }

        /**
         * @brief operator [] - Get const reference to the value with specified index.
         * @param index [in] - Index of the value to get.
         * @return Const reference to a value.
         */
        const ContainedType& operator [](size_t index) const
        {
            return m_values[index];
        }

        /**
         * @brief push_back - Insert a new value last in the sequence. If the sequence was null
         * before it will no longer be null after a call to push_back.
         * @param val [in] - Value to be inserted.
         */
        void push_back(const ContainedType& val)
        {
            m_bIsChanged=true;
            m_values.push_back(val);
        }

        /**
         * @brief SetVal - Update a specific value. Will not add new values.
         * @param index [in] - Index of the value to set.
         * @param val [in] - Value to set.
         */
        void SetVal(size_t index, const ContainedType& val)
        {
            m_bIsChanged=true;
            m_values[index]=val;
        }

        /**
         * @brief GetVal - Get const reference to the value with specified index.
         * @param index [in] - Index of the value to get.
         * @return Const reference to a value.
         */
        const ContainedType& GetVal(size_t index) const
        {
            return m_values[index];
        }

        /**
         * @brief InsertAt - Insert a new value at specified index. The sequence size will grow.
         * @param index [in] - Index of the new value.
         * @param value [in] - Value to insert.
         */
        void InsertAt(size_t index, const ContainedType& value)
        {
            m_values.insert(m_values.begin()+index, value);
        }

        /**
         * @brief EraseAt - Erase a value at specified index. The sequence will shrink.
         * @param index [in] - Index of the value to be removed.
         */
        void EraseAt(size_t index)
        {
            m_bIsChanged=true;
            m_values.erase(m_values.begin()+index);
        }

        /**
         * @brief Copy - Copy all the members from "that" into "this". Types must be the same for this to work!
         * @param that [in] - The object to copy into this.
         * @throws SoftwareViolationException If the types are not of the same kind.
         */
        virtual void Copy(const ContainerBase& that)
        {
            if (this != &that)
            {
                if (typeid(*this) != typeid(that))
                {
                    throw SoftwareViolationException(L"Invalid call to Copy, containers are not of same type",__WFILE__,__LINE__);
                }

                const SequenceContainerBase<ContainedType>& other=
                    static_cast<const SequenceContainerBase<ContainedType>& >(that);

                m_bIsChanged=other.m_bIsChanged;
                m_values.clear();
                m_values.reserve(other.m_values.size());
                for (typename StorageType::const_iterator it=other.m_values.begin(); it!=other.m_values.end(); ++it)
                {
                    m_values.push_back(SequenceCopyHelper<ContainedType>::Copy(*it));
                }
            }
        }

    private:
        template <class U> struct SequenceCopyHelper
        {
            static U Copy(const U& val) {return val;}
        };

        template <class U> struct SequenceCopyHelper< boost::shared_ptr<U> >
        {
            static boost::shared_ptr<U> Copy(const boost::shared_ptr<U> & val) {return boost::static_pointer_cast<U>(val->Clone());}
        };


        StorageType m_values;
    };

    template <class T>
    class SequenceContainer : public SequenceContainerBase<T>
    {
        typedef SequenceContainerBase<T> Base;
    public:

        //Override of inherited method. Parent comment describes this behaviour too..
        virtual bool IsChanged() const {return Base::m_bIsChanged;}

        //Override of inherited method. Parent comment describes this behaviour too..
        virtual void SetChanged(const bool changed) {Base::m_bIsChanged = changed;}
    };

    template <class T>
    class GenericObjectSequenceContainer
        : public SequenceContainerBase<boost::shared_ptr<T>>
    {
        typedef SequenceContainerBase<boost::shared_ptr<T>> Base;
    public:

        //Override of inherited method. Parent comment describes this behaviour too..
        virtual bool IsChanged() const
        {
            if (Base::m_bIsChanged)
            {
                return true;
            }

            for (typename Base::const_iterator it=Base::begin(); it!=Base::end(); ++it)
            {
                if ((*it)->IsChanged()) //something in an object has changed
                {
                    return true;
                }
            }

            return false;
        }

        /**
         * Is the change flag in the container set?
         *
         * This method is like IsChanged without the recursion.
         *
         * @return True if the containers change flag is set.
         */
        virtual bool IsChangedHere() const
        {
            return Base::m_bIsChanged;
        }

        //Override of inherited method. Parent comment describes this behaviour too..
        virtual void SetChanged(const bool changed)
        {
            Base::m_bIsChanged = changed;

            for (typename Base::const_iterator it=Base::begin(); it!=Base::end(); ++it)
            {
                (*it)->SetChanged(changed);
            }
        }

        /**
         * Set the change flag in the container.
         *
         * This method is like SetChanged without the recursion
         *
         * @param changed [in] - The value to set the change flag to.
         */
        virtual void SetChangedHere(const bool changed)
        {
            Base::m_bIsChanged = changed;
        }
    };


    /**

     * @name Basic type container typedefs
     */
    /** @{ */


    /** A container containing string values */
    typedef SequenceContainer<std::wstring> StringSequenceContainer;

    /** A container containing boolean values */
    typedef SequenceContainer<bool> BooleanSequenceContainer;

    /** A container containing 32 bit integer values */
    typedef SequenceContainer<Int32> Int32SequenceContainer;

    /** A container containing 64 bit integer values */
    typedef SequenceContainer<Int64> Int64SequenceContainer;

    /** A container containing 32 bit floating point values */
    typedef SequenceContainer<Float32> Float32SequenceContainer;

    /** A container containing 64 bit floating point values */
    typedef SequenceContainer<Float64> Float64SequenceContainer;

    /** A container containing TypeId values */
    typedef SequenceContainer<TypeId> TypeIdSequenceContainer;

    /** A container containing InstanceId values */
    typedef SequenceContainer<InstanceId> InstanceIdSequenceContainer;

    /** A container containing EntityId values */
    typedef SequenceContainer<EntityId> EntityIdSequenceContainer;

    /** A container containing ChannelId values */
    typedef SequenceContainer<ChannelId> ChannelIdSequenceContainer;

    /** A container containing HandlerId values */
    typedef SequenceContainer<HandlerId> HandlerIdSequenceContainer;

    /** A container containing Binary values */
    typedef SequenceContainer<Binary> BinarySequenceContainer;

    /** A container containing Object values */
    typedef GenericObjectSequenceContainer<Object> ObjectSequenceContainer;

    /** @} */

    //--------------------------------------------------
    // SI-types (32-bits)
    //--------------------------------------------------
    namespace Si32
    {
        /** A container containing 32 bit Ampere values */
        typedef SequenceContainer<Ampere> AmpereSequenceContainer;

        /** A container containing 32 bit CubicMeter values */
        typedef SequenceContainer<CubicMeter> CubicMeterSequenceContainer;

        /** A container containing 32 bit Hertz values */
        typedef SequenceContainer<Hertz> HertzSequenceContainer;

        /** A container containing 32 bit Joule values */
        typedef SequenceContainer<Joule> JouleSequenceContainer;

        /** A container containing 32 bit Kelvin values */
        typedef SequenceContainer<Kelvin> KelvinSequenceContainer;

        /** A container containing 32 bit Kilogram values */
        typedef SequenceContainer<Kilogram> KilogramSequenceContainer;

        /** A container containing 32 bit Meter values */
        typedef SequenceContainer<Meter> MeterSequenceContainer;

        /** A container containing 32 bit MeterPerSecond values */
        typedef SequenceContainer<MeterPerSecond> MeterPerSecondSequenceContainer;

        /** A container containing 32 bit MeterPerSecondSquared values */
        typedef SequenceContainer<MeterPerSecondSquared> MeterPerSecondSquaredSequenceContainer;

        /** A container containing 32 bit Newton values */
        typedef SequenceContainer<Newton> NewtonSequenceContainer;

        /** A container containing 32 bit Pascal values */
        typedef SequenceContainer<Pascal> PascalSequenceContainer;

        /** A container containing 32 bit Radian values */
        typedef SequenceContainer<Radian> RadianSequenceContainer;

        /** A container containing 32 bit RadianPerSecond values */
        typedef SequenceContainer<RadianPerSecond> RadianPerSecondSequenceContainer;

        /** A container containing 32 bit RadianPerSecondSquared values */
        typedef SequenceContainer<RadianPerSecondSquared> RadianPerSecondSquaredSequenceContainer;

        /** A container containing 32 bit Second values */
        typedef SequenceContainer<Second> SecondSequenceContainer;

        /** A container containing 32 bit SquareMeter values */
        typedef SequenceContainer<SquareMeter> SquareMeterSequenceContainer;

        /** A container containing 32 bit Steradian values */
        typedef SequenceContainer<Steradian> SteradianSequenceContainer;

        /** A container containing 32 bit Volt values */
        typedef SequenceContainer<Volt> VoltSequenceContainer;

        /** A container containing 32 bit Watt values */
        typedef SequenceContainer<Watt> WattSequenceContainer;
    }

    //--------------------------------------------------
    // SI-types (64-bits)
    //--------------------------------------------------
    namespace Si64
    {
        /** A container containing 64 bit Ampere values */
        typedef SequenceContainer<Ampere> AmpereSequenceContainer;

        /** A container containing 64 bit CubicMeter values */
        typedef SequenceContainer<CubicMeter> CubicMeterSequenceContainer;

        /** A container containing 64 bit Hertz values */
        typedef SequenceContainer<Hertz> HertzSequenceContainer;

        /** A container containing 64 bit Joule values */
        typedef SequenceContainer<Joule> JouleSequenceContainer;

        /** A container containing 64 bit Kelvin values */
        typedef SequenceContainer<Kelvin> KelvinSequenceContainer;

        /** A container containing 64 bit Kilogram values */
        typedef SequenceContainer<Kilogram> KilogramSequenceContainer;

        /** A container containing 64 bit Meter values */
        typedef SequenceContainer<Meter> MeterSequenceContainer;

        /** A container containing 64 bit MeterPerSecond values */
        typedef SequenceContainer<MeterPerSecond> MeterPerSecondSequenceContainer;

        /** A container containing 64 bit MeterPerSecondSquared values */
        typedef SequenceContainer<MeterPerSecondSquared> MeterPerSecondSquaredSequenceContainer;

        /** A container containing 64 bit Newton values */
        typedef SequenceContainer<Newton> NewtonSequenceContainer;

        /** A container containing 64 bit Pascal values */
        typedef SequenceContainer<Pascal> PascalSequenceContainer;

        /** A container containing 64 bit Radian values */
        typedef SequenceContainer<Radian> RadianSequenceContainer;

        /** A container containing 64 bit RadianPerSecond values */
        typedef SequenceContainer<RadianPerSecond> RadianPerSecondSequenceContainer;

        /** A container containing 64 bit RadianPerSecondSquared values */
        typedef SequenceContainer<RadianPerSecondSquared> RadianPerSecondSquaredSequenceContainer;

        /** A container containing 64 bit Second values */
        typedef SequenceContainer<Second> SecondSequenceContainer;

        /** A container containing 64 bit SquareMeter values */
        typedef SequenceContainer<SquareMeter> SquareMeterSequenceContainer;

        /** A container containing 64 bit Steradian values */
        typedef SequenceContainer<Steradian> SteradianSequenceContainer;

        /** A container containing 64 bit Volt values */
        typedef SequenceContainer<Volt> VoltSequenceContainer;

        /** A container containing 64 bit Watt values */
        typedef SequenceContainer<Watt> WattSequenceContainer;
    }
}
}
}
