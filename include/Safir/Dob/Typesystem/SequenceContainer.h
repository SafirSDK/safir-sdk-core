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

#include <Safir/Dob/Typesystem/ChannelId.h>
#include <Safir/Dob/Typesystem/ContainerBase.h>
#include <Safir/Dob/Typesystem/EntityId.h>
#include <Safir/Dob/Typesystem/HandlerId.h>
#include <Safir/Dob/Typesystem/Object.h>
#include <Safir/Dob/Typesystem/Utilities.h>
#include <boost/container/vector.hpp>
#include <stdexcept>
#include <typeinfo>
#include <vector>

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
    class SequenceContainer : public ContainerBase
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
        SequenceContainer()
            :ContainerBase()
            ,m_values()
        {
        }

        bool IsNull() const override {return empty();}

        void SetNull() override
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
         * Note that no checks are made to see whether index is inside range.
         * @param index [in] - Index of the value to get.
         * @return Const reference to a value.
         */
        const ContainedType& operator [](const size_t index) const
        {
            return m_values[index];
        }

        /**
         * @brief operator [] - Get const reference to the value with specified index.
         * @param index [in] - Index of the value to get.
         * @return Const reference to a value.
         * @throws std::out_of_range exception if index is not in range
         */
        const ContainedType& at(const size_t index) const
        {
            return m_values.at(index);
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
        void SetVal(const size_t index, const ContainedType& val)
        {
            m_bIsChanged=true;
            m_values[index]=val;
        }

        /**
         * @brief GetVal - Get const reference to the value with specified index.
         * @param index [in] - Index of the value to get.
         * @return Const reference to a value.
         */
        const ContainedType& GetVal(const size_t index) const
        {
            return m_values[index];
        }

        /**
         * @brief InsertAt - Insert a new value at specified index. The sequence size will grow.
         * @param index [in] - Index of the new value.
         * @param value [in] - Value to insert.
         */
        void InsertAt(const size_t index, const ContainedType& value)
        {
            m_bIsChanged=true;
            m_values.insert(m_values.begin()+index, value);
        }

        /**
         * @brief EraseAt - Erase a value at specified index. The sequence will shrink.
         * @param index [in] - Index of the value to be removed.
         */
        void EraseAt(const size_t index)
        {
            m_bIsChanged=true;
            m_values.erase(m_values.begin()+index);
        }

        /**
         * @brief Copy - Copy all the members from "that" into "this". Types must be the same for this to work!
         * @param that [in] - The object to copy into this.
         * @throws SoftwareViolationException If the types are not of the same kind.
         */
        void Copy(const ContainerBase& that) override
        {
            if (this != &that)
            {
                if (typeid(*this) != typeid(that))
                {
                    throw SoftwareViolationException(L"Invalid call to Copy, containers are not of same type",__WFILE__,__LINE__);
                }

                const SequenceContainer<ContainedType>& other=
                    static_cast<const SequenceContainer<ContainedType>& >(that);

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

    class EnumerationSequenceContainerBase : public ContainerBase
    {
    public:
        /**
         * Default Constructor.
         *
         * Construct a container that is not changed and not null.
         */
        EnumerationSequenceContainerBase()
            : ContainerBase()
        {
        }

        bool IsNull() const override {return empty();}

        void SetNull() override
        {
            clear();
        }


        /**
         * @brief size - Get the size of the sequence, i.e number of contained values.
         * @return The number of values in the sequence.
         */
        virtual size_t size() const = 0;

        /**
         * @brief empty - Check if sequence is empty.
         * @return True if sequence is empty, else false.
         */
        virtual bool empty() const = 0;

        /**
         * @brief clear - Clear the sequence, i.e remove all values. After a call to clear
         * the sequence will be empty but not automatically set to null.
         */
        virtual void clear() = 0;

        /**
         * @brief PushBackOrdinal - Insert a new ordinal value last in the sequence. If the sequence was null
         * before it will no longer be null after this call..
         * @param val [in] - Value to be inserted.
         */
        virtual void PushBackOrdinal(const EnumerationValue val) = 0;

        /**
         * @brief SetVal - Update a specific value. Will not add new values.
         * @param index [in] - Index of the value to set.
         * @param val [in] - Value to set.
         */
        virtual void SetOrdinal(const size_t index, const EnumerationValue val) = 0;

        /**
         * @brief GetVal - Get const reference to the value with specified index.
         * @param index [in] - Index of the value to get.
         * @return Const reference to a value.
         */
        virtual EnumerationValue GetOrdinal(const size_t index) const = 0;

        /**
         * @brief InsertOrdinalAt - Insert a new ordinal value at specified index. The sequence size will grow.
         * @param index [in] - Index of the new value.
         * @param value [in] - Value to insert.
         */
        virtual void InsertOrdinalAt(const size_t index, EnumerationValue value) = 0;

        /**
         * @brief EraseAt - Erase a value at specified index. The sequence will shrink.
         * @param index [in] - Index of the value to be removed.
         */
        virtual void EraseAt(const size_t index) = 0;

    };

    template <class T>
    class EnumerationSequenceContainer : public EnumerationSequenceContainerBase
    {
    public:
        typedef typename T::Enumeration ContainedType;
        typedef boost::container::vector<typename T::Enumeration> StorageType;  //we use boost version instead of std because we want to be able to use vector<bool> without warnings and errors.
        typedef typename StorageType::const_iterator const_iterator;

        /**
         * Default Constructor.
         *
         * Construct a container that is not changed and not null.
         */
        EnumerationSequenceContainer()
            :EnumerationSequenceContainerBase()
            ,m_values()
        {
        }

        /**
         * @brief size - Get the size of the sequence, i.e number of contained values.
         * @return The number of values in the sequence.
         */
        size_t size() const override {return m_values.size();}

        /**
         * @brief empty - Check if sequence is empty.
         * @return True if sequence is empty, else false.
         */
        bool empty() const override {return m_values.empty();}

        /**
         * @brief front - Get a const reference to the first value in the sequence.
         * @return Reference to first value.
         */
        ContainedType front() const {return m_values.front();}

        /**
         * @brief back - Get a const reference to the last value in the sequence.
         * @return Reference to last value.
         */
        ContainedType back() const {return m_values.back();}

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
        void clear() override
        {
            m_bIsChanged=true;
            m_values.clear();
        }

        /**
         * @brief operator [] - Get const reference to the value with specified index.
         * Note that no checks are made to see whether index is inside range.
         * @param index [in] - Index of the value to get.
         * @return Const reference to a value.
         */
        ContainedType operator [](const size_t index) const
        {
            T::CheckForMismatch();
            return m_values[index];
        }

        /**
         * @brief operator [] - Get const reference to the value with specified index.
         * @param index [in] - Index of the value to get.
         * @return Const reference to a value.
         * @throws std::out_of_range exception if index is not in range
         */
        ContainedType at(const size_t index) const
        {
            T::CheckForMismatch();
            return m_values.at(index);
        }

        /**
         * @brief push_back - Insert a new value last in the sequence. If the sequence was null
         * before it will no longer be null after a call to push_back.
         * @param val [in] - Value to be inserted.
         */
        void push_back(const ContainedType val)
        {
            T::CheckForMismatch();
            PushBackOrdinal(val);
        }
        /**
         * @brief PushBackOrdinal - Insert a new ordinal value last in the sequence. If the sequence was null
         * before it will no longer be null after this call..
         * @param val [in] - Value to be inserted.
         */
        void PushBackOrdinal(const EnumerationValue val) override
        {
            if (val < T::FirstOrdinal() || val > T::LastOrdinal())
            {
                throw Safir::Dob::Typesystem::IllegalValueException(L"The enumerated type DotsTest.TestEnum does not have such a value",__WFILE__,__LINE__);
            }
            m_bIsChanged=true;
            m_values.push_back(static_cast<ContainedType>(val));
        }


        /**
         * @brief SetVal - Update a specific value. Will not add new values.
         * @param index [in] - Index of the value to set.
         * @param val [in] - Value to set.
         */
        void SetVal(const size_t index, const ContainedType val)
        {
            T::CheckForMismatch();
            SetOrdinal(index, val);
        }

        /**
         * @brief GetVal - Get const reference to the value with specified index.
         * @param index [in] - Index of the value to get.
         * @return Const reference to a value.
         */
        ContainedType GetVal(const size_t index) const
        {
            T::CheckForMismatch();
            return m_values[index];
        }

        /**
         * @brief SetVal - Update a specific value. Will not add new values.
         * @param index [in] - Index of the value to set.
         * @param val [in] - Value to set.
         */
        void SetOrdinal(const size_t index, const EnumerationValue val) override
        {
            if (val < T::FirstOrdinal() || val > T::LastOrdinal())
            {
                throw Safir::Dob::Typesystem::IllegalValueException(L"The enumerated type DotsTest.TestEnum does not have such a value",__WFILE__,__LINE__);
            }

            m_bIsChanged=true;
            m_values[index]=static_cast<ContainedType>(val);
        }

        /**
         * @brief GetVal - Get const reference to the value with specified index.
         * @param index [in] - Index of the value to get.
         * @return Const reference to a value.
         */
        EnumerationValue GetOrdinal(const size_t index) const override
        {
            return m_values[index];
        }

        /**
         * @brief InsertAt - Insert a new value at specified index. The sequence size will grow.
         * @param index [in] - Index of the new value.
         * @param value [in] - Value to insert.
         */
        void InsertAt(const size_t index, ContainedType value)
        {
            T::CheckForMismatch();
            InsertOrdinalAt(index, value);
        }

        /**
         * @brief InsertAt - Insert a new value at specified index. The sequence size will grow.
         * @param index [in] - Index of the new value.
         * @param value [in] - Value to insert.
         */
        void InsertOrdinalAt(const size_t index, EnumerationValue value) override
        {
            if (value < T::FirstOrdinal() || value > T::LastOrdinal())
            {
                throw Safir::Dob::Typesystem::IllegalValueException(L"The enumerated type DotsTest.TestEnum does not have such a value",__WFILE__,__LINE__);
            }

            m_bIsChanged=true;
            m_values.insert(m_values.begin()+index, static_cast<ContainedType>(value));
        }

        /**
         * @brief EraseAt - Erase a value at specified index. The sequence will shrink.
         * @param index [in] - Index of the value to be removed.
         */
        void EraseAt(const size_t index) override
        {
            m_bIsChanged=true;
            m_values.erase(m_values.begin()+index);
        }

        /**
         * @brief Copy - Copy all the members from "that" into "this". Types must be the same for this to work!
         * @param that [in] - The object to copy into this.
         * @throws SoftwareViolationException If the types are not of the same kind.
         */
        void Copy(const ContainerBase& that) override
        {
            if (this != &that)
            {
                if (typeid(*this) != typeid(that))
                {
                    throw SoftwareViolationException(L"Invalid call to Copy, containers are not of same type",__WFILE__,__LINE__);
                }

                const auto& other=
                    static_cast<const EnumerationSequenceContainer<T>& >(that);

                m_bIsChanged = other.m_bIsChanged;
                m_values = other.m_values;
                m_values.clear();
            }
        }

    private:
        StorageType m_values;
    };


    /** Base class for all object sequences. Needed for the reflection stuff. */
    class GenericObjectSequenceContainerBase
    {
    public:
        /**
         * Is the change flag in the container set?
         *
         * This method is like IsChanged without the recursion.
         *
         * @return True if the containers change flag is set.
         */
        virtual bool IsChangedHere() const = 0;


        /**
         * Set the change flag in the container.
         *
         * This method is like SetChanged without the recursion
         *
         * @param changed [in] - The value to set the change flag to.
         */
        virtual void SetChangedHere(const bool changed) = 0;

        /**
         * @brief size - Get the size of the sequence, i.e number of contained values.
         * @return The number of values in the sequence.
         */
        virtual size_t size() const = 0;

        /**
         * @name Reflection part.
         * These methods allow applications to manipulate the members of objects
         * without having been compiled against it.
         * There should be no reason for most applications to use these methods.
         */
        /** @{ */

        /**
         * Get a generic poionter to the object at the specified index.
         *
         * Note: Unless you know that you need to use the reflection interface you should
         * prefer to use the functions in GenericObjectSequenceContainer.
         *
         * Indexing outside the sequence provokes undefined behavior.
         *
         * @param index [in] - The index of the object to get.
         * @return Pointer to object.
         */
        virtual ObjectPtr GetObj(const size_t index) = 0;

        /**
         * Get a const generic poionter to the object at the specified index.
         *
         * Note: Unless you know that you need to use the reflection interface you should
         * prefer to use the functions in GenericObjectSequenceContainer.
         *
         * Indexing outside the sequence provokes undefined behavior.
         *
         * @param index [in] - The index of the object to get.
         * @return Const pointer to object.
         */
        virtual ObjectConstPtr GetObj(const size_t index) const = 0;

        /** @} */

    private:
        friend void Utilities::MergeChanges(ObjectPtr into, const ObjectConstPtr& from);

        /**
         * Function needed by Utilities::MergeChanges to be able to merge
         * dictionaries. Will in turn call Utilities::MergeChanges recursively
         * if it needs to merge objects.
         */
        virtual void Merge(const GenericObjectSequenceContainerBase& other) = 0;
    };


    template <class T>
    class GenericObjectSequenceContainer
        : public SequenceContainer<boost::shared_ptr<T> >
        , public GenericObjectSequenceContainerBase
    {
        typedef SequenceContainer<boost::shared_ptr<T> > Base;
    public:

        //Override of inherited method. Parent comment describes this behaviour too..
        bool IsChanged() const override
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
        bool IsChangedHere() const override
        {
            return Base::m_bIsChanged;
        }

        //Override of inherited method. Parent comment describes this behaviour too..
        void SetChanged(const bool changed) override
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
        void SetChangedHere(const bool changed) override
        {
            Base::m_bIsChanged = changed;
        }

        size_t size() const override {return Base::size();}

        ObjectPtr GetObj(const size_t index) override {return Base::operator[](index);}
        ObjectConstPtr GetObj(const size_t index) const override {return Base::operator[](index);}

    private:
        void Merge(const GenericObjectSequenceContainerBase& that) override
        {
#ifndef NDEBUG
            if (typeid(GenericObjectSequenceContainer<T>) != typeid(that))
            {
                throw SoftwareViolationException(L"Invalid call to Merge, containers are not of same type",
                                                 __WFILE__,__LINE__);
            }
#endif

            const GenericObjectSequenceContainer<T>& other =
                static_cast<const GenericObjectSequenceContainer<T>&>(that);

            //Note: this function only gets called when IsChangedHere() == false

            if (!other.IsChanged())
            {
                return;
            }

            if (Base::size() != other.size())
            {
                throw SoftwareViolationException(L"It is not possible to merge two object sequences of different sizes.",
                                                 __WFILE__,__LINE__);
            }

            for (size_t i = 0; i < Base::size(); ++i)
            {
                if (other.at(i)->IsChanged())
                {
                    //recurse
                    Utilities::MergeChanges(Base::at(i),other.at(i));
                }
            }
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
