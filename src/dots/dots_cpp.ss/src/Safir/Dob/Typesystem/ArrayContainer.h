/******************************************************************************
*
* Copyright Saab AB, 2006-2008 (http://www.safirsdk.com)
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

#ifndef __DOTS_ARRAY_CONTAINER_H__
#define __DOTS_ARRAY_CONTAINER_H__

#include <Safir/Dob/Typesystem/ValueContainers.h>
#include <Safir/Dob/Typesystem/ObjectContainer.h>
#include <Safir/Dob/Typesystem/Exceptions.h>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
    /**
     * STL container for arrays of DOB-containers.
     *
     * This template class is used for arrays of containers in objects.
     * The arrays cannot change size once they have been created.
     * Apart from that they behave like a normal STL container.
     *
     * This class exports the following from std::vector:
     *
     *   Types: iterator, const_iterator, reference const_reference;
     *
     *    Methods: begin(), end().
     *
     * See your STL documentation for information on them
     *
     * Note: This container is currently a random access container (based on std::vector).
     *       At some point in the future it may change to become a reversible container (based on a std::map).
     *       Do not assume that the [] operator is constant time.
     */
    template <class T>
    class ArrayContainer : private std::vector<T>
    {
    private:
        typedef std::vector<T> Vector;
    public:

        /**
         * Constructor with size.
         *
         * Creates an array of the given size. Remember that once it has been created the size cannot be changed.
         *
         * @param size [in] - The desired size of the array. Must be > 0.
         *
         * @throws IllegalValueException The size argument is <= 0.
         */
        explicit ArrayContainer(const Int32 size):
            std::vector<T>(size),
            m_size(size)
        {
            if (size <= 0)
            {
                throw IllegalValueException(L"Arrays must be at least one in length!",__WFILE__,__LINE__);
            }
        }

        /**
         * Copy Constructor
         *
         * Creates a copy of an array
         *
         * @param other [in] - The array to create a copy of.
         */
        ArrayContainer(const ArrayContainer & other):
            std::vector<T>(other),
            m_size(other.m_size)
        {

        }

        /**
         * Copy Assignment operator
         *
         * Copies an array into another array.
         * The arrays must be of the same size.
         *
         * @param other [in] - The array to create a copy of.
         *
         * @throws SoftwareViolationException The arrays are not of the same size.
         */
        ArrayContainer & operator= (const ArrayContainer & other)
        {
            if (other.size() != size())
            {
                throw SoftwareViolationException(L"You are not allowed to copy arrays of different sizes",__WFILE__,__LINE__);
            }
            std::vector<T>::operator=(other);
            m_size = other.size();
            return *this;
        }

        //This may change to become a bidirectional iterator in the future (it is random access now)
        typedef typename Vector::iterator iterator;
        typedef typename Vector::const_iterator const_iterator;
        typedef typename Vector::reference reference;
        typedef typename Vector::const_reference const_reference;

        /**
         * Get an iterator to the start of the array.
         *
         * @return an iterator to the beginning of the array.
         */
        using Vector::begin;
        using Vector::end;

        /**
         * Get the size of the array.
         * This is a constant time operation.
         *
         * @return the size of the array.
         */
        Int32 size() const {return m_size;}

        /**
         * Returns a reference to the vector element at a specified position.
         *
         * @param index The index of the element to get.
         * @return A reference to the element.
         *
         * @throws IllegalValueException The array index is outside the range of the array.
         */
        reference operator[](const Int32 index)
        {
            if (index < 0 || index >= m_size)
            {
                throw IllegalValueException(L"Illegal array index!",__WFILE__,__LINE__);
            }
            return Vector::operator[](index);
        }

        /**
         * Returns a const reference to the vector element at a specified position.
         *
         * @param index The index of the element to get.
         * @return A const reference to the element.
         *
         * @throws IllegalValueException The array index is outside the range of the array.
         */
        const_reference operator[](const Int32 index) const
        {
            if (index < 0 || index >= m_size)
            {
                throw IllegalValueException(L"Illegal array index!",__WFILE__,__LINE__);
            }
            return Vector::operator[](index);
        }

        /**
         * Check if any element has a change flag set on it.
         *
         * Note that if this array contains objects this call will be recursive.
         *
         * @return true if any element has changed.
         */
        bool IsChanged() const
        {
            return end() != std::find_if(begin(),end(),&Typesystem::IsChanged);
        }

        /**
         * Set the change flag on all elements in the array.
         *
         * Note that if this array contains objects this call will be recursive.
         *
         * @param changed [in] - The value to set the change flags to.
         */
        void SetChanged(bool changed)
        {
            const_iterator e = end();
            for (iterator it = begin(); it!=e;++it)
            {
                it->SetChanged(changed);
            }
        }
    private:
        Int32 m_size;
    };


    /**
     * @name Container array typedefs
     */
    /** @{ */

    /** An array of DOB-containers containing booleans. */
    typedef ArrayContainer<BooleanContainer> BooleanContainerArray;

    /** An array of DOB-containers containing 32 bit integers. */
    typedef ArrayContainer<Int32Container> Int32ContainerArray;

    /** An array of DOB-containers containing 64 bit integers. */
    typedef ArrayContainer<Int64Container> Int64ContainerArray;

    /** An array of DOB-containers containing 32 bit floats. */
    typedef ArrayContainer<Float32Container> Float32ContainerArray;

    /** An array of DOB-containers containing 64 bit floats. */
    typedef ArrayContainer<Float64Container> Float64ContainerArray;

    /** An array of DOB-containers containing TypeIds. */
    typedef ArrayContainer<TypeIdContainer> TypeIdContainerArray;

    /** An array of DOB-containers containing InstanceIds. */
    typedef ArrayContainer<InstanceIdContainer> InstanceIdContainerArray;

    /** An array of DOB-containers containing EntityIds. */
    typedef ArrayContainer<EntityIdContainer> EntityIdContainerArray;

    /** An array of DOB-containers containing ChannelIds. */
    typedef ArrayContainer<ChannelIdContainer> ChannelIdContainerArray;

    /** An array of DOB-containers containing HandlerIds. */
    typedef ArrayContainer<HandlerIdContainer> HandlerIdContainerArray;

    /** An array of DOB-containers containing strings. */
    typedef ArrayContainer<StringContainer> StringContainerArray;

    /** An array of DOB-containers containing DOB-objects. */
    typedef Safir::Dob::Typesystem::ArrayContainer<ObjectContainer> ObjectContainerArray;

    /** An array of DOB-containers containing Binaries. */
    typedef Safir::Dob::Typesystem::ArrayContainer<BinaryContainer> BinaryContainerArray;

    /** @} */

    //--------------------------------------------------
    // SI-types (32-bits)
    //--------------------------------------------------
    namespace Si32
    {
        /** An array of DOB-containers containing 32 bit Ampere values */
        typedef ArrayContainer<AmpereContainer> AmpereContainerArray;

        /** An array of DOB-containers containing 32 bit CubicMeter values */
        typedef ArrayContainer<CubicMeterContainer> CubicMeterContainerArray;

        /** An array of DOB-containers containing 32 bit Hertz values */
        typedef ArrayContainer<HertzContainer> HertzContainerArray;

        /** An array of DOB-containers containing 32 bit Joule values */
        typedef ArrayContainer<JouleContainer> JouleContainerArray;

        /** An array of DOB-containers containing 32 bit Kelvin values */
        typedef ArrayContainer<KelvinContainer> KelvinContainerArray;

        /** An array of DOB-containers containing 32 bit Kilogram values */
        typedef ArrayContainer<KilogramContainer> KilogramContainerArray;

        /** An array of DOB-containers containing 32 bit Meter values */
        typedef ArrayContainer<MeterContainer> MeterContainerArray;

        /** An array of DOB-containers containing 32 bit MeterPerSecond values */
        typedef ArrayContainer<MeterPerSecondContainer> MeterPerSecondContainerArray;

        /** An array of DOB-containers containing 32 bit MeterPerSecondSquared values */
        typedef ArrayContainer<MeterPerSecondSquaredContainer> MeterPerSecondSquaredContainerArray;

        /** An array of DOB-containers containing 32 bit Newton values */
        typedef ArrayContainer<NewtonContainer> NewtonContainerArray;

        /** An array of DOB-containers containing 32 bit Pascal values */
        typedef ArrayContainer<PascalContainer> PascalContainerArray;

        /** An array of DOB-containers containing 32 bit Radian values */
        typedef ArrayContainer<RadianContainer> RadianContainerArray;

        /** An array of DOB-containers containing 32 bit RadianPerSecond values */
        typedef ArrayContainer<RadianPerSecondContainer> RadianPerSecondContainerArray;

        /** An array of DOB-containers containing 32 bit RadianPerSecondSquared values */
        typedef ArrayContainer<RadianPerSecondSquaredContainer> RadianPerSecondSquaredContainerArray;

        /** An array of DOB-containers containing 32 bit Second values */
        typedef ArrayContainer<SecondContainer> SecondContainerArray;

        /** An array of DOB-containers containing 32 bit SquareMeter values */
        typedef ArrayContainer<SquareMeterContainer> SquareMeterContainerArray;

        /** An array of DOB-containers containing 32 bit Steradian values */
        typedef ArrayContainer<SteradianContainer> SteradianContainerArray;

        /** An array of DOB-containers containing 32 bit Volt values */
        typedef ArrayContainer<VoltContainer> VoltContainerArray;

        /** An array of DOB-containers containing 32 bit Watt values */
        typedef ArrayContainer<WattContainer> WattContainerArray;
    }

    //--------------------------------------------------
    // SI-types (64-bits)
    //--------------------------------------------------
    namespace Si64
    {
        /** An array of DOB-containers containing 64 bit Ampere values */
        typedef ArrayContainer<AmpereContainer> AmpereContainerArray;

        /** An array of DOB-containers containing 64 bit CubicMeter values */
        typedef ArrayContainer<CubicMeterContainer> CubicMeterContainerArray;

        /** An array of DOB-containers containing 64 bit Hertz values */
        typedef ArrayContainer<HertzContainer> HertzContainerArray;

        /** An array of DOB-containers containing 64 bit Joule values */
        typedef ArrayContainer<JouleContainer> JouleContainerArray;

        /** An array of DOB-containers containing 64 bit Kelvin values */
        typedef ArrayContainer<KelvinContainer> KelvinContainerArray;

        /** An array of DOB-containers containing 64 bit Kilogram values */
        typedef ArrayContainer<KilogramContainer> KilogramContainerArray;

        /** An array of DOB-containers containing 64 bit Meter values */
        typedef ArrayContainer<MeterContainer> MeterContainerArray;

        /** An array of DOB-containers containing 64 bit MeterPerSecond values */
        typedef ArrayContainer<MeterPerSecondContainer> MeterPerSecondContainerArray;

        /** An array of DOB-containers containing 64 bit MeterPerSecondSquared values */
        typedef ArrayContainer<MeterPerSecondSquaredContainer> MeterPerSecondSquaredContainerArray;

        /** An array of DOB-containers containing 64 bit Newton values */
        typedef ArrayContainer<NewtonContainer> NewtonContainerArray;

        /** An array of DOB-containers containing 64 bit Pascal values */
        typedef ArrayContainer<PascalContainer> PascalContainerArray;

        /** An array of DOB-containers containing 64 bit Radian values */
        typedef ArrayContainer<RadianContainer> RadianContainerArray;

        /** An array of DOB-containers containing 64 bit RadianPerSecond values */
        typedef ArrayContainer<RadianPerSecondContainer> RadianPerSecondContainerArray;

        /** An array of DOB-containers containing 64 bit RadianPerSecondSquared values */
        typedef ArrayContainer<RadianPerSecondSquaredContainer> RadianPerSecondSquaredContainerArray;

        /** An array of DOB-containers containing 64 bit Second values */
        typedef ArrayContainer<SecondContainer> SecondContainerArray;

        /** An array of DOB-containers containing 64 bit SquareMeter values */
        typedef ArrayContainer<SquareMeterContainer> SquareMeterContainerArray;

        /** An array of DOB-containers containing 64 bit Steradian values */
        typedef ArrayContainer<SteradianContainer> SteradianContainerArray;

        /** An array of DOB-containers containing 64 bit Volt values */
        typedef ArrayContainer<VoltContainer> VoltContainerArray;

        /** An array of DOB-containers containing 64 bit Watt values */
        typedef ArrayContainer<WattContainer> WattContainerArray;
    }
}
}
}
#endif

