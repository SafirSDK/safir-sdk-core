/******************************************************************************
*
* Copyright Saab AB, 2009 (http://www.safirsdk.com)
*
* Created by: Anders Widén / stawi
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
#ifndef __DOSE_SHM_ARRAY_H__
#define __DOSE_SHM_ARRAY_H__

#include <Safir/Dob/Internal/SharedMemoryObject.h>
#include <boost/utility.hpp>
#include <boost/interprocess/offset_ptr.hpp>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    /**
     * STL-like fixed size array in shared memory.
     *
     * The fixed array doesn't require the stored type to be copyable.
     * (Boost Interprocess supports STL-like vectors in shared memory. Since these vectors
     * are "dynamic" the stored type must be copyable.)
     * 
     */
    template<typename T>
    class ShmArray : public SharedMemoryObject, private boost::noncopyable
    {
    public:

        // type definitions
        typedef boost::interprocess::offset_ptr<T> ArrayPtr;

        typedef T               value_type;
        typedef ArrayPtr        iterator;
        typedef const ArrayPtr  const_iterator;
        typedef T&              reference;
        typedef const T&        const_reference;
        typedef size_t          size_type;
        typedef ptrdiff_t       difference_type;                    

        // constructor
        ShmArray(const size_t size)
            : arraySize(size),
              arrayPtr(GetSharedMemory().template construct<T>(boost::interprocess::anonymous_instance)[size]())
        {
        }

        // 1 arg constructor
        template <typename U>
        ShmArray(const size_t size, const U& arg)
            : arraySize(size),
              arrayPtr(GetSharedMemory().template construct<T>(boost::interprocess::anonymous_instance)[size](arg))
        {
        }

        // 2 arg constructor
        template <typename U, typename W>
        ShmArray(const size_t size, const U& arg1, const W& arg2)
            : arraySize(size),
              arrayPtr(GetSharedMemory().template construct<T>(boost::interprocess::anonymous_instance)[size](arg1, arg2))
        {
        }


        // destructor
        ~ShmArray()
        {
            GetSharedMemory().destroy_ptr(arrayPtr.get());
        }

        // iterator support
        iterator begin() {return arrayPtr;}
        const_iterator begin() const {return arrayPtr;}
        iterator end() {arrayPtr + arraySize;}

        // direct element access
        reference operator[](size_t i) {return *(arrayPtr + i);}
        const_reference operator[](size_t i) const {return *(arrayPtr + i);}
        
        // size
        size_type size() const {return arraySize;}
        size_type max_size() const {return arraySize;}

    private:
        const size_t      arraySize;
        const ArrayPtr    arrayPtr;
    };


}
}
}

#endif

