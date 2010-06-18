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
#ifndef __DOSE_ARRAY_H__
#define __DOSE_ARRAY_H__

#include <boost/utility.hpp> 

namespace Safir
{
namespace Utilities
{
    /**
     * STL-like fixed size array.
     *
     * This type differs from boost::array in that the size is a constructor parameter
     * (and not a template parameter).
     * 
     */
    template<typename T>
    class Array : private boost::noncopyable
    {
    public:

        // type definitions
        typedef T* ArrayPtr;

        typedef T               value_type;
        typedef ArrayPtr        iterator;
        typedef const ArrayPtr  const_iterator;
        typedef T&              reference;
        typedef const T&        const_reference;
        typedef size_t          size_type;
        typedef ptrdiff_t       difference_type;                    

        // constructor
        Array(const size_t size)
            : arraySize(size),
              arrayPtr(new T[size])
        {
        }

        // destructor
        ~Array()
        {
            delete[] arrayPtr;
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

#endif

