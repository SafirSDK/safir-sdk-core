/******************************************************************************
*
* Copyright Saab AB, 2021 (http://safirsdkcore.com)
*
* Created by: Lars Hagström / lars@foldspace.nu
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
#pragma once
#include <memory>

namespace Safir
{
namespace Utilities
{
namespace Internal
{
    //Visual Studio 2015 shared_ptr cannot handle char arrays, so we use boost::shared_ptr instead
    //there. Hopefully this can be removed when we drop VS2015 support.
#if (_MSC_VER == 1900)
    typedef boost::shared_ptr<char[]> SharedCharArray;
    typedef boost::shared_ptr<const char[]> SharedConstCharArray;
#else
    typedef std::shared_ptr<char[]> SharedCharArray;
    typedef std::shared_ptr<const char[]> SharedConstCharArray;
#endif

    static inline SharedCharArray MakeSharedArray(const size_t size)
    {
#if (__cpp_lib_shared_ptr_arrays >= 201707L)
        return std::make_shared<char[]>(size);
#else
        return SharedCharArray(new char[size]);
#endif
    }
}
}
}


