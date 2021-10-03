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

namespace Safir
{
namespace Utilities
{
namespace Internal
{
    static inline std::shared_ptr<char[]> MakeSharedArray(const size_t size)
    {
#if (_MSC_VER >= 1927) || (__GNUC__ && __cpp_lib_shared_ptr_arrays >= 201707L)
        return std::make_shared<char[]>(size);
#else
        return std::shared_ptr<char[]>(new char[size]);
#endif
    }
}
}
}


