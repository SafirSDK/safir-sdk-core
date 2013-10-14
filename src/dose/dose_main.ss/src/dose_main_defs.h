/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
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

#ifndef _dose_main_defs_h
#define _dose_main_defs_h

#include <Safir/Utilities/Internal/UnorderedMap.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{

    const int NUM_NODES = 64;

    const int NUM_PRIORITY_CHANNELS = 6;
    /*
    static inline std::wstring AppendErrorCode(const std::wstring & str, int err)
    {
        std::wostringstream out;
        out << str << err;
        return out.str();
        }*/

}
}
}

#endif //_dose_main_defs_h
