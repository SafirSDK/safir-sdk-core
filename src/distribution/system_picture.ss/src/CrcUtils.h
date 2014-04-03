/******************************************************************************
*
* Copyright Saab AB, 2013 (http://safir.sourceforge.net)
*
* Created by: Lars Hagström / lars.hagstrom@consoden.se
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
#ifndef __CRC_UTILS_H__
#define __CRC_UTILS_H__

//TODO: remove checksumming when communication is stable
#define CHECK_CRC

#ifdef CHECK_CRC

#  ifdef _MSC_VER
#    pragma warning(push)
#    pragma warning(disable: 4245)
#    pragma warning(disable: 4244)
#  endif

#  include <boost/crc.hpp>

#  ifdef _MSC_VER
#    pragma warning(pop)
#  endif

namespace
{
#ifdef CHECK_CRC
    int GetCrc32(const void* data, const size_t size) {
        boost::crc_32_type result;
        result.process_bytes(data, size);
        return result.checksum();
    }
    
    BOOST_STATIC_ASSERT(sizeof(int) == 4);
#endif

}

#endif //CHECK_CRC
#endif

