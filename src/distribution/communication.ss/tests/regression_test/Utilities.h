/******************************************************************************
*
* Copyright Saab AB, 2013-2015 (http://safirsdkcore.com)
*
* Created by: Joel Ottosson / joel.ottosson@consoden.se
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

#include <iostream>

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4267)
#pragma warning (disable: 4244)
#pragma warning (disable: 4245)
#pragma warning (disable: 4701)
#endif

#include <boost/asio.hpp>
#include <boost/crc.hpp>

#ifdef _MSC_VER
#pragma warning (pop)
#endif

#include <memory>
#include <boost/asio/steady_timer.hpp>
#include <boost/lexical_cast.hpp>
#include <Safir/Dob/Internal/Communication.h>
#include <Safir/Utilities/Internal/SharedCharArray.h>
#include <boost/chrono.hpp>

namespace Com = Safir::Dob::Internal::Com;

namespace Utilities
{
    inline void SetCRC(char* ptr, size_t size)
    {
        boost::crc_32_type crc;
        crc.process_bytes(static_cast<const void*>(ptr), size-4);
        uint32_t* val=reinterpret_cast<uint32_t*>(ptr+size-4);
        *val=crc.checksum();
    }

    inline bool ValidCRC(const char* ptr, size_t size)
    {
        boost::crc_32_type crc;
        crc.process_bytes(static_cast<const void*>(ptr), size-4);
        uint32_t checksum=*reinterpret_cast<const uint32_t*>(ptr+size-4);
        return checksum==crc.checksum();
    }

    inline Safir::Utilities::Internal::SharedCharArray CreateMsg(uint64_t value, size_t size)
    {
        auto data=Safir::Utilities::Internal::SharedCharArray(new char[size]);
        (*reinterpret_cast<uint64_t*>(data.get()))=value;
        SetCRC(data.get(), size);
        return data;
    }

    inline uint64_t GetValue(const char* data)
    {
        return *reinterpret_cast<const uint64_t*>(data);
    }

    inline std::string TimeString()
    {
        auto time=boost::chrono::steady_clock::now();
        auto s=boost::chrono::duration_cast<boost::chrono::milliseconds>(time.time_since_epoch());
        return boost::lexical_cast<std::string>(s.count()/1000.0);
    }
}
