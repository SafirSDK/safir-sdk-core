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
#ifndef __RAW_SUBSCRIBER_REMOTE_H__
#define __RAW_SUBSCRIBER_REMOTE_H__

#include <Safir/Dob/Internal/Communication.h>
#include <Safir/Utilities/Internal/Id.h>
#include "RawHandler.h"
#include "CrcUtils.h"

namespace Safir
{
namespace Dob
{
namespace Internal
{
namespace SP
{
    class RawSubscriberRemote
    {
    public:
        RawSubscriberRemote(Com::Communication& communication,
                            const char* const receiverId,
                            RawHandler& rawHandler)
            : m_rawHandler(rawHandler)
        {
            communication.SetDataReceiver([this](const boost::int64_t from, 
                                                 const boost::int64_t /*nodeTypeId*/, 
                                                 const boost::shared_ptr<char[]>& data, 
                                                 const size_t size)
                                          {
                                              GotData(from,data,size);
                                          },
                                          LlufId_Generate64(receiverId));
        }
                            

    private:
        void GotData(const boost::int64_t from, 
                     const boost::shared_ptr<char[]>& data, 
                     size_t size)
        {
#ifdef CHECK_CRC
            size -= sizeof(int); //remove the crc from size
            int expected;
            memcpy(&expected, data.get() + size, sizeof(int));
            const int crc = GetCrc32(data.get(), size);
            if (crc != expected)
            {
                SEND_SYSTEM_LOG(Alert,
                                << "Bad CRC in data from " << from << ", expected " << expected << " got " << crc);
                throw std::logic_error("CRC check failed!");
            }
#endif
            m_rawHandler.UpdateRemoteStatistics(from, data, size);
        }

        RawHandler& m_rawHandler;
    };
}
}
}
}

#endif

