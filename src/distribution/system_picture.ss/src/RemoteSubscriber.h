/******************************************************************************
*
* Copyright Saab AB, 2013 (http://safirsdkcore.com)
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
#pragma once

#include <Safir/Utilities/Internal/Id.h>
#include <Safir/Utilities/Internal/SystemLog.h>
#include <Safir/Utilities/Internal/SharedCharArray.h>
#include <cstring>

namespace Safir
{
namespace Dob
{
namespace Internal
{
namespace SP
{
    template <class CommunicationT, class ReceiverT>
    class RemoteSubscriber
    {
    public:
        RemoteSubscriber(CommunicationT& communication,
                         const char* const receiverId,
                         ReceiverT& receiver)
            : m_receiver(receiver)
        {
            communication.SetDataReceiver([this](const int64_t from,
                                                 const int64_t /*nodeTypeId*/,
                                                 const char* const data,
                                                 const size_t size)
                                          {
                                              GotData(from,Safir::Utilities::Internal::SharedConstCharArray(data),size);
                                          },
                                          LlufId_Generate64(receiverId),
                                          [](size_t size){return new char[size];},
                                          [](const char * data){delete[] data;});
        }

    private:
        void GotData(const int64_t from,
                     const Safir::Utilities::Internal::SharedConstCharArray& data,
                     const size_t size)
        {
            m_receiver.NewRemoteStatistics(from, data, size);
        }

        ReceiverT& m_receiver;
    };

}
}
}
}
