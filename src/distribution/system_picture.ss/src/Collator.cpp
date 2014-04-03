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
#include "Collator.h"
#include "MessageWrapperCreators.h"
#include "RawHandler.h"
#include <Safir/Dob/Internal/Communication.h>
#include <Safir/Dob/Internal/RawStatistics.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <boost/make_shared.hpp>
#include <set>

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4244)
#pragma warning (disable: 4127)
#endif

#include "SystemStateMessage.pb.h"

#ifdef _MSC_VER
#pragma warning (pop)
#endif



namespace Safir
{
namespace Dob
{
namespace Internal
{
namespace SP
{
    Collator::Collator(const boost::shared_ptr<boost::asio::io_service>& ioService,
                       const boost::shared_ptr<Com::Communication>& communication,
                       const boost::shared_ptr<RawHandler>& rawHandler)
        : m_strand (*ioService)
        , m_communication(communication)
    {
        rawHandler->SetCollateCallback(m_strand.wrap([this](const RawStatistics& statistics)
                                                     {
                                                         Collate(statistics);
                                                     }));
    }
    

    //must be called in strand
    void Collator::Collate(const RawStatistics& statistics)
    {
        //currently we just copy the raw data... mucho stupido...

        lllog(4) << "Collating" << std::endl;
        if (!m_stateMessage)
        {
            m_stateMessage = boost::make_shared<SystemStateMessage>();

            m_stateMessage->set_name(statistics.Name());
            m_stateMessage->set_id(statistics.Id());
            m_stateMessage->set_control_address(statistics.Address());
        }

        std::set<boost::int64_t> deadNodes;
        for (int i = 0; i < statistics.Size(); ++i)
        {
            if (statistics.HasRemoteStatistics(i))
            {
                const auto remote = statistics.RemoteStatistics(i);
                for (int j = 0; j < remote.Size(); ++j)
                {
                    if (remote.IsDead(j))
                    {
                        deadNodes.insert(remote.Id(j));
                    }
                }
            }
        }


        m_stateMessage->clear_node_info(); //don't care about efficiency...

        for (int i = 0; i < statistics.Size(); ++i)
        {
            if (!statistics.IsDead(i))
            {
                //does another node think that that node is dead?
                if (deadNodes.find(statistics.Id(i)) != deadNodes.end())
                {
                    m_communication->ExcludeNode(statistics.Id(i));
                }
                else
                {
                    auto node = m_stateMessage->add_node_info();
                    node->set_name(statistics.Name(i));
                    node->set_id(statistics.Id(i));
                    node->set_control_address(statistics.Address(i));
                    node->set_multicast_enabled(statistics.MulticastEnabled(i));
                }
            }
        }


    }

    void Collator::PerformOnStateMessage(const boost::function<void(const boost::shared_ptr<char []>& data, 
                                                              const size_t size)> & fn) const
    {
        m_strand.dispatch([this,fn]
                          {
                              if (m_stateMessage != nullptr)
                              {
                                  const size_t size = m_stateMessage->ByteSize();
                                  auto data = boost::make_shared<char[]>(size);
                                  m_stateMessage->SerializeWithCachedSizesToArray
                                      (reinterpret_cast<google::protobuf::uint8*>(data.get()));
                                  fn(data, size);
                              }
                          });
    }
}
}
}
}

