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

#include <Safir/Dob/Internal/Communication.h>
#include <Safir/Utilities/Internal/Id.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include "RawHandler.h"
#include <set>

namespace Safir
{
namespace Dob
{
namespace Internal
{
namespace SP
{
    template <class RawHandlerT, class CommunicationT>
    class RawPublisherRemoteBasic
    {
    public:
        RawPublisherRemoteBasic(const std::wstring& logPrefix,
                                boost::asio::io_service& ioService,
                                CommunicationT& communication,
                                const std::map<int64_t, NodeType>& nodeTypes,
                                const char* const senderId,
                                RawHandlerT& rawHandler,
                                const std::chrono::steady_clock::duration& period)
            : m_logPrefix(logPrefix)
            , m_strand(ioService)
            , m_timer(ioService)
            , m_stopped(false)
            , m_communication(communication)
            , m_senderId(LlufId_Generate64(senderId))
            , m_nodeTypes(nodeTypes)
            , m_rawHandler(rawHandler)
            , m_allNodeTypes(ExtractKeys(nodeTypes))
            , m_period(period)
        {

            SchedulePublishTimer(period, m_allNodeTypes);

            rawHandler.AddRawChangedCallback(m_strand.wrap([this](const RawStatistics&,
                                                                  const RawChanges flags,
                                                                  std::shared_ptr<void> /*completionSignaller*/)
            {
                if (flags.NodesChanged() || flags.MetadataChanged())
                {
                    Publish(m_allNodeTypes);
                }
            }));
        }

        void Stop()
        {
            const bool was_stopped = m_stopped.exchange(true);
            if (!was_stopped)
            {
                m_strand.dispatch([this]
                                  {
                                      m_timer.cancel();
                                  });
            }
        }

    private:

        template <class T, class U>
        static std::set<T> ExtractKeys(const std::map<T,U>& map)
        {
            std::set<T> set;
            for (auto it = map.cbegin(); it != map.cend(); ++it)
            {
                set.insert(it->first);
            }
            return set;
        }


        //must be called in strand
        void SchedulePublishTimer(const std::chrono::steady_clock::duration& delay,
                                  const std::set<int64_t>& toNodeTypes)
        {
            if (m_stopped)
            {
                return;
            }

            m_timer.cancel();
            m_timer.expires_from_now(delay);
            m_timer.async_wait(m_strand.wrap([this, toNodeTypes](const boost::system::error_code& error)
            {
                if (!error)
                {
                    Publish(toNodeTypes);
                }
            }));
        }

        //must be called in strand
        //if empty set is passed to this function we send to all node types
        void Publish(const std::set<int64_t>& toNodeTypes)
        {
            if (m_stopped)
            {
                return;
            }

            lllog(8) << m_logPrefix << "Publishing raw statistics to other nodes" << std::endl;

            //start by scheduling next timer to send to all nodes.
            //this will be cancelled if we need to do a resend due to overflow below.
            SchedulePublishTimer(m_period, m_allNodeTypes);

            m_rawHandler.PerformOnMyStatisticsMessage([this,toNodeTypes]
                                                      (std::unique_ptr<char[]> d,
                                                       const size_t size)
            {
                //we need to move the data into a shared_ptr
                Safir::Utilities::Internal::SharedCharArray data(std::move(d));
                std::set<int64_t> overflowNodes;

                for (auto id = toNodeTypes.cbegin(); id != toNodeTypes.cend(); ++id)
                {
                    const bool sent = m_communication.Send(0, *id, data, size, m_senderId, true);
                    if (!sent)
                    {
                        lllog(7) << m_logPrefix << "RawPublisherRemote: Overflow when sending to node type "
                                 << m_nodeTypes.find(*id)->second.name.c_str() << std::endl;
                        overflowNodes.insert(*id);
                    }
                }

                if (!overflowNodes.empty())
                {
                    SchedulePublishTimer(std::chrono::milliseconds(100), overflowNodes);
                }
            });
        }

        const std::wstring m_logPrefix;
        boost::asio::io_service::strand m_strand;
        boost::asio::steady_timer m_timer;
        std::atomic<bool> m_stopped;
        CommunicationT& m_communication;
        const uint64_t m_senderId;
        const std::map<int64_t, NodeType> m_nodeTypes;
        RawHandlerT& m_rawHandler;
        const std::set<int64_t> m_allNodeTypes;
        const std::chrono::steady_clock::duration m_period;
    };

    typedef RawPublisherRemoteBasic<RawHandler, Com::Communication> RawPublisherRemote;
}
}
}
}
