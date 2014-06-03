/******************************************************************************
*
* Copyright Saab AB, 2013 (http://safir.sourceforge.net)
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
#ifndef __SAFIR_DOB_COMMUNICATION_HEARTBEAT_H__
#define __SAFIR_DOB_COMMUNICATION_HEARTBEAT_H__

#include <boost/noncopyable.hpp>
#include <boost/asio.hpp>
#include <boost/asio/steady_timer.hpp>
#include <boost/function.hpp>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/SystemLog.h>
#include "Node.h"
#include "Message.h"
#include "Writer.h"

namespace Safir
{
namespace Dob
{
namespace Internal
{
namespace Com
{
    /**
     * This class is sending periodic heart beats to all other system nodes.
     */

    template <class WriterType>
    class HeartbeatSenderBasic : private WriterType
    {
    public:
        HeartbeatSenderBasic(boost::asio::io_service& ioService,
                             int64_t myNodeId,
                             int ipVersion,
                             const std::string& multicast,
                             int heartbeatInterval)
            :WriterType(ioService, ipVersion, multicast)
            ,m_strand(ioService)
            ,m_heartbeatTimer(ioService)
            ,m_heartbeat(new Heartbeat(myNodeId))
            ,m_interval(heartbeatInterval)
            ,m_nodes()
            ,m_running(false)
        {
        }

        void Start()
        {
            m_strand.dispatch([=]
            {
                m_running=true;
                m_heartbeatTimer.expires_from_now(boost::chrono::milliseconds(m_interval));
                m_heartbeatTimer.async_wait(m_strand.wrap([=](const boost::system::error_code&){OnTimeout();}));
            });
        }

        void Stop()
        {
            m_strand.dispatch([=]
            {
                m_running=false;
                m_heartbeatTimer.cancel();
            });
        }

        //Add a node and starts sending heartbeats
        void AddNode(int64_t id, const boost::asio::ip::udp::endpoint& endpoint)
        {
            m_strand.dispatch([=]
            {
                m_nodes.insert(std::make_pair(id, NodeInfo(false, endpoint)));
            });
        }

        //Remove node and stop sending heartbeats
        void SetSystemNode(int64_t id)
        {
            m_strand.dispatch([=]
            {
                m_nodes.erase(id);
            });
        }

        //Make node included or excluded. If excluded it is also removed.
        void SetSystemNode(int64_t id, bool isSystemNode)
        {
            m_strand.dispatch([=]
            {
                if (isSystemNode)
                {
                    const auto it=m_nodes.find(id);
                    if (it!=m_nodes.end())
                    {
                        it->second.systemNode=isSystemNode;
                    }
                }
                else
                {
                    m_nodes.erase(id);
                }
            });
        }

    private:
        boost::asio::io_service::strand m_strand;
        boost::asio::steady_timer m_heartbeatTimer;
        boost::shared_ptr<Heartbeat> m_heartbeat;
        int m_interval;

        struct NodeInfo
        {
            bool systemNode;
            boost::asio::ip::udp::endpoint endpoint;
            NodeInfo() : systemNode(false), endpoint() {}
            NodeInfo(bool sn_, const boost::asio::ip::udp::endpoint& ep_) : systemNode(sn_), endpoint(ep_) {}
        };

        std::map<int64_t, NodeInfo> m_nodes;
        bool m_running;

        void OnTimeout()
        {
            if (m_running)
            {
                if (WriterType::IsMulticastEnabled())
                {
                    WriterType::SendMulticast(m_heartbeat);
                }
                else
                {
                    for (auto& vt : m_nodes)
                    {
                        if (vt.second.systemNode)
                        {
                            WriterType::SendTo(m_heartbeat, vt.second.endpoint);
                        }
                    }
                }

                m_heartbeatTimer.expires_from_now(boost::chrono::milliseconds(m_interval));
                m_heartbeatTimer.async_wait(m_strand.wrap([=](const boost::system::error_code&){OnTimeout();}));
            }
        }
    };

    typedef HeartbeatSenderBasic< Writer<Heartbeat> > HeartbeatSender;
}
}
}
}

#endif
