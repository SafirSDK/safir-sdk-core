/******************************************************************************
*
* Copyright Saab AB, 2013 (http://safir.sourceforge.net)
* Copyright Consoden AB, 2015 (http://www.consoden.se)
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

#include <boost/noncopyable.hpp>
#include <boost/function.hpp>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/SystemLog.h>
#include "Node.h"
#include "Message.h"
#include "Writer.h"

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4267)
#endif

#include <boost/asio.hpp>
#include <boost/asio/steady_timer.hpp>

#ifdef _MSC_VER
#pragma warning (pop)
#endif

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
                             int64_t nodeTypeId,
                             int64_t myNodeId,
                             int ipVersion,
                             const std::string& localIf,
                             const std::string& multicast,
                             int heartbeatInterval)
            :WriterType(ioService, ipVersion, localIf, multicast)
            ,m_strand(ioService)
            ,m_heartbeatTimer(ioService)
            ,m_heartbeat(new Heartbeat(myNodeId))
            ,m_interval(heartbeatInterval)
            ,m_nodes()
            ,m_running(false)
            ,m_logPrefix([&]{std::ostringstream os;
                             os<<"COM: (HeartbeatSender nodeType "<<nodeTypeId<<") - ";
                             return os.str();}())
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
        void AddNode(int64_t id, const std::string address)
        {
            m_strand.dispatch([=]
            {
                if (m_nodes.find(id)!=m_nodes.end())
                {
                    std::ostringstream os;
                    os<<m_logPrefix.c_str()<<"Duplicated call to AddNode with same nodeId! NodeId: "<<id<<", address: "<<address;
                    throw std::logic_error(os.str());
                }

                m_nodes.emplace(id, Resolver::StringToEndpoint(address));
            });
        }

        void RemoveNode(int64_t id)
        {
            m_strand.dispatch([=]
            {
                m_nodes.erase(id);
            });
        }

    private:
        boost::asio::io_service::strand m_strand;
        boost::asio::steady_timer m_heartbeatTimer;
        boost::shared_ptr<Heartbeat> m_heartbeat;
        int m_interval;
        std::map<int64_t, boost::asio::ip::udp::endpoint> m_nodes;
        bool m_running;
        const std::string m_logPrefix;

        void OnTimeout()
        {
            if (m_running)
            {
                if (WriterType::IsMulticastEnabled())
                {
                    lllog(8)<<m_logPrefix.c_str()<<"Send Heartbeat multicast"<<std::endl;
                    WriterType::SendMulticast(m_heartbeat);
                }
                else
                {
                    for (auto& vt : m_nodes)
                    {
                        lllog(8)<<m_logPrefix.c_str()<<"Send Heartbeat to "<<vt.first<<std::endl;
                        WriterType::SendTo(m_heartbeat, vt.second);
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
