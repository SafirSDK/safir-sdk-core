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

#include <memory>
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
        HeartbeatSenderBasic(boost::asio::io_context& ioContext,
                             int64_t nodeTypeId,
                             int64_t myNodeId,
                             int ipVersion,
                             const std::string& localIf,
                             const std::string& multicast,
                             int heartbeatInterval)
            :WriterType(ioContext, ipVersion, localIf, multicast)
            ,m_strand(ioContext)
            ,m_heartbeatTimer(ioContext)
            ,m_heartbeat(new Heartbeat(myNodeId))
            ,m_interval(heartbeatInterval)
            ,m_nodes()
            ,m_running(false)
            ,m_logPrefix(GenerateLogPrefix(myNodeId, nodeTypeId))
        {
            
        }

        void Start() SAFIR_GCC_VISIBILITY_BUG_WORKAROUND
        {
            boost::asio::dispatch(m_strand, [this]
            {
                m_running=true;
                m_heartbeatTimer.expires_after(boost::chrono::milliseconds(m_interval));
                m_heartbeatTimer.async_wait(boost::asio::bind_executor(m_strand, [this](const boost::system::error_code&){OnTimeout();}));
            });
        }

        void Stop()
        {
            boost::asio::dispatch(m_strand, [this]
            {
                m_running=false;
                m_heartbeatTimer.cancel();
                m_nodes.clear();
            });
        }

        //Add a node and starts sending heartbeats
        void AddNode(int64_t id, const std::string address)
        {
            boost::asio::dispatch(m_strand, [this, id, address]
            {
                if (m_nodes.find(id)!=m_nodes.end())
                {
                    std::ostringstream os;
                    os<<m_logPrefix.c_str()<<"Duplicated call to AddNode with same nodeId! NodeId: "<<id<<", address: "<<address;
                    throw std::logic_error(os.str());
                }

                m_nodes.insert(std::make_pair(id, Resolver::StringToEndpoint(address)));
            });
        }

        void RemoveNode(int64_t id)
        {
            boost::asio::dispatch(m_strand, [this,id]
            {
                m_nodes.erase(id);
            });
        }

    private:
        boost::asio::io_context::strand m_strand;
        boost::asio::steady_timer m_heartbeatTimer;
        std::shared_ptr<Heartbeat> m_heartbeat;
        int m_interval;
        std::map<int64_t, boost::asio::ip::udp::endpoint> m_nodes;
        bool m_running;
        const std::string m_logPrefix;

        static std::string GenerateLogPrefix(int64_t nodeId, int64_t nodeTypeId)
        {
            std::ostringstream os;
            os<<"COM["<<nodeId<<"]: (HeartbeatSender nodeType "<<nodeTypeId<<") - ";
            return os.str();
        }

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
                    for (auto vt = m_nodes.cbegin(); vt != m_nodes.cend(); ++vt)
                    {
                        lllog(8)<<m_logPrefix.c_str()<<"Send Heartbeat to "<<vt->first<<std::endl;
                        WriterType::SendTo(m_heartbeat, vt->second);
                    }
                }

                m_heartbeatTimer.expires_after(boost::chrono::milliseconds(m_interval));
                m_heartbeatTimer.async_wait(boost::asio::bind_executor(m_strand, [this](const boost::system::error_code&){OnTimeout();}));
            }
        }
    };

    typedef HeartbeatSenderBasic< Writer<Heartbeat> > HeartbeatSender;
}
}
}
}
