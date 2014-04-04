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
    class HeartBeatSenderBasic : private WriterType
    {
    public:
        HeartBeatSenderBasic(boost::asio::io_service& ioService, const Node& me)
            :WriterType(ioService, me)
            ,m_strand(ioService)
            ,m_heartBeatTimer(ioService)
            ,m_heartBeat(new HeartBeat(me.Id()))
            ,m_nodes()
            ,m_running(false)
        {
        }

        void Start()
        {
            m_strand.dispatch([=]
            {
                m_running=true;
                m_heartBeatTimer.expires_from_now(boost::chrono::milliseconds(Parameters::HeartBeatInterval));
                m_heartBeatTimer.async_wait(m_strand.wrap([=](const boost::system::error_code&){OnTimeout();}));
            });
        }

        void Stop()
        {
            m_strand.dispatch([=]
            {
                m_running=false;
                m_heartBeatTimer.cancel();
            });
        }

        //Add a node.
        void AddNode(const Node& node)
        {
            m_strand.dispatch([=]
            {
                m_nodes.insert(std::make_pair(node.Id(), node));
            });
        }

        //Make node included or excluded. If excluded it is also removed.
        void SetSystemNode(boost::int64_t id, bool isSystemNode)
        {
            m_strand.dispatch([=]
            {
                if (isSystemNode)
                {
                    const auto it=m_nodes.find(id);
                    if (it!=m_nodes.end())
                    {
                        it->second.SetSystemNode(isSystemNode);
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
        boost::asio::steady_timer m_heartBeatTimer;
        boost::shared_ptr<HeartBeat> m_heartBeat;
        NodeMap m_nodes;
        bool m_running;

        void OnTimeout()
        {
            if (m_running)
            {
                WriterType::SendToAllSystemNodes(m_heartBeat, m_nodes);
                m_heartBeatTimer.expires_from_now(boost::chrono::milliseconds(Parameters::HeartBeatInterval));
                m_heartBeatTimer.async_wait(m_strand.wrap([=](const boost::system::error_code&){OnTimeout();}));
            }
        }
    };

    typedef HeartBeatSenderBasic< Writer<HeartBeat> > HeartBeatSender;
}
}
}
}

#endif
