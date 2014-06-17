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
#ifndef __SYSTEM_PICTURE_COORDINATOR_H__
#define __SYSTEM_PICTURE_COORDINATOR_H__

#include <Safir/Dob/Internal/RawStatistics.h>
#include <boost/noncopyable.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/asio.hpp>
#include <boost/asio/steady_timer.hpp>
#include <functional>
#include <limits>
#include <atomic>
#include <map>
#include <set>

#ifdef _MSC_VER
#  pragma warning (push)
#  pragma warning (disable: 4244)
#  pragma warning (disable: 4127)
#endif

#include "SystemStateMessage.pb.h"

#ifdef _MSC_VER
#  pragma warning (pop)
#endif

namespace Safir
{
namespace Dob
{
namespace Internal
{
    namespace Com
    {
        //forward declaration.
        class Communication;
    }


namespace SP
{
    //forward declarations
    class RawHandler;
    class RawStatistics;
    class SystemState;

    class Coordinator
        : private boost::noncopyable
    {
    public:
        Coordinator(boost::asio::io_service& ioService,
                    Com::Communication& communication,
                    const std::string& name,
                    const int64_t id,
                    const int64_t nodeTypeId,
                    const std::string& controlAddress,
                    const std::string& dataAddress,
                    const std::map<int64_t, NodeType>& nodeTypes,
                    const char* const receiverId,
                    RawHandler& rawHandler);
        
        bool IsElected() const {return m_id == m_elected;}

        //used to send state message
        //extraSpace adds bytes at the end of the buffer, e.g. for adding a crc
        //if onlyOwnState is true the callback will only be called if we're elected
        //and have a valid own system state that is ok to send.
        void PerformOnStateMessage(const std::function<void(std::unique_ptr<char []> data, 
                                                            const size_t size)> & fn,
                                   const size_t extraSpace,
                                   const bool onlyOwnState);
        
        //new incoming system state from elected coordinator
        void NewRemoteData(const int64_t from, 
                            const boost::shared_ptr<char[]>& data, 
                            const size_t size);

        void Stop();
    private:
        //returns true if the state is okay to publish
        bool UpdateMyState();

        void StartElection();
        void ElectionTimeout();

        void GotData(const int64_t from, 
                     const int64_t nodeTypeId, 
                     const boost::shared_ptr<char[]>& data, 
                     size_t size);

        void SendPendingElectionMessages();

        mutable boost::asio::strand m_strand;
        Com::Communication& m_communication;
        const uint64_t m_dataIdentifier;
        RawStatistics m_lastStatistics;
        SystemStateMessage m_stateMessage;
        
        const std::string m_name;
        const int64_t m_id;
        const int64_t m_nodeTypeId;
        const std::string m_controlAddress;
        const std::string m_dataAddress;
        const std::map<int64_t, NodeType> m_nodeTypes;
        const std::set<int64_t> m_nonLightNodeTypes;

        std::atomic<int64_t> m_elected;
        boost::asio::steady_timer m_electionTimer;
        int64_t m_currentElectionId = 0;

        boost::asio::steady_timer m_sendMessageTimer;
        RawHandler& m_rawHandler;


        //a set of node type ids to which we want to send INQUIRY to, using m_currentElectionId
        std::set<int64_t> m_pendingInquiries;
        
        //this is a list of the nodes that we need to send ALIVE messages to (key), along with the
        //election id that came in the INQUIRY (value.second) and the nodeTypeId of the recipient 
        //(value.first)
        std::map<int64_t, std::pair<int64_t, int64_t>> m_pendingAlives;

        //a set of node type ids to which we want to send VICTORY to, using m_currentElectionId
        std::set<int64_t> m_pendingVictories;
        
    };
}
}
}
}

#endif

