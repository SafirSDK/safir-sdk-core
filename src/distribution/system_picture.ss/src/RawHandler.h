/******************************************************************************
*
* Copyright Saab AB, 2012 (http://safir.sourceforge.net)
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
#ifndef __RAW_HANDLER_H__
#define __RAW_HANDLER_H__

#include <Safir/Utilities/Internal/Atomic.h>
#include <Safir/Utilities/Internal/AsioPeriodicTimer.h>
#include <Safir/Dob/Internal/SystemPictureDefs.h>
#include <boost/asio.hpp>
#include <boost/chrono.hpp>
#include <boost/noncopyable.hpp>
#include <string>
#include <unordered_map>
#include <functional>

#ifdef _MSC_VER
#  pragma warning (push)
#  pragma warning (disable: 4244)
#  pragma warning (disable: 4127)
#endif

#include "NodeStatisticsMessage.pb.h"

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
        //forward declaration.
        class Communication;
    }

namespace SP
{
    //forward declaration
    class RawStatistics;

    typedef std::function<void(const RawStatistics& statistics)> StatisticsCallback;

    class RawHandler
        : private boost::noncopyable
    {
    public:
        RawHandler(boost::asio::io_service& ioService,
                   Com::Communication& communication,
                   const std::string& name,
                   const int64_t id,
                   const int64_t nodeTypeId,
                   const std::string& controlAddress,
                   const std::string& dataAddress,
                   const std::map<int64_t, NodeType>& nodeTypes);

        void Stop();

        //extraSpace adds bytes at the end of the buffer, e.g. for adding a crc
        void PerformOnMyStatisticsMessage(const std::function<void(const boost::shared_ptr<char[]>& data, 
                                                                     const size_t size)> & fn,
                                          const size_t extraSpace) const;

        void PerformOnAllStatisticsMessage(const std::function<void(std::unique_ptr<char []> data, 
                                                                    const size_t size)> & fn,
                                           const size_t extraSpace) const;
        
        void NewRemoteData(const int64_t from, const boost::shared_ptr<char[]>& data, const size_t size);


        /** 
         * Add a callback that will be called whenever a new node is discovered,
         * or is declared as dead.
         *
         * Will always be posted! data will be a copy
         */
        void AddNodesChangedCallback(const StatisticsCallback& callback);

        /** 
         * Add a callback that will be called whenever a new election id is set.
         *
         * Will always be posted! data will be a copy
         */
        void AddElectionIdChangedCallback(const StatisticsCallback& callback);

        /** 
         * Add a callback that will be called whenever the raw data is changed
         * as a result of receiving new raw data from other node.
         *
         * Will always be posted! data will be a copy
         */
        void AddRawChangedCallback(const StatisticsCallback& callback);

        void SetDeadNode(const int64_t id);
        void SetElectionId(const int64_t id);
    private:
        void NewNode(const std::string& name,
                     const int64_t id,
                     const int64_t nodeTypeId,
                     const std::string& controlAddress,
                     const std::string& dataAddress);
        
        void GotReceive(int64_t id);
        void Retransmit(int64_t id);

        void CheckDeadNodes(const boost::system::error_code& error);

        /** Post a copy of the data on the ioservice */
        void PostNodesChangedCallback();

        /** Post a copy of the data on the ioservice */
        void PostElectionIdChangedCallback();

        /** Post a copy of the data on the ioservice */
        void PostRawChangedCallback();

        struct NodeInfo
        {
            explicit NodeInfo(NodeStatisticsMessage_NodeInfo* const nodeInfo_)
                : lastReceiveTime(boost::chrono::steady_clock::now()),nodeInfo(nodeInfo_) {}

            boost::chrono::steady_clock::time_point lastReceiveTime;
            NodeStatisticsMessage_NodeInfo* nodeInfo;
        };
        typedef std::unordered_map<int64_t, NodeInfo> NodeTable;

        boost::asio::io_service& m_ioService;
        Com::Communication& m_communication;

        const int64_t m_id;
        const std::map<int64_t, NodeType> m_nodeTypes;
        const boost::chrono::steady_clock::time_point m_epoch;
        mutable boost::asio::strand m_strand;

        Safir::Utilities::Internal::AsioPeriodicTimer m_checkDeadNodesTimer;
        
        NodeTable m_nodeTable;
        mutable NodeStatisticsMessage m_allStatisticsMessage;

        std::vector<StatisticsCallback> m_nodesChangedCallbacks;
        std::vector<StatisticsCallback> m_electionIdChangedCallbacks;
        std::vector<StatisticsCallback> m_rawChangedCallbacks;

        std::atomic<bool> m_stopped;
    };
}
}
}
}

#endif

