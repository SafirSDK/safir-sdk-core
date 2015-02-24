/******************************************************************************
*
* Copyright Consoden AB, 2015 (http://safir.sourceforge.net)
*
* Created by: Anders Widén / anders.widen@consoden.se
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

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4267)
#endif

#include <boost/asio.hpp>

#ifdef _MSC_VER
#pragma warning (pop)
#endif

#include <Safir/Dob/Internal/StateDeleter.h>
#include <Safir/Dob/Internal/DistributionData.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>

#include "Distribution.h"

namespace Safir
{
namespace Dob
{
namespace Internal
{

    typedef std::function<void(const DistributionData& data, const bool isAckedData)> IncomingDataCallback;
    typedef std::function<void(void)> StartPoolDistributionCallback;

    template <typename DistributionT>
    class ExternNodeCommunicationBasic:
        private boost::noncopyable
    {
    public:

        ExternNodeCommunicationBasic(boost::asio::io_service::strand&        receiveStrand,
                                     const IncomingDataCallback&             handleDataCb,
                                     const StartPoolDistributionCallback&    startPoolDistributionCb)
            : m_distribution(),
              m_receiveStrand(receiveStrand),
              m_handleDataCb(handleDataCb),
              m_startPoolDistributionCb(startPoolDistributionCb)
        {

        }

        void SetOwnNode(const std::string&        ownNodeName,
                        int64_t                   ownNodeId,
                        int64_t                   ownNodeTypeId,
                        const std::string&        ownDataAddress)
        {
            m_distribution.reset(new DistributionT(m_receiveStrand.get_io_service(),
                                                   ownNodeName,
                                                   ownNodeId,
                                                   ownNodeTypeId,
                                                   ownDataAddress,
                                                   m_receiveStrand.wrap(
                                                       [this](int64_t fromNodeId,
                                                              int64_t fromNodeType,
                                                              const boost::shared_ptr<char[]>& data,
                                                              size_t size)
                                                       {
                                                           HandleIncomingData(fromNodeId,
                                                                              fromNodeType,
                                                                              data,
                                                                              size);
                                                       })));
        }

        void InjectNode(const std::string& nodeName,
                        int64_t            nodeId,
                        int64_t            nodeTypeId,
                        const std::string& dataAddress)
        {
            if (m_distribution == nullptr)
            {
                std::ostringstream os;
                os << "DOSE_MAIN: ExternNodeCommunication::InjectNode called before SetOwnNode";
                throw std::logic_error(os.str());
            }

            m_distribution->InjectNode(nodeName,
                                       nodeId,
                                       nodeTypeId,
                                       dataAddress);
        }

    private:

        std::unique_ptr<DistributionT> m_distribution;

        boost::asio::io_service::strand&    m_receiveStrand;

        const IncomingDataCallback&             m_handleDataCb;
        const StartPoolDistributionCallback&    m_startPoolDistributionCb;

        void HandleIncomingData(int64_t fromNodeId,
                                int64_t fromNodeType,
                                const boost::shared_ptr<char[]>& data,
                                size_t size)
        {
            // TODO: In the future the data chunk given in this callback should already be in shared memory.
            //       Change this when Communication is given this ability.

            // Create shm chunk
            auto shmData = DistributionData::NewData(size);

            std::memcpy(shmData, data.get(), size);

            DistributionData msg(new_data_tag, shmData);

            // The data reference count is now 2, drop it to 1.
            DistributionData::DropReference(shmData);

            lllog(9) << "DOSE_MAIN: Msg RECEIVED from node id " << fromNodeId
                     << " with node type " << fromNodeType << '\n'
                     <<  msg.Image() << std::endl;

        }



    };

    typedef ExternNodeCommunicationBasic<Distribution> ExternNodeCommunication;

}
}
}

#if 0 //stewart

#ifndef _dose_main_communication_h
#define _dose_main_communication_h

#include <string>
#include <boost/shared_ptr.hpp>
#include <boost/noncopyable.hpp>

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4267)
#endif

#include <boost/asio.hpp>

#ifdef _MSC_VER
#pragma warning (pop)
#endif

#include "dose_main_quality_of_service_data.h"
#include "dose_main_thread_monitor.h"
#include <Safir/Dob/Internal/DistributionData.h>
#include <DoseCom_Interface_Classes.h>
#include <boost/scoped_array.hpp>
#include <boost/thread/mutex.hpp>

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4267)
#endif

#include <boost/asio.hpp>

#ifdef _MSC_VER
#pragma warning (pop)
#endif

namespace Safir
{
namespace Dob
{
namespace Internal
{

    class ExternNodeCommunication:
        public DoseComNotificationHandler,
        private boost::noncopyable
    {
    public:
        typedef boost::function<void(const DistributionData & data, const bool isAckedData)> IncomingDataCallback;
        typedef boost::function<void(void)> QueueNotFullCallback;
        typedef boost::function<void(void)> NodeStatusChangedNotifierCallback;
        typedef boost::function<void(void)> StartPoolDistributionCallback;
        typedef boost::function<void(const int nodeId)> RequestPoolDistributionCallback;

        explicit ExternNodeCommunication(boost::asio::io_service & ioService);

        bool Init (const IncomingDataCallback & dataCb,
                   const QueueNotFullCallback & queueNotFullCb,
                   const NodeStatusChangedNotifierCallback & nodeStatusChangedNotifierCb,
                   const StartPoolDistributionCallback & startPoolDistributionCb,
                   const RequestPoolDistributionCallback & requestPoolDistributionCallback);

        bool Send(const DistributionData & msg);

        //this will block if there is overflow
        void SendPoolDistributionData(const DistributionData & msg, ThreadMonitor& threadMonitor, const boost::thread::id& threadId);
        //this will block if necessary.
        void PoolDistributionCompleted(ThreadMonitor& threadMonitor, const boost::thread::id& threadId);

        // tell DoseCom that nodeId need a pool distribution. 
        void ForcePoolDistribution(const int nodeId);


        //Check if type shall be distributed (returns true if system is standalone too)
        bool IsLocal(Dob::Typesystem::TypeId tid) const;

        const QualityOfServiceData & GetQualityOfServiceData() const {return m_QualityOfServiceData;}

        std::wstring GetOwnIpAddress() const;
        std::wstring IpAddressToString(const unsigned long ipAddr) const;

        void SetOkToSignalPDComplete() {m_okToSignalPDComplete = 1;}

        static const Identifier DoseComVirtualConnectionId;

    private:
        const Dob::Typesystem::Int32 m_thisNode;

        bool ShouldBeDiscarded(const DistributionData & msg);

        void HandleEvents();
        void HandleIncomingData(const int startFromPriority);

        virtual void NotifyIncomingData(const int priorityChannel);
        virtual void NotifyQueueNotFull(const int priorityChannel);
        virtual void NotifyNodeStatusChanged();
        virtual void NotifyStartPoolDistribution();
        virtual void NotifyRequestPoolDistribution(const int nodeId);

        IncomingDataCallback m_handleDataCb;
        QueueNotFullCallback m_queueNotFullCb;
        NodeStatusChangedNotifierCallback m_nodeStatusChangedNotifierCb;
        StartPoolDistributionCallback m_startPoolDistributionCb;
        RequestPoolDistributionCallback m_requestPoolDistributionCallback;


        QualityOfServiceData m_QualityOfServiceData;

        boost::scoped_array<Safir::Utilities::Internal::AtomicUint32> m_queueIsFull;
        boost::mutex m_queueIsFullLock;

        Safir::Utilities::Internal::AtomicUint32 m_okToSignalPDComplete;
        Safir::Utilities::Internal::AtomicUint32 m_isNotified;

        int m_pdChannel;
        int m_pdPriority;
        bool m_pdIsAcked;

        boost::scoped_array<Safir::Utilities::Internal::AtomicUint32> m_incomingDataEvents;
        boost::scoped_array<Safir::Utilities::Internal::AtomicUint32> m_requestPDEvents; // Request pooldistribution from these nodes.
        Safir::Utilities::Internal::AtomicUint32 m_queueNotFullEvent;
        Safir::Utilities::Internal::AtomicUint32 m_startPoolDistributionEvent;

        boost::asio::io_service& m_ioService;
    };
}
}
}
#endif
#endif

