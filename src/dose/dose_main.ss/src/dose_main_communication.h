/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
*
* Created by: Lars Hagström / stlrha
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

#ifndef _dose_main_communication_h
#define _dose_main_communication_h

#include "dose_main_quality_of_service_data.h"
#include "dose_main_thread_monitor.h"
#include <Safir/Dob/Internal/DistributionData.h>
#include <Safir/Dob/Internal/DoseCom_Interface_Classes.h>
#include <boost/scoped_array.hpp>
#include <boost/thread/mutex.hpp>
#include <boost/asio.hpp>

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
        void SendPoolDistributionData(const DistributionData & msg, boost::shared_ptr<ThreadMonitor> threadMonitorPtr, const boost::thread::id& threadId);
        //this will block if necessary.
        void PoolDistributionCompleted(boost::shared_ptr<ThreadMonitor> threadMonitorPtr, const boost::thread::id& threadId);

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

        boost::scoped_array<AtomicUint32> m_queueIsFull;
        boost::mutex m_queueIsFullLock;

        AtomicUint32 m_okToSignalPDComplete;
        AtomicUint32 m_isNotified;

        int m_pdChannel;
        int m_pdPriority;
        bool m_pdIsAcked;

        boost::scoped_array<AtomicUint32> m_incomingDataEvents;
        boost::scoped_array<AtomicUint32> m_requestPDEvents; // Request pooldistribution from these nodes.
        AtomicUint32 m_queueNotFullEvent;
        AtomicUint32 m_startPoolDistributionEvent;

        boost::asio::io_service& m_ioService;
    };
}
}
}

#endif

