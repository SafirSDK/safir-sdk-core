/******************************************************************************
*
* Copyright Saab AB, 2011 (http://www.safirsdk.com)
*
* Created by: Anders Widén / stawi
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
#ifndef CONNECTIONQUEUEMONITOR_H
#define CONNECTIONQUEUEMONITOR_H

#include <Safir/Dob/Typesystem/Defs.h>
#include <Safir/Dob/Internal/ConsumerId.h>
#include <Safir/Dob/Internal/Connection.h>
#include <Safir/Dob/Internal/ConnectionId.h>
#include <map>

#if defined _MSC_VER
  #pragma warning (push)
  #pragma warning (disable : 4127)
#endif
#include <boost/date_time/posix_time/posix_time.hpp>
#if defined _MSC_VER
  #pragma warning (pop)
#endif

class ConnectionQueueMonitor
{
public:

    ConnectionQueueMonitor(const int                         checkInterval,
                           const boost::posix_time::seconds& maxQInactivityTime,
                           const int                         noStalledQueuesToKillDoseMain);
    ~ConnectionQueueMonitor();

    void Start();

private:

    void UpdateQStatistics();

    bool Check();

    int                         m_checkInterval;
    boost::posix_time::seconds  m_maxQInactivityDuration;
    int                         m_noStalledQueuesToKillDoseMain;

    struct InQStat
    {
        InQStat() :
            noDispatched(0),
            size(0),
            isStalled(false),
            stalledDuration(0),
            lastUpdateTime() {};

        Safir::Dob::Typesystem::Int32       noDispatched;
        size_t                              size;
        bool                                isStalled;
        boost::posix_time::seconds          stalledDuration;
        boost::posix_time::ptime            lastUpdateTime;
    };

    typedef std::map<Safir::Dob::Internal::ConsumerId, InQStat> InQMap;

    struct ConnStat
    {
        InQMap      reqInQStat;
        InQMap      msgInQStat;                  
    };

    typedef std::map<Safir::Dob::Internal::ConnectionId, ConnStat> ConnStatMap;

    ConnStatMap m_connStat;

    struct QStatSummary
    {
        QStatSummary() :
            noReqInQueues(0),
            noStalledReqInQueues(0),
            noMsgInQueues(0),
            noStalledMsgInQueues(0){};

        Safir::Dob::Typesystem::Int32   noReqInQueues;
        Safir::Dob::Typesystem::Int32   noStalledReqInQueues;
        Safir::Dob::Typesystem::Int32   noMsgInQueues;
        Safir::Dob::Typesystem::Int32   noStalledMsgInQueues;
    };

    QStatSummary                m_qStatSummary;
    
    typedef std::set<Safir::Dob::Internal::ConnectionId> ConnectionSet;
    
    static ACE_THR_FUNC_RETURN QCheckThread(void *);

    void ProcessInQ(const InQMap::iterator&                      consumerIt,
                    const InQStat&                               newInQStat,
                    bool&                                        qIsStalled);
                    

    void ProcessReqInQ(const ConnStatMap::iterator&                 connIt,
                       const Safir::Dob::Internal::ConsumerId&      consumer,
                       Safir::Dob::Internal::RequestInQueue&        queue,
                       bool&                                        queueIsStalled);

    void ProcessMsgInQ(const ConnStatMap::iterator&                 connIt,
                       const Safir::Dob::Internal::ConsumerId&      consumer,
                       Safir::Dob::Internal::MessageQueue&          queue,
                       bool&                                        queueIsStalled);

    void ProcessConnection(const Safir::Dob::Internal::ConnectionPtr&   connection,
                           ConnectionSet&                               existingConnections);

    bool KillProcess(const int pid) const;

    void FindDoseMainPid(const Safir::Dob::Internal::ConnectionPtr&   connection,
                         int&                                         pid,
                         bool&                                        pidFound) const;

    void KillDoseMain() const;

};


#endif
