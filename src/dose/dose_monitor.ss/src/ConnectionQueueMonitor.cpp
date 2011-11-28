/******************************************************************************
*
* Copyright Saab AB, 2011(http://www.safirsdk.com)
*
* Created by: Anders Widén
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

#include "ConnectionQueueMonitor.h"
#include <Safir/Dob/Internal/Connections.h>
#include <Safir/Dob/Internal/RequestInQueue.h>
#include <Safir/Dob/Internal/MessageQueue.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>

#if defined(_WIN32)
#include <windows.h>
#endif

ConnectionQueueMonitor::ConnectionQueueMonitor(const int                         checkInterval,
                                               const boost::posix_time::seconds& maxQInactivityDuration,
                                               const int                         noStalledQueuesToKillDoseMain)
    : m_checkInterval(checkInterval),
      m_maxQInactivityDuration(maxQInactivityDuration),
      m_noStalledQueuesToKillDoseMain(noStalledQueuesToKillDoseMain),
      m_connStat(),
      m_qStatSummary()
{
}

ConnectionQueueMonitor::~ConnectionQueueMonitor()
{
}

void ConnectionQueueMonitor::Start()
{
    ACE_Thread::spawn(&ConnectionQueueMonitor::QCheckThread, this);
}

void ConnectionQueueMonitor::ProcessInQ(const InQMap::iterator&                      consumerIt,
                                        const InQStat&                               newInQStat,
                                        bool&                                        qIsStalled)
{
    consumerIt->second.isStalled = false;

    const boost::posix_time::ptime now = boost::posix_time::second_clock::universal_time();

    if (consumerIt->second.lastUpdateTime != boost::posix_time::ptime() && // Check that the queue has a valid last update time
        newInQStat.size > 0 &&
        newInQStat.noDispatched == consumerIt->second.noDispatched)
    {
        // The queue is not empty, but no dispatches has been made since the last update.
        // Update the stalled duration for this queue.
        consumerIt->second.stalledDuration += now - consumerIt->second.lastUpdateTime;

        if (consumerIt->second.stalledDuration > m_maxQInactivityDuration)
        {
            consumerIt->second.isStalled = true;
        }
    }
    else
    {
        consumerIt->second.stalledDuration = boost::posix_time::seconds(0);
    }

    consumerIt->second.noDispatched = newInQStat.noDispatched;
    consumerIt->second.size = newInQStat.size;
    consumerIt->second.lastUpdateTime = now;

    qIsStalled = consumerIt->second.isStalled;
}

void ConnectionQueueMonitor::ProcessReqInQ(const ConnStatMap::iterator&                 connIt,
                                           const Safir::Dob::Internal::ConsumerId&      consumer,
                                           Safir::Dob::Internal::RequestInQueue&        queue,
                                           bool&                                        queueIsStalled)
{
    InQStat newReqInQStat;

    // Get queue statistics
    newReqInQStat.noDispatched = queue.NumberOfDispatchedReq();
    newReqInQStat.size = queue.size();
   
    // Find the consumer queue
    InQMap::iterator consumerIt = connIt->second.reqInQStat.find(consumer);

    if (consumerIt != connIt->second.reqInQStat.end())
    {
        // Consumer queue exists
        ++m_qStatSummary.noReqInQueues;

        bool qIsStalled;

        ProcessInQ(consumerIt,
                   newReqInQStat,
                   qIsStalled);

        if (qIsStalled)
        {
            queueIsStalled = true;
            ++m_qStatSummary.noStalledReqInQueues;
        }
    }
    else
    {
        // Consumer queue doesn't exist, create it.
        connIt->second.reqInQStat.insert(std::make_pair(consumer, newReqInQStat));
    }
}

void ConnectionQueueMonitor::ProcessMsgInQ(const ConnStatMap::iterator&                 connIt,
                                           const Safir::Dob::Internal::ConsumerId&      consumer,
                                           Safir::Dob::Internal::MessageQueue&          queue,
                                           bool&                                        queueIsStalled)
{
    InQStat newMsgInQStat;

    // Get queue statistics
    newMsgInQStat.noDispatched = queue.NumberOfDispatchedMsg();
    newMsgInQStat.size = queue.size();
   
    // Find the consumer queue
    InQMap::iterator consumerIt = connIt->second.msgInQStat.find(consumer);

    if (consumerIt != connIt->second.msgInQStat.end())
    {
        // Consumer queue exists
        ++m_qStatSummary.noMsgInQueues;

        bool qIsStalled;

        ProcessInQ(consumerIt,
                   newMsgInQStat,
                   qIsStalled);

        if (qIsStalled)
        {
            queueIsStalled = true;
            ++m_qStatSummary.noStalledMsgInQueues;
        }
    }
    else
    {
        // Consumer queue doesn't exist, create it.
        connIt->second.msgInQStat.insert(std::make_pair(consumer, newMsgInQStat));
    }
}

void ConnectionQueueMonitor::ProcessConnection(const Safir::Dob::Internal::ConnectionPtr&   connection,
                                               ConnectionSet&                               existingConnections)
{
    if (!connection->IsLocal())
    {
        return;
    }

    existingConnections.insert(connection->Id());

    // Find connection
    ConnStatMap::iterator connIt = m_connStat.find(connection->Id());

    if (connIt == m_connStat.end())
    {
        // The connection doesn't exist, create it
        connIt = m_connStat.insert(std::make_pair(connection->Id(), ConnStat())).first; 
    }

    bool atLeastOneReqInQIsStalled = false;
    bool atLeastOneMsgInQIsStalled = false;

    connection->ForEachRequestInQueue(boost::bind(&ConnectionQueueMonitor::ProcessReqInQ,
                                                  this,
                                                  boost::cref(connIt),
                                                  _1,
                                                  _2,
                                                  boost::ref(atLeastOneReqInQIsStalled)));

    connection->ForEachMessageInQueue(boost::bind(&ConnectionQueueMonitor::ProcessMsgInQ,
                                                  this,
                                                  boost::cref(connIt),
                                                  _1,
                                                  _2,
                                                  boost::ref(atLeastOneMsgInQIsStalled)));

    if (atLeastOneReqInQIsStalled)
    {
        lllerr << "Connection " << connection->NameWithCounter() << " seems to have at least one request in queue that is stalled!" << std::endl;
    }
    if (atLeastOneMsgInQIsStalled)
    {
        lllerr << "Connection " << connection->NameWithCounter() << " seems to have at least one message in queue that is stalled!" << std::endl;
    }
}

void ConnectionQueueMonitor::UpdateQStatistics()
{
    // Clear queue summary
    m_qStatSummary = QStatSummary();

    ConnectionSet currentConnections;

    Safir::Dob::Internal::Connections::Instance().ForEachConnectionPtr
         (boost::bind(&ConnectionQueueMonitor::ProcessConnection,
                      this,
                      _1,
                      boost::ref(currentConnections)));

    // Remove connections that aren't present any more.
    for (ConnStatMap::iterator it = m_connStat.begin();
         it != m_connStat.end();)  // Note increment below to get it all correct when erasing via iterator
    {
        if (currentConnections.find(it->first) == currentConnections.end())
        {
            m_connStat.erase(it++);
        }
        else
        {
            ++it;
        }
    }
}

bool ConnectionQueueMonitor::Check()
{
    if (m_qStatSummary.noStalledReqInQueues + m_qStatSummary.noStalledMsgInQueues >= m_noStalledQueuesToKillDoseMain)
    {
        // Ok, this is considered serious. Maybe dose_main for some reason can't manage to kick the applications.

        KillDoseMain();  // !!!!!!!
        return false;       
    }
    return true;
}

void ConnectionQueueMonitor::FindDoseMainPid(const Safir::Dob::Internal::ConnectionPtr&   connection,
                                             int&                                         pid,
                                             bool&                                        pidFound) const
{
    std::string connectionName(connection->NameWithoutCounter());
    if (connectionName.find(";dose_main;") != connectionName.npos)
    {
        pid = connection->Pid();
        pidFound = true;
    }
}

bool ConnectionQueueMonitor::KillProcess(const int pid) const
{

#ifdef _WIN32

    DWORD access = PROCESS_TERMINATE;
    BOOL  inheritHandle  = FALSE;
    HANDLE hProcess = OpenProcess(access, inheritHandle, (DWORD)pid);
    if (hProcess == NULL)
        return FALSE;

    BOOL result = TerminateProcess(hProcess, 666);

    CloseHandle(hProcess);

    return result != 0;
#endif

//---------------------------------
#ifdef _LINUX

#endif

}

void ConnectionQueueMonitor::KillDoseMain() const
{
    int pid;
    bool pidFound = false;

    // Find dose_main pid
    Safir::Dob::Internal::Connections::Instance().ForEachConnectionPtr
         (boost::bind(&ConnectionQueueMonitor::FindDoseMainPid,
                      this,
                      _1,
                      boost::ref(pid),
                      boost::ref(pidFound)));

    if (!pidFound)
    {
        lllerr << "Failed to kill dose_main! The pid could not be found!" << std::endl;
        return;
    }

    bool killOk = KillProcess(pid);
    if (!killOk)
    {
        lllerr << "Failed to kill dose_main (pid=" << pid << ")!" << std::endl;
        return;
    }

    lllerr << "Killed dose_main since " << m_qStatSummary.noStalledReqInQueues << " (out of " << m_qStatSummary.noReqInQueues << ") request in queues "
              " and " << m_qStatSummary.noStalledMsgInQueues << " (out of " << m_qStatSummary.noMsgInQueues << ") message in queues seem to be stalled!" << std::endl;
}

ACE_THR_FUNC_RETURN ConnectionQueueMonitor::QCheckThread(void * _this)
{
    ConnectionQueueMonitor* This = static_cast<ConnectionQueueMonitor*>(_this);

    try
    {
        for (;;)
        {
            This->UpdateQStatistics();
            if (!This->Check())
            {
                // It seems that just breaking out from the loop in this thread isn't detected
                // by the main loop. This has to be investigated. For now an exit will do the job.
                std::wcout << "Have killed dose_main! I'm stopping execution ..." << std::endl; 
                exit(0);
                break;
            }
            ACE_OS::sleep(ACE_Time_Value(This->m_checkInterval));
        }
    }
    catch(std::exception & e)
    {
        lllerr << "ConnectionQueueMonitor::QCheckThread Caught std::exception! Contents of exception is:\n"
            << e.what() << std::endl;
    }
    catch (...)
    {
        lllerr << "ConnectionQueueMonitor::QCheckThread ... exception!\n" << std::endl;
    }
    return 0;
}
