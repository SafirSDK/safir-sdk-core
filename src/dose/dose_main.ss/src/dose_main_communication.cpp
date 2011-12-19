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

#include "dose_main_communication.h"


#include "dose_main_defs.h"
#include <Safir/Dob/Internal/Connection.h>
#include <Safir/Dob/Internal/ServiceTypes.h>
#include <Safir/Dob/Internal/EntityTypes.h>
#include <Safir/Dob/Internal/DoseCom_Interface.h>
#include <Safir/Dob/Internal/ContextSharedTable.h>
#include <Safir/Dob/NodeParameters.h>
#include <Safir/Dob/ThisNodeParameters.h>
#include <Safir/Dob/Typesystem/Internal/InternalUtils.h>
#include <Safir/Dob/Typesystem/Utilities.h>
#include <Safir/Dob/Typesystem/Operations.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <ace/Guard_T.h>
#include <boost/bind.hpp>
#include <boost/scoped_ptr.hpp>
#include <ace/Reactor.h>
#include <ace/Thread.h>
#include <ace/OS_NS_unistd.h>

//uncomment this to get all headers logged to lllout
//#define LOG_HEADERS

namespace Safir
{
namespace Dob
{
namespace Internal
{

    namespace
    {
        BOOST_STATIC_ASSERT(sizeof(dcom_ulong32) == 4);
        BOOST_STATIC_ASSERT(sizeof(dcom_ulong64) == 8);
    }

#if 0 //DEBUG DoseCom memory handling
    struct MemData
    {
        MemData(const size_t s, const std::string& w): size(s), where(w), count(1) {}
        size_t size;
        std::string where;
        int count;
    };

    std::map<const void*,MemData> memMap;
    std::map<const void*,std::string> removedMem;
    ACE_Thread_Mutex memMapLock;

    ACE_THR_FUNC_RETURN dcom_memory_monitor(void *)
    {
        std::map<const void*, MemData> last;
        for(;;)
        {
            ACE_OS::sleep(30);

            size_t totalMem = 0;
            size_t blocks = 0;

            {
                ACE_Guard<ACE_Thread_Mutex> lck(memMapLock);
#if 0 //display all memory known by dose_com
                lllinfo << "DoseCom memory info: " << std::endl;

                for (std::map<const void*,MemData>::iterator it = memMap.begin();
                     it != memMap.end(); ++it)
                {
                    ++blocks;
                    totalMem += it->second.size;
                    lllinfo << "  " << it->second.size << " bytes allocated in " << it->second.where.c_str()
                           << ": " << std::hex <<it->first << std::dec << ", count = " << it->second.count << std::endl;
                }
                lllinfo << " Total: " << std::dec << blocks << " blocks allocated, total size = " << totalMem/1000 << " Kbytes" << std::endl;

                blocks = 0;
                totalMem = 0;

#endif
                lllinfo << "Potential leaks: " << std::endl;
                std::map<const void*,MemData> intersection;
                std::set_intersection(memMap.begin(),memMap.end(),
                                      last.begin(),last.end(),
                                      std::inserter(intersection,intersection.begin()),
                                      memMap.value_comp());

                for (std::map<const void*,MemData>::iterator it = intersection.begin();
                     it != intersection.end(); ++it)
                {
                    ++blocks;
                    totalMem += it->second.size;
                    lllinfo << "  " << it->second.size << " bytes allocated in " << it->second.where.c_str()
                           << ": " << std::hex <<it->first << std::dec << ", count = " << it->second.count << std::endl;
                }
                lllinfo << " Total: " << std::dec << blocks << " blocks allocated, total size = " << totalMem/1000 << " Kbytes" << std::endl;
#if 0
                static int numTimesWithLeaks = 0;
                if (blocks > 10)
                {
                    ++numTimesWithLeaks;
                    if (numTimesWithLeaks > 3)
                    {
                        exit(0);
                    }
                }
                else
                {
                    numTimesWithLeaks = 0;
                }
#endif
                last = memMap;
            }

            //lllinfo << "DoseCom has references to " << std::dec << blocks << " blocks, total size = " << totalMem/1000 << " Kbytes" << std::endl;


        }
#ifndef _MSC_VER
        return 0;
#endif
    }

    void StartMemoryMonitor()
    {
        ACE_Thread::spawn(dcom_memory_monitor,NULL);
    }

    void MemMapAdd(const char * const mem, const size_t size, const char * const where)
    {
        ACE_Guard<ACE_Thread_Mutex> lck(memMapLock);
        const std::pair<std::map<const void*, MemData>::iterator, bool> result =
            memMap.insert(std::make_pair(mem,MemData(size,where)));

        if (!result.second)
        {
            ++result.first->second.count;
        }
    }

    void MemMapRemove(const char * const mem, const char * const where)
    {
        if (mem != NULL)
        {
            ACE_Guard<ACE_Thread_Mutex> lck(memMapLock);
            std::map<const void*,MemData>::iterator it = memMap.find(mem);
            if (it == memMap.end())
            {
                const std::map<const void*, std::string>::const_iterator remIt = removedMem.find(mem);
                if (remIt == removedMem.end())
                {
                    lllinfo << "MemMapRemove called from " << where << " for unknown memory! " << std::hex<< (const void *)mem<< std::endl;
                }
                else
                {
                    lllinfo << "MemMapRemove called from " << where << " for mem deallocated in " << remIt->second.c_str()
                           << ". use_count = " << *((const boost::uint32_t *)(mem - 4)) << std::endl;
                }
            }
            else
            {
                if (it->second.count == 1)
                {
                    if (removedMem.size() > 1000000)
                    {
                        lllinfo << "Clearing removedMem, so we dont run out of memory" << std::endl;
                        removedMem.clear();
                    }
                    removedMem.insert(std::make_pair(it->first,where));
                    memMap.erase(it);
                }
                else
                {
                    --it->second.count;
                }
            }
        }
    }
#else
    void MemMapAdd(const char * const, const size_t, const char * const) {}
    void MemMapRemove(const char * const, const char * const) {}
    void StartMemoryMonitor() {}
#endif

    class MyAllocator:
        public DoseComAllocator
    {
    public:
        // returns NULL if it is not possible to allocate memory.
        virtual char * Allocate(const size_t size)
        {
            char * mem = DistributionData::NewData(size);
            MemMapAdd(mem,size, "Allocate");
            return mem;
        }

        // Undefined result if called with a buffer that was not successfully
        // allocated with Allocate although it is ok to call it with pBuf = NULL
        virtual void Deallocate(char * data)
        {
            if (data != NULL)
            {
                MemMapRemove(data,"Deallocate");
                DistributionData::DropReference(data);
            }
        }
    };

    //TODO: move this one to header.
    static MyAllocator myAllocator;

    const Identifier ExternNodeCommunication::DoseComVirtualConnectionId = Connection::CalculateIdentifier("dose_com_virtual_conn");

    const std::deque<bool> NO_CHANNELS_HAVE_DATA(NUM_PRIORITY_CHANNELS,false);

    namespace
    {
        BOOST_STATIC_ASSERT(NUM_PRIORITY_CHANNELS == MAX_NUM_PRIO_CHANNELS);
    }

    const Typesystem::Int32 NumberOfNodes = Safir::Dob::NodeParameters::NumberOfNodes();

    ExternNodeCommunication::ExternNodeCommunication():
        m_thisNode(Safir::Dob::ThisNodeParameters::NodeNumber()),
        m_queueIsFull(new AtomicUint32 [NUM_PRIORITY_CHANNELS]),
        m_okToSignalPDComplete(0),
        m_isNotified(0),
        m_incomingDataEvents(new AtomicUint32 [NUM_PRIORITY_CHANNELS]),
        m_requestPDEvents(new AtomicUint32 [NumberOfNodes]),
        m_queueNotFullEvent(0),
        m_startPoolDistributionEvent(0)
    {

        for (int i = 0; i< NUM_PRIORITY_CHANNELS; ++i)
        {
            m_queueIsFull[i] = 0;
            m_incomingDataEvents[i] = 0;
        }
    }

    bool ExternNodeCommunication::Init(const IncomingDataCallback & dataCb,
                                       const QueueNotFullCallback & queueNotFullCb,
                                       const NodeStatusChangedNotifierCallback & nodeStatusChangedNotifierCb,
                                       const StartPoolDistributionCallback & startPoolDistributionCb,
                                       const RequestPoolDistributionCallback & requestPoolDistributionCallback)
    {
        m_handleDataCb = dataCb;
        m_queueNotFullCb = queueNotFullCb;
        m_nodeStatusChangedNotifierCb = nodeStatusChangedNotifierCb;
        m_startPoolDistributionCb = startPoolDistributionCb;
        m_requestPoolDistributionCallback = requestPoolDistributionCallback;

        m_QualityOfServiceData.Init();

        if (m_QualityOfServiceData.IsStandalone())
        {
            std::wcout << "Running in Standalone mode" <<std::endl;
            return false;
        }

        std::wcout << "Running in multi node mode. Configuration is expected to contain " << NodeParameters::NumberOfNodes() << " nodes." <<std::endl;

        for (DistributionChannelTable::const_iterator it = m_QualityOfServiceData.GetDistributionChannelTable().begin();
             it != m_QualityOfServiceData.GetDistributionChannelTable().end(); ++it)
        {
            const int result =
                DoseCom_Add_DestinationId
                    (it->first,it->second.multicastAddress.c_str(),it->second.includedNodes);
            ENSURE(result == ERR_DOSECOM_OK, << L"DoseCom_Add_DestinationId failed with error code: " << result);
        }

        std::string netAddr;
        try
        {
            netAddr = Dob::Typesystem::Utilities::ToUtf8(NodeParameters::NetworkAddress());
        }
        catch (const Dob::Typesystem::NullException &)
        {
            throw Dob::Typesystem::ConfigurationErrorException
                (L"NetworkAddress must be defined in NodeConfig",__WFILE__,__LINE__);
        }

        const int result = DoseCom_Init(&myAllocator,
                                        m_QualityOfServiceData.GetDistributionChannelTable().find(64)->second.multicastAddress.c_str(),
                                        NodeParameters::RoutingHops(),

                                        netAddr.c_str(),
                                        m_thisNode,
                                        this);

        ENSURE(result == ERR_DOSECOM_OK ||
               result == ERR_DOSECOM_OTHER_EXISTS,
               << L"DoseCom_Init failed with error code: " << result);

        m_QualityOfServiceData.
            GetQualityOfServiceInfoForPoolDistribution(m_pdChannel,m_pdPriority, m_pdIsAcked);
        StartMemoryMonitor();

        return result == ERR_DOSECOM_OTHER_EXISTS; //return true if there are other nodes in the system
    }

    //this will block if there is overflow
    void ExternNodeCommunication::SendPoolDistributionData(const DistributionData & msg, ThreadMonitor& threadMonitor, const boost::thread::id& threadId)
    {
        assert (!m_QualityOfServiceData.IsStandalone());

        bool success = false;

        while (!success)
        {
            threadMonitor.KickWatchdog(threadId);

            const DistributionData::Type type = msg.GetType();

            //check for things that should not be sent (i.e. local messages)
            if (type != DistributionData::Action_Connect &&
                type != DistributionData::Action_Disconnect)
            {
                int distributionChannel;
                int dummyInt;
                bool dummyBool;
                m_QualityOfServiceData.GetQualityOfServiceInfo(msg.GetTypeId(),
                                                               distributionChannel,dummyInt,dummyBool);
                //if the distribution channel is "local", dont send the object
                if (m_QualityOfServiceData.IsLocal(distributionChannel))
                {
                    return;
                }
            }

            if (m_queueIsFull[m_pdPriority] != 0)
            {
                ACE_OS::sleep(ACE_Time_Value(0,1000)); //1 millisecond
                continue;
            }

            const char * extRef = msg.GetReference();

            MemMapAdd(extRef,msg.Size(), "SendPoolDistributionData");

            //must lock the m_queueIsFullLock so that the notfull event doesnt trigger before
            //we've set the m_queueIsFull variable.
            ACE_Guard<ACE_Thread_Mutex> lck(m_queueIsFullLock);
            const int errCode = DoseCom_Send(extRef,
                                             static_cast<unsigned long>(msg.Size()),
                                             true,
                                             m_pdIsAcked,
                                             m_pdPriority,
                                             m_pdChannel);
            if (errCode==ERR_DOSECOM_OK)
            {

    #ifdef LOG_HEADERS
                lllout << L"Msg SENT to external node (pool distribution)" << std::endl
                       << msg.Image() << std::endl << std::endl;
    #endif
                success = true;
            }
            else if (errCode==ERR_DOSECOM_OVERFLOW)
            {
                MemMapRemove(extRef,"SendPoolDistributionData");

                // lllout << "Overflow in Pooldistribution send to DoseCom" << std::endl;
                DistributionData::DropReference(extRef);

                m_queueIsFull[m_pdPriority] = 1;
            }
            else
            {
                ENSURE(false, << "ExternNodeCommunication::SendPoolDistributionData, DoseCom returned error code "<<errCode);
            }
        }
    }


    void ExternNodeCommunication::PoolDistributionCompleted(ThreadMonitor& threadMonitor, const boost::thread::id& threadId)
    {
        lllout << "Waiting for m_okToSignalPDComplete to be set to true. Current value = " << m_okToSignalPDComplete.value() << std::endl;
        //there is logic in dose_main_app.cpp that will tell us when/if it is okay to finish pd.
        while (m_okToSignalPDComplete == 0)
        {
            threadMonitor.KickWatchdog(threadId);
            lllout << "Waiting for m_okToSignalPDComplete to be set to true. Current value = " << m_okToSignalPDComplete.value() << std::endl;
            ACE_OS::sleep(ACE_Time_Value(0,20000)); //20 milliseconds
        }
        lllout << "m_okToSignalPDComplete is true!" << std::endl;

        // m_queueIsFullLock used to prevent dose_com_send to interfere with this message.
        ACE_Guard<ACE_Thread_Mutex> lck(m_queueIsFullLock);
        DoseCom_PoolDistributed(m_pdPriority,m_pdChannel);

        lllout << "Pool distribution completed" << std::endl;
    }

    void ExternNodeCommunication::ForcePoolDistribution(const int nodeId)
    {
        lllout << "ForcePoolDistribution to node " << nodeId << std::endl;
        DoseCom_ForcePoolDistribution(nodeId);
    }


    bool ExternNodeCommunication::IsLocal(Dob::Typesystem::TypeId tid) const
    {
        if (m_QualityOfServiceData.IsStandalone())
        {
            return true;
        }
        int dc, p;
        bool ack;
        m_QualityOfServiceData.GetQualityOfServiceInfo(tid, dc, p, ack);
        return m_QualityOfServiceData.IsLocal(dc);
    }

    bool ExternNodeCommunication::Send(const DistributionData & data)
    {
        DistributionData::Type dataType = data.GetType();

        if (m_QualityOfServiceData.IsStandalone())
        {
            return true;
        }

        //Get the QoS data
        int distributionChannel, priority;
        bool isAcked;
        if (dataType == DistributionData::Action_Connect ||
            dataType == DistributionData::Action_Disconnect ||
            dataType == DistributionData::Action_HavePersistenceDataRequest ||
            dataType == DistributionData::Action_HavePersistenceDataResponse ||
            dataType == DistributionData::Action_RequestPoolDistribution)
        {
            m_QualityOfServiceData.GetQualityOfServiceInfoForInternalDistribution(distributionChannel,
                                                                                  priority,
                                                                                  isAcked);

            // Request PD shall only be sent unicast to one node.
            if (dataType == DistributionData::Action_RequestPoolDistribution)
            {
                distributionChannel = data.GetPDRequestReceiverId().m_node;
            }
        }
        else
        {
            m_QualityOfServiceData.GetQualityOfServiceInfo(data.GetTypeId(),
                                                           distributionChannel,
                                                           priority,
                                                           isAcked);

            //for requests, responses, registration states and delete states we need to modify this data a bit
            //e.g. move to other distribution channel or use an acked priority
            if (dataType == DistributionData::Request_EntityCreate ||
                dataType == DistributionData::Request_EntityUpdate ||
                dataType == DistributionData::Request_EntityDelete ||
                dataType == DistributionData::Request_Service ||
                dataType == DistributionData::Response ||
                dataType == DistributionData::RegistrationState ||
                (dataType == DistributionData::EntityState &&
                (!data.IsCreated() || data.GetEntityStateKind() == DistributionData::Injection)))
            {
                if (!isAcked)
                {
                    priority = m_QualityOfServiceData.GetClosestAckedPriority(priority);
                    isAcked = true;
                }

                //use a singlecast distribution channel if possible
                switch (dataType)
                {
                case DistributionData::Response:
                    {
                        distributionChannel = data.GetReceiverId().m_node;
                    }
                    break;

                case DistributionData::Request_Service:
                    {
                        ContextId context;
                        if (ContextSharedTable::Instance().IsContextShared(data.GetTypeId()))
                        {
                            context = 0;
                        }
                        else
                        {
                            context = data.GetSenderId().m_contextId;
                        }

                        distributionChannel = ServiceTypes::Instance().
                            GetRegisterer(data.GetTypeId(),
                                          data.GetHandlerId(),
                                          context).connection->Id().m_node;
                    }
                    break;

                case DistributionData::Request_EntityCreate:
                case DistributionData::Request_EntityUpdate:
                case DistributionData::Request_EntityDelete:
                    {
                        ContextId context;
                        if (ContextSharedTable::Instance().IsContextShared(data.GetTypeId()))
                        {
                            context = 0;
                        }
                        else
                        {
                            context = data.GetSenderId().m_contextId;
                        }

                        distributionChannel = EntityTypes::Instance().
                            GetRegisterer(data.GetTypeId(),
                                          data.GetHandlerId(),
                                          context).connection->Id().m_node;
                    }
                    break;

                default:
                    break;
                }
            }
        }

        //if the distribution channel is "local", dont send the object
        if (m_QualityOfServiceData.IsLocal(distributionChannel))
        {
            return true;
        }

        if (m_queueIsFull[priority] != 0)
        {
#ifdef LOG_HEADERS
            lllout << L"While in Overflow: Wanted to send msg to external node" << std::endl
                   << data.Image() << std::endl << std::endl;
#endif
            return false;
        }

        const char * extRef = data.GetReference();

        MemMapAdd(extRef,data.Size(), "Send");

        //must lock the m_queueIsFullLock so that the notfull callback doesnt trigger before
        //we've set the m_queueIsFull variable.
        ACE_Guard<ACE_Thread_Mutex> lck(m_queueIsFullLock);

        const int errCode=DoseCom_Send(extRef,
                                       static_cast<unsigned long>(data.Size()),
                                       false,
                                       isAcked,
                                       priority,
                                       distributionChannel);

        switch (errCode)
        {
        case ERR_DOSECOM_OK:

#ifdef LOG_HEADERS
                lllout << L"Msg SENT to external node (DC = " << distributionChannel << ", Prio = " << priority << ", isAcked = " << isAcked << ")" << std::endl
                       << data.Image() << std::endl << std::endl;
#endif
            return true;

        case ERR_DOSECOM_OVERFLOW:

            MemMapRemove(extRef,"Send");
            // lllout << "Overflow in send to DoseCom" << std::endl;
            DistributionData::DropReference(extRef);


            m_queueIsFull[priority] = 1;

            return false;
        default:
            ENSURE(false, << "ExternNodeCommunication::Send, DoseCom returned error code "<<errCode);
            return false; //keep compiler happy
        }
    }


    bool ExternNodeCommunication::ShouldBeDiscarded(const DistributionData & data)
    {
        const DistributionData::Type dataType = data.GetType();

        switch (dataType)
        {
        case DistributionData::Action_Connect:
        case DistributionData::Action_Disconnect:
        case DistributionData::Action_HavePersistenceDataRequest:
        case DistributionData::Action_HavePersistenceDataResponse:
        case DistributionData::Action_RequestPoolDistribution:
            return false;

        default:
            //Does the type exist on this node or is it Local (ie it shouldnt be received)
            if (m_QualityOfServiceData.IsNodeInDistributionChannel
                (data.GetTypeId(),
                 m_thisNode))
            {
                return false;
            }
            else
            {
                lllout << "Discarding received SHM_MSG containing " <<
                    Safir::Dob::Typesystem::Operations::GetName(data.GetTypeId()) << " since it is local or this node is not in its distribution channel" << std::endl;
                return true;
            }
        }
    }

    int ExternNodeCommunication::handle_exception(ACE_HANDLE)
    {
        m_isNotified = 0;
        if (m_queueNotFullEvent != 0)
        {
            m_queueNotFullEvent = 0;
            m_queueNotFullCb();
        }

        for (int i = 0; i < NUM_PRIORITY_CHANNELS; ++i)
        {
            if (m_incomingDataEvents[i] != 0)
            {
                m_incomingDataEvents[i] = 0;
                HandleIncomingData(i);
            }
        }

        for (int nodeId = 0; nodeId < NumberOfNodes; ++nodeId)
        {
            if (m_requestPDEvents[nodeId] != 0)
            {
                m_requestPDEvents[nodeId] = 0;
                m_requestPoolDistributionCallback(nodeId);
            }
        }
        

        if (m_startPoolDistributionEvent != 0)
        {
            m_startPoolDistributionEvent = 0;
            m_startPoolDistributionCb();
        }
        return 0;
    }

    int GetChannelNo(const unsigned long channelBitMap)
    {
        for (int bitToTest = 0; bitToTest < NUM_PRIORITY_CHANNELS; ++bitToTest)
        {
            const unsigned long mask = 1 << bitToTest;

            if (channelBitMap & mask)
            {
                assert ((channelBitMap ^ mask) == 0); //check that one and only one bit is set
                return bitToTest;
            }
        }
        throw Safir::Dob::Typesystem::SoftwareViolationException(L"Got to end of GetChannelNo",__WFILE__,__LINE__);
    }

    void ExternNodeCommunication::HandleIncomingData(const int currentChannel)
    {
        ENSURE (!m_QualityOfServiceData.IsStandalone(),
                << L"Should not get any incoming data when in standalone mode");

        int numReadsFromThisChannel = 0;
        for(;;)
        {
            const unsigned long prioMask = 1<<currentChannel;

            dcom_ulong32 size;
            bool isNative;
            dcom_ulong32 fromChannel;
            char * data;
            int errCode=DoseCom_Read(prioMask, &fromChannel, &data, &size, &isNative);

            if (errCode==ERR_DOSECOM_OK)
            {
                ENSURE (fromChannel != 0, << "DoseCom_Read unexpectedly gave 0 value for fromChannel when reading message!");
                MemMapRemove(data,"HandleIncomingData");

                DistributionData msg(new_data_tag,data);

                DistributionData::DropReference(data);

#ifdef LOG_HEADERS
                lllout << L"Msg RECEIVED from external node (DC = " << fromChannel
                       << ", prio = " << currentChannel
                       << ", isNative = " << isNative << ")" << std::endl;
                lllout <<  msg.Image() << std::endl;
#endif

                if (ShouldBeDiscarded(msg))
                {
                    continue;
                }

                m_handleDataCb(msg,m_QualityOfServiceData.IsPriorityAcked(GetChannelNo(fromChannel)));

                //the dose_com queue size is 32, so we want to empty them completely most of the
                //time, but make sure that we don't stay on one queue forever. The formula below
                //means that we'll read 12 at a time from the lowest priority and 32 at a time from
                //the highest. This formula has not in any way been verified to be "optimal".
                ++numReadsFromThisChannel;
                if (numReadsFromThisChannel >= (NUM_PRIORITY_CHANNELS - currentChannel) * 4 + 8)
                {
                    // lllout << "We've read enough times from this channel, so we'll break out and read from others" << std::endl;

                    m_incomingDataEvents[currentChannel] = 1;

                    if (m_isNotified == 0)
                    {
                        m_isNotified = 1;
                        //we need to do the timeout kind of notify, since we otherwise
                        //might get to wait on ourselves (this is a notify from within the reactor)
                        ACE_Time_Value delay (0);
                        const int res = ACE_Reactor::instance()->notify(this, ACE_Event_Handler::EXCEPT_MASK,&delay);
                        ENSURE(res == 0, << "GAAAH! We failed to notify ourselves! This means that some other"
                                         << "part of dose_main has not filtered its notifies correctly!");
                    }
                    break;
                }
            }
            else if (errCode==ERR_DOSECOM_NO_MSG)
            {
                //continue with next channel.
                break;
            }
            else
            {
                ENSURE(false, <<"ExternNodeCommunication::Read DoseCom_Read returned error (ERR)");
            }
        }
    }


    std::wstring ExternNodeCommunication::IpAddressToString(const unsigned long ipAddr) const
    {
        std::wostringstream ostr;
        const unsigned char * ip = reinterpret_cast<const unsigned char*>(&ipAddr);
        for (int i = 0; i< 4; ++i)
        {
            ostr << static_cast<int>(ip[i]);
            if (i != 3)
            {
                ostr << ".";
            }
        }
        return ostr.str();
    }

    std::wstring ExternNodeCommunication::GetOwnIpAddress() const
    {
        if (m_QualityOfServiceData.IsStandalone())
        {
            return L"127.0.0.1";
        }
        else
        {
            dcom_ulong32 ipAddr;
            DoseCom_GetOwnIpAddr(ipAddr);
            return IpAddressToString(ipAddr);
        }
    }


    void ExternNodeCommunication::NotifyIncomingData(const int priorityChannel)
    {
       //lllout << "ExternNodeCommunication::NotifyIncomingData() - priorityChannel: " << priorityChannel << std::endl;

        m_incomingDataEvents[priorityChannel] = 1;
        if (m_isNotified == 0)
        {
            m_isNotified = 1;
            ACE_Reactor::instance()->notify(this);
        }
    }


    void ExternNodeCommunication::NotifyQueueNotFull(const int priorityChannel)
    {
        // lllout << "ExternNodeCommunication::NotifyQueueNotFull() - priorityChannel: " << priorityChannel << std::endl;

        {
            ACE_Guard<ACE_Thread_Mutex> lck(m_queueIsFullLock);
            m_queueIsFull[priorityChannel] = 0;
        }
        m_queueNotFullEvent = 1;
        if (m_isNotified == 0)
        {
            m_isNotified = 1;
            ACE_Reactor::instance()->notify(this);
        }
    }

    void ExternNodeCommunication::NotifyNodeStatusChanged()
    {
        // lllout << "ExternNodeCommunication::NotifyNodeStatusChanged() - called... " << std::endl;
        m_nodeStatusChangedNotifierCb();
    }

    void ExternNodeCommunication::NotifyStartPoolDistribution()
    {
        // lllout << "ExternNodeCommunication::NotifyStartPoolDistribution() - called... " << std::endl;
        m_startPoolDistributionEvent = 1;
        if (m_isNotified == 0)
        {
            m_isNotified = 1;
            ACE_Reactor::instance()->notify(this);
        }
    }

    void ExternNodeCommunication::NotifyRequestPoolDistribution(const int nodeId)
    {
        lllout << "ExternNodeCommunication::NotifyRequestPoolDistribution(" << nodeId << ") - called... " << std::endl;

        m_requestPDEvents[nodeId] = 1;
        if (m_isNotified == 0)
        {
            m_isNotified = 1;
            ACE_Reactor::instance()->notify(this);
        }
    }

}
}
}
