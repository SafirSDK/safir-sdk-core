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

#include "Coordinator.h"
#include "RawHandler.h"
#include "RawPublisherLocal.h"
#include "RawPublisherRemote.h"
#include "RawSubscriberLocal.h"
#include "RawSubscriberRemote.h"
#include "StatePublisherLocal.h"
#include "StateSubscriberLocal.h"
#include "StatePublisherRemote.h"
#include "StateSubscriberRemote.h"
#include <Safir/Dob/Internal/Communication.h>
#include <Safir/Dob/Internal/SystemPicture.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <boost/asio.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/thread.hpp>
#include <memory>

namespace
{
    const char* const MASTER_LOCAL_RAW_NAME = "SYSTEM_PICTURE_MASTER_RAW";
    const char* const MASTER_LOCAL_STATE_NAME = "SYSTEM_PICTURE_MASTER_STATE";
    const char* const MASTER_REMOTE_RAW_NAME = "SP_RAW";
    const char* const MASTER_REMOTE_STATE_NAME = "SP_STATE";
    const char* const MASTER_REMOTE_ELECTION_NAME = "SP_ELECTION";
}

namespace Safir
{
namespace Dob
{
namespace Internal
{
namespace SP
{


    using namespace Safir::Utilities::Internal;
    
    class SystemPicture::Impl
        : private boost::noncopyable
    {
    public:

        /** 
         * Construct a master SystemPicture.
         */
        Impl(boost::asio::io_service& ioService,
             Com::Communication& communication,
             const std::string& name,
             const boost::int64_t id,
             const boost::int64_t nodeTypeId,
             const std::string& controlAddress,
             const std::string& dataAddress,
             const std::map<boost::int64_t, NodeType>& nodeTypes)
            : m_rawHandler(std::make_unique<RawHandler>(ioService,
                                                        communication,
                                                        name,
                                                        id,
                                                        nodeTypeId,
                                                        controlAddress,
                                                        dataAddress,
                                                        nodeTypes))
            , m_rawPublisherLocal(std::make_unique<RawPublisherLocal>(ioService, 
                                                                      *m_rawHandler, 
                                                                      MASTER_LOCAL_RAW_NAME))
            , m_rawPublisherRemote(std::make_unique<RawPublisherRemote>(ioService,
                                                                        communication,
                                                                        nodeTypes, 
                                                                        MASTER_REMOTE_RAW_NAME, 
                                                                        *m_rawHandler))
            , m_rawSubscriberRemote(std::make_unique<RawSubscriberRemote>(communication, 
                                                                          MASTER_REMOTE_RAW_NAME, 
                                                                          *m_rawHandler))
            , m_coordinator(std::make_unique<Coordinator>(ioService, 
                                                          communication,
                                                          name,
                                                          id,
                                                          nodeTypeId,
                                                          controlAddress,
                                                          dataAddress,
                                                          nodeTypes,
                                                          MASTER_REMOTE_ELECTION_NAME,
                                                          *m_rawHandler))
            , m_statePublisherLocal(std::make_unique<StatePublisherLocal>(ioService, 
                                                                          *m_coordinator, 
                                                                          MASTER_LOCAL_STATE_NAME))
            , m_statePublisherRemote(std::make_unique<StatePublisherRemote>(ioService, 
                                                                            communication, 
                                                                            nodeTypes,
                                                                            MASTER_REMOTE_STATE_NAME, 
                                                                            *m_coordinator))
            , m_stateSubscriberRemote(std::make_unique<StateSubscriberRemote>(communication, 
                                                                              MASTER_REMOTE_STATE_NAME, 
                                                                              *m_coordinator))
            , m_stopped(false)
        {

        }

        /** 
         * Construct a slave SystemPicture.
         */
        Impl()
            : m_rawSubscriberLocal(std::make_unique<RawSubscriberLocal>(MASTER_LOCAL_RAW_NAME))
            , m_stateSubscriberLocal(std::make_unique<StateSubscriberLocal>(MASTER_LOCAL_STATE_NAME))
            , m_stopped(true) //not really started in this case...
        {

        }

        ~Impl()
        {
            if (!m_stopped)
            {
                SEND_SYSTEM_LOG(Error,
                                << "You have to call SystemPicture::Stop before destroying object");
                abort();
            }
        }

        void Stop()
        {
            const bool was_stopped = m_stopped.exchange(true);
            if (!was_stopped)
            {
                //only masters have anything that needs stopping.
                m_rawHandler->Stop();
                m_rawPublisherLocal->Stop();
                m_statePublisherLocal->Stop();
                m_rawPublisherRemote->Stop();
                m_statePublisherRemote->Stop();
                m_coordinator->Stop();
            }
        }
        
        //Only valid for slaves
        RawStatisticsSubscriber& GetRawStatistics() const
        {
            return *m_rawSubscriberLocal;
        }

        SystemStateSubscriber& GetSystemState() const
        {
            return *m_stateSubscriberLocal;
        }

        
    private:
        
        std::unique_ptr<RawHandler> m_rawHandler;

        std::unique_ptr<RawPublisherLocal> m_rawPublisherLocal;
        std::unique_ptr<RawSubscriberLocal> m_rawSubscriberLocal;

        std::unique_ptr<RawPublisherRemote> m_rawPublisherRemote;
        std::unique_ptr<RawSubscriberRemote> m_rawSubscriberRemote;

        std::unique_ptr<Coordinator> m_coordinator;

        std::unique_ptr<StatePublisherLocal> m_statePublisherLocal;
        std::unique_ptr<StateSubscriberLocal> m_stateSubscriberLocal;

        std::unique_ptr<StatePublisherRemote> m_statePublisherRemote;
        std::unique_ptr<StateSubscriberRemote> m_stateSubscriberRemote;

        std::atomic<bool> m_stopped;
    };
    
    SystemPicture::SystemPicture(master_tag_t,
                                 boost::asio::io_service& ioService,
                                 Com::Communication& communication,
                                 const std::string& name,
                                 const boost::int64_t id,
                                 const boost::int64_t nodeTypeId,
                                 const std::string& controlAddress,
                                 const std::string& dataAddress,
                                 const std::map<boost::int64_t, NodeType>& nodeTypes)
    : m_impl(std::make_unique<Impl>(ioService,
                                    communication,
                                    name,
                                    id,
                                    nodeTypeId,
                                    controlAddress,
                                    dataAddress,
                                    nodeTypes))
    {

    }

    SystemPicture::SystemPicture(slave_tag_t)
        : m_impl(std::make_unique<Impl>())
    {

    }

    SystemPicture::~SystemPicture()
    {

    }

    void SystemPicture::Stop()
    {
        m_impl->Stop();
    }

    RawStatisticsSubscriber& 
    SystemPicture::GetRawStatistics() const
    {
        return m_impl->GetRawStatistics();
    }

    SystemStateSubscriber&
    SystemPicture::GetSystemState() const
    {
        return m_impl->GetSystemState();
    }


}
}
}
}
