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
#include <boost/make_shared.hpp>
#include <boost/thread.hpp>

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
        Impl(const boost::shared_ptr<boost::asio::io_service>& ioService,
             const boost::shared_ptr<Com::Communication>& communication,
             const std::string& name,
             const boost::int64_t id,
             const std::string& address,
             const std::string& multicastAddress,
             const std::map<std::string, NodeType>& /*nodeTypes*/)
            : m_ioService(ioService)
            , m_communication(communication)
            , m_rawHandler(boost::make_shared<RawHandler>(ioService,
                                                          communication,
                                                          name,
                                                          id,
                                                          address,
                                                          multicastAddress))
            , m_rawPublisherLocal(boost::make_shared<RawPublisherLocal>(ioService, 
                                                                        m_rawHandler, 
                                                                        MASTER_LOCAL_RAW_NAME))
            , m_rawPublisherRemote(boost::make_shared<RawPublisherRemote>(ioService, 
                                                                          communication, 
                                                                          MASTER_REMOTE_RAW_NAME, 
                                                                          m_rawHandler))
            , m_rawSubscriberRemote(boost::make_shared<RawSubscriberRemote>(communication, 
                                                                            MASTER_REMOTE_RAW_NAME, 
                                                                            m_rawHandler))
            , m_coordinator(boost::make_shared<Coordinator>(ioService, 
                                                            communication,
                                                            name,
                                                            id,
                                                            address,
                                                            MASTER_REMOTE_ELECTION_NAME,
                                                            m_rawHandler))
            , m_statePublisherLocal(boost::make_shared<StatePublisherLocal>(ioService, 
                                                                            m_coordinator, 
                                                                            MASTER_LOCAL_STATE_NAME))
            , m_statePublisherRemote(boost::make_shared<StatePublisherRemote>(ioService, 
                                                                              communication, 
                                                                              MASTER_REMOTE_STATE_NAME, 
                                                                              m_coordinator))
            , m_stateSubscriberRemote(boost::make_shared<StateSubscriberRemote>(communication, 
                                                                                MASTER_REMOTE_STATE_NAME, 
                                                                                m_coordinator))
            , m_stopped(false)
        {

        }

        /** 
         * Construct a slave SystemPicture.
         */
        Impl()
            : m_rawSubscriberLocal(boost::make_shared<RawSubscriberLocal>(MASTER_LOCAL_RAW_NAME))
            , m_stateSubscriberLocal(boost::make_shared<StateSubscriberLocal>(MASTER_LOCAL_STATE_NAME))
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
        boost::shared_ptr<RawStatisticsSubscriber> GetRawStatistics() const
        {
            return m_rawSubscriberLocal;
        }

        boost::shared_ptr<SystemStateSubscriber> GetSystemState() const
        {
            return m_stateSubscriberLocal;
        }

        
    private:
 
        const boost::shared_ptr<boost::asio::io_service> m_ioService;
        const boost::shared_ptr<Com::Communication> m_communication;
        
        boost::shared_ptr<RawHandler> m_rawHandler;

        boost::shared_ptr<RawPublisherLocal> m_rawPublisherLocal;
        boost::shared_ptr<RawSubscriberLocal> m_rawSubscriberLocal;

        boost::shared_ptr<RawPublisherRemote> m_rawPublisherRemote;
        boost::shared_ptr<RawSubscriberRemote> m_rawSubscriberRemote;

        boost::shared_ptr<Coordinator> m_coordinator;

        boost::shared_ptr<StatePublisherLocal> m_statePublisherLocal;
        boost::shared_ptr<StateSubscriberLocal> m_stateSubscriberLocal;

        boost::shared_ptr<StatePublisherRemote> m_statePublisherRemote;
        boost::shared_ptr<StateSubscriberRemote> m_stateSubscriberRemote;

        std::atomic<bool> m_stopped;
    };
    
    SystemPicture::SystemPicture(master_tag_t,
                                 const boost::shared_ptr<boost::asio::io_service>& ioService,
                                 const boost::shared_ptr<Com::Communication>& communication,
                                 const std::string& name,
                                 const boost::int64_t id,
                                 const std::string& address,
                                 const std::string& multicastAddress,
                                 const std::map<std::string, NodeType>& nodeTypes)
        : m_impl(boost::make_shared<Impl>(ioService, 
                                          communication,
                                          name,
                                          id,
                                          address,
                                          multicastAddress,
                                          nodeTypes))
    {

    }

    SystemPicture::SystemPicture(slave_tag_t)
        : m_impl(boost::make_shared<Impl>())
    {

    }

    void SystemPicture::Stop()
    {
        m_impl->Stop();
    }

    boost::shared_ptr<RawStatisticsSubscriber> 
    SystemPicture::GetRawStatistics() const
    {
        return m_impl->GetRawStatistics();
    }

    boost::shared_ptr<SystemStateSubscriber> 
    SystemPicture::GetSystemState() const
    {
        return m_impl->GetSystemState();
    }


}
}
}
}
