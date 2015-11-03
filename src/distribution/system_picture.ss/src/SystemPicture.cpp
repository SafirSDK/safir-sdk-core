/******************************************************************************
*
* Copyright Saab AB, 2012 (http://safirsdkcore.com)
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
#include "RawHandler.h"
#include "Coordinator.h"
#include "RawPublisherLocal.h"
#include "RawPublisherRemote.h"
#include "StatePublisherLocal.h"
#include "StatePublisherRemote.h"
#include "LocalSubscriber.h"
#include "RemoteSubscriber.h"
#include "StateSubscriberMaster.h"
#include "MessageWrapperCreators.h"
#include <Safir/Dob/Internal/Communication.h>
#include <Safir/Dob/Internal/SystemPicture.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/MakeUnique.h>
#include <Safir/Utilities/Internal/IpcSubscriber.h>
#include <boost/lexical_cast.hpp>
#include <memory>

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4267)
#pragma warning (disable: 4100)
#endif

#include <boost/asio.hpp>
#include <boost/thread.hpp>

#ifdef _MSC_VER
#pragma warning (pop)
#endif

namespace
{
    const char* const MASTER_LOCAL_RAW_NAME = "SYSTEM_PICTURE_MASTER_RAW";
    const char* const SLAVE_LOCAL_RAW_NAME = "SYSTEM_PICTURE_SLAVE_RAW";
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
        Impl(master_tag_t,
             boost::asio::io_service& ioService,
             Com::Communication& communication,
             const std::string& name,
             const int64_t id,
             const int64_t nodeTypeId,
             const std::map<int64_t, NodeType>& nodeTypes,
             const boost::function<bool (const int64_t incarnationId)>& validateJoinSystemCallback,
             const boost::function<bool (const int64_t incarnationId)>& validateFormSystemCallback)
            : m_rawHandler(Safir::make_unique<RawHandler>(ioService,
                                                          communication,
                                                          name,
                                                          id,
                                                          nodeTypeId,
                                                          communication.ControlAddress(),
                                                          communication.DataAddress(),
                                                          nodeTypes,
                                                          true,
                                                          validateJoinSystemCallback,
                                                          validateFormSystemCallback))
            , m_rawPublisherLocal(Safir::make_unique<RawPublisherLocal>(ioService,
                                                                        *m_rawHandler,
                                                                        MASTER_LOCAL_RAW_NAME,
                                                                        boost::chrono::seconds(1),
                                                                        true))
            , m_rawSubscriberLocal(Safir::make_unique<LocalSubscriber<Safir::Utilities::Internal::IpcSubscriber,
                                                                      RawStatisticsSubscriber,
                                                                      RawStatisticsCreator>>(ioService, SLAVE_LOCAL_RAW_NAME))
            , m_rawPublisherRemote(Safir::make_unique<RawPublisherRemote>(ioService,
                                                                          communication,
                                                                          nodeTypes,
                                                                          MASTER_REMOTE_RAW_NAME,
                                                                          *m_rawHandler,
                                                                          boost::chrono::seconds(30)))
            , m_rawSubscriberRemote(Safir::make_unique<RemoteSubscriber<Com::Communication, RawHandler>>
                                    (communication,
                                     MASTER_REMOTE_RAW_NAME,
                                     *m_rawHandler))
            , m_coordinator(Safir::make_unique<Coordinator>(ioService,
                                                            communication,
                                                            name,
                                                            id,
                                                            nodeTypeId,
                                                            communication.ControlAddress(),
                                                            communication.DataAddress(),
                                                            nodeTypes,
                                                            MASTER_REMOTE_ELECTION_NAME,
                                                            *m_rawHandler))
            , m_statePublisherLocal(Safir::make_unique<StatePublisherLocal>(ioService,
                                                                            *m_coordinator,
                                                                            MASTER_LOCAL_STATE_NAME,
                                                                            boost::chrono::seconds(1)))
            , m_stateSubscriberLocal(Safir::make_unique<StateSubscriberMaster>(ioService,
                                                                               *m_coordinator))
            , m_statePublisherRemote(Safir::make_unique<StatePublisherRemote>(ioService,
                                                                              communication,
                                                                              nodeTypes,
                                                                              MASTER_REMOTE_STATE_NAME,
                                                                              *m_coordinator,
                                                                              boost::chrono::seconds(1)))
            , m_stateSubscriberRemote(Safir::make_unique<RemoteSubscriber<Com::Communication,Coordinator>>
                                      (communication,
                                       MASTER_REMOTE_STATE_NAME,
                                       *m_coordinator))
            , m_stopped(false)
        {
            //when we're in master mode we set up a subscription to raw data from the slave
            m_rawSubscriberLocal->Start([this](const RawStatistics& data)
            {
                m_rawHandler->NewDataChannelStatistics(data);
            });


        }

        /**
         * Construct a slave SystemPicture.
         */
        explicit Impl(slave_tag_t,
                      boost::asio::io_service& ioService,
                      Com::Communication& communication,
                      const std::string& name,
                      const int64_t id,
                      const int64_t nodeTypeId,
                      const std::map<int64_t, NodeType>& nodeTypes)
            : m_rawHandler(Safir::make_unique<RawHandler>(ioService,
                                                          communication,
                                                          name,
                                                          id,
                                                          nodeTypeId,
                                                          "",
                                                          communication.DataAddress(),
                                                          nodeTypes,
                                                          false,
                                                          boost::function<bool (const int64_t)>(),
                                                          boost::function<bool (const int64_t)>())) //NULL function pointers to make vs2010 happy
            , m_rawPublisherLocal(Safir::make_unique<RawPublisherLocal>(ioService,
                                                                        *m_rawHandler,
                                                                        SLAVE_LOCAL_RAW_NAME,
                                                                        boost::chrono::seconds(1),
                                                                        false))
            , m_stopped(false)
        {
            auto stateSubscriberLocal = Safir::make_unique<LocalSubscriber<Safir::Utilities::Internal::IpcSubscriber,
                                                                           SystemStateSubscriber,
                                                                           SystemStateCreator>>(ioService,
                                                                                                MASTER_LOCAL_STATE_NAME);

            stateSubscriberLocal->AddSubscriber([&](const SystemState& ss)
                {
                    std::vector<int64_t> deadNodes;
                    for (int i = 0; i < ss.Size(); ++i)
                    {
                        if (ss.IsDead(i))
                        {
                            deadNodes.push_back(ss.Id(i));
                        }
                    }

                    if(!deadNodes.empty())
                    {
                        m_rawHandler->RecentlyDeadNodes(std::move(deadNodes));
                    }
                });

            m_stateSubscriberLocal = std::move(stateSubscriberLocal);
        }


        /**
         * Construct a subscriber SystemPicture.
         */
        explicit Impl(subscriber_tag_t,
                      boost::asio::io_service& ioService)
            : m_rawSubscriberLocal(Safir::make_unique<LocalSubscriber<Safir::Utilities::Internal::IpcSubscriber,
                                                                      RawStatisticsSubscriber,
                                                                      RawStatisticsCreator>>(ioService,
                                                                                             MASTER_LOCAL_RAW_NAME))
            , m_stateSubscriberLocal(Safir::make_unique<LocalSubscriber<Safir::Utilities::Internal::IpcSubscriber,
                                                                        SystemStateSubscriber,
                                                                        SystemStateCreator>>(ioService,
                                                                                             MASTER_LOCAL_STATE_NAME))
            , m_stopped(false)
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
                if (m_rawHandler != nullptr)
                {
                    m_rawHandler->Stop();
                }

                if (m_rawPublisherLocal != nullptr)
                {
                    m_rawPublisherLocal->Stop();
                }

                if (m_rawSubscriberLocal != nullptr)
                {
                    m_rawSubscriberLocal->Stop();
                }

                if (m_statePublisherLocal != nullptr)
                {
                    m_statePublisherLocal->Stop();
                }

                if (m_stateSubscriberLocal != nullptr)
                {
                    m_stateSubscriberLocal->Stop();
                }

                if (m_rawPublisherRemote != nullptr)
                {
                    m_rawPublisherRemote->Stop();
                }

                if (m_statePublisherRemote != nullptr)
                {
                    m_statePublisherRemote->Stop();
                }

                if (m_coordinator != nullptr)
                {
                    m_coordinator->Stop();
                }
            }
        }

        void StartRawSubscription(const boost::function<void (const RawStatistics& data)>& dataCallback)
        {
            if (m_stopped)
            {
                throw std::logic_error("SystemPicture has already been stopped");
            }

            if (m_rawSubscriberLocal == nullptr)
            {
                throw std::logic_error("Raw Subscriptions are not available in this SystemPicture instance");
            }

            m_rawSubscriberLocal->Start(dataCallback);
        }

        void StartStateSubscription(const boost::function<void (const SystemState& data)>& dataCallback)
        {
            if (m_stopped)
            {
                throw std::logic_error("SystemPicture has already been stopped");
            }

            if (m_stateSubscriberLocal == nullptr)
            {
                throw std::logic_error("State Subscriptions are not available in this SystemPicture instance");
            }

            m_stateSubscriberLocal->Start(dataCallback);
        }

        void ExcludeNode(const int64_t id)
        {
            if (m_rawHandler == nullptr)
            {
                throw std::logic_error("ExcludeNode is not available in this SystemPicture instance");
            }

            m_rawHandler->ExcludeNode(id);
        }


    private:
        std::unique_ptr<RawHandler> m_rawHandler;

        std::unique_ptr<RawPublisherLocal> m_rawPublisherLocal;
        std::unique_ptr<RawStatisticsSubscriber> m_rawSubscriberLocal;

        std::unique_ptr<RawPublisherRemote> m_rawPublisherRemote;
        std::unique_ptr<RemoteSubscriber<Com::Communication, RawHandler>> m_rawSubscriberRemote;

        std::unique_ptr<Coordinator> m_coordinator;

        std::unique_ptr<StatePublisherLocal> m_statePublisherLocal;
        std::unique_ptr<SystemStateSubscriber> m_stateSubscriberLocal;

        std::unique_ptr<StatePublisherRemote> m_statePublisherRemote;
        std::unique_ptr<RemoteSubscriber<Com::Communication, Coordinator>> m_stateSubscriberRemote;

        boost::atomic<bool> m_stopped;
    };

    SystemPicture::SystemPicture(master_tag_t,
                                 boost::asio::io_service& ioService,
                                 Com::Communication& communication,
                                 const std::string& name,
                                 const int64_t id,
                                 const int64_t nodeTypeId,
                                 const std::map<int64_t, NodeType>& nodeTypes,
                                 const boost::function<bool (const int64_t incarnationId)>& validateJoinSystemCallback,
                                 const boost::function<bool (const int64_t incarnationId)>& validateFormSystemCallback)
        : m_impl(Safir::make_unique<Impl>(master_tag,
                                          ioService,
                                          communication,
                                          name,
                                          id,
                                          nodeTypeId,
                                          nodeTypes,
                                          validateJoinSystemCallback,
                                          validateFormSystemCallback))
    {

    }

    SystemPicture::SystemPicture(slave_tag_t,
                                 boost::asio::io_service& ioService,
                                 Com::Communication& communication,
                                 const std::string& name,
                                 const int64_t id,
                                 const int64_t nodeTypeId,
                                 const std::map<int64_t, NodeType>& nodeTypes)
        : m_impl(Safir::make_unique<Impl>(slave_tag,
                                          ioService,
                                          communication,
                                          name,
                                          id,
                                          nodeTypeId,
                                          nodeTypes))
    {

    }

    SystemPicture::SystemPicture(subscriber_tag_t,
                                 boost::asio::io_service& ioService)
        : m_impl(Safir::make_unique<Impl>(subscriber_tag,
                                          ioService))
    {

    }

    SystemPicture::~SystemPicture()
    {

    }

    void SystemPicture::Stop()
    {
        m_impl->Stop();
    }

    void SystemPicture::StartRawSubscription(const boost::function<void (const RawStatistics& data)>& dataCallback)
    {
        m_impl->StartRawSubscription(dataCallback);
    }

    void SystemPicture::StartStateSubscription(const boost::function<void (const SystemState& data)>& dataCallback)
    {
        m_impl->StartStateSubscription(dataCallback);
    }

    void SystemPicture::ExcludeNode(const int64_t id)
    {
        m_impl->ExcludeNode(id);
    }
}
}
}
}
