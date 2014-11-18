/******************************************************************************
*
* Copyright Saab AB, 2008-2013 (http://safir.sourceforge.net)
*
* Created by: Joel Ottosson / stjoot
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

#include "dose_controller.h"

#include <boost/bind.hpp>
#include <Safir/Dob/AccessDeniedException.h>
#include <Safir/Dob/Entity.h>
#include <Safir/Dob/Internal/Connection.h>
#include <Safir/Dob/Internal/Connections.h>
#include <Safir/Dob/Internal/InternalDefs.h>
#include <Safir/Dob/Internal/MessageTypes.h>
#include <Safir/Dob/Internal/ServiceTypes.h>
#include <Safir/Dob/Internal/EntityTypes.h>
#include <Safir/Dob/Internal/EndStates.h>
#include <Safir/Dob/Internal/InjectionKindTable.h>
#include <Safir/Dob/Internal/State.h>
#include <Safir/Dob/Internal/SubscriptionId.h>
#include <Safir/Dob/Internal/TimestampOperations.h>
#include <Safir/Dob/Internal/ContextSharedTable.h>
#include <Safir/Dob/Internal/ContextIdComposer.h>
#include <Safir/Dob/Internal/NodeStatuses.h>
#include <Safir/Dob/Message.h>
#include <Safir/Dob/NodeParameters.h>
#include <Safir/Dob/QueueParameters.h>
#include <Safir/Dob/NotFoundException.h>
#include <Safir/Dob/NotOpenException.h>
#include <Safir/Dob/OverflowException.h>
#include <Safir/Dob/Typesystem/Exceptions.h>
#include <Safir/Dob/Service.h>
#include <Safir/Dob/Response.h>
#include <Safir/Dob/EntityIdResponse.h>
#include <Safir/Dob/CallbackId.h>
#include <Safir/Dob/ThisNodeParameters.h>
#include <Safir/Dob/Typesystem/Internal/BlobOperations.h>
#include <Safir/Dob/Typesystem/Internal/InternalOperations.h>
#include <Safir/Dob/Typesystem/Internal/InternalUtils.h>
#include <Safir/Dob/Typesystem/Operations.h>
#include <Safir/Dob/Typesystem/Utilities.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/ProcessInfo.h>
#include <Safir/Dob/ErrorResponse.h>
#include <Safir/Dob/ResponseGeneralErrorCodes.h>
#include <Safir/Dob/Typesystem/Serialization.h>

#include <assert.h>
#include <boost/shared_ptr.hpp>

#ifdef _MSC_VER
  #pragma warning(push)
  #pragma warning(disable: 4702)
#endif
#include <boost/lexical_cast.hpp>
#ifdef _MSC_VER
  #pragma warning(pop)
#endif

#include <iostream>

using namespace std;

namespace Safir
{
namespace Dob
{
namespace Internal
{
    Controller::Controller()
        : m_isConnected(false),
          m_connection(NULL),
          m_requestQueueInOverflowState(false),
          m_messageQueueInOverflowState(false),
          m_ctrlId(0),
          m_contextId(0),
          m_exitDispatch(false),
          m_dispatchedInjection(no_state_tag),
          m_originalInjectionState(no_state_tag),
          m_consumerReferences()
    {

    }

    void Controller::SetInstanceId(long id)
    {
        m_ctrlId=id;
    }

    //---------------------------------------------------------------------------------------
    // DOB Startup and Initialization
    //---------------------------------------------------------------------------------------
    bool Controller::IsConnected()
    {
        return m_isConnected;
    }

    void Controller::Connect(const char* connectionNameCommonPart,
                             const char* connectionNameInstancePart,
                             const ContextId contextId,
                             long lang,
                             const ConsumerId & connectionOwner,
                             const ConsumerId & dispatcher,
                             OnDispatchCb* onDispatchCb, //callback instead of event
                             OnStopOrderCb* onStopOrderCb,
                             OnNewEntityCb* onNewEntityCb,
                             OnUpdatedEntityCb* onUpdatedEntityCb,
                             OnDeletedEntityCb* onDeletedEntityCb,
                             OnCreateRequestCb* onCreateRequestCb,
                             OnUpdateRequestCb* onUpdateRequestCb,
                             OnDeleteRequestCb* onDeleteRequestCb,
                             OnServiceRequestCb* onServiceRequestCb,
                             OnResponseCb* onResponseCb,
                             OnMessageCb* onMessageCb,
                             OnRegisteredCb* onRegisteredCb,
                             OnUnregisteredCb* onUnregisteredCb,
                             OnRevokedRegistrationCb* onRevokedRegistrationCb,
                             OnCompletedRegistrationCb* onCompletedRegistrationCb,
                             OnInjectedNewEntityCb* onInjectedNewEntityCb,
                             OnInjectedUpdatedEntityCb* onInjectedUpdatedEntityCb,
                             OnInjectedDeletedEntityCb* onInjectedDeletedEntityCb,
                             OnInitialInjectionsDoneCb* onInitialInjectionsDoneCb,
                             OnNotRequestOverflowCb* onNotRequestOverflowCb,
                             OnNotMessageOverflowCb* onNotMessageOverflowCb,
                             OnDropReferenceCb* onDropReferenceCb)
    {
        // The context parameter is used to signal wich context to connect to, and also to signal if this connection
        // should be allowed to connect before "ordinary" apps. Originally, when there was only one context (0),
        // -1 was used to signal a premature connect in context 0. Now when there are several contexts
        // another scheme is required:
        //
        // contextId      Connection
        // ---------      ----------
        //    0           Ordinary connection in context 0.
        //    1           Ordinary connection in context 1.
        //    345         Ordinary connection in context 345.
        //
        //    -1000000    "minus one"-connection in context 0
        //    -1000001    "minus one"-connection in context 1
        //    -1000345    "minus one"-connection in context 345
        // 
        //
        bool minusOneConnection = false;
        if (contextId < 0)
        {
            minusOneConnection = true;
            m_contextId = ComposeContext(contextId);
        }
        else
        {
            m_contextId = contextId;
        }
       
        if (m_contextId >= NodeParameters::NumberOfContexts())
        {
            std::wostringstream ostr;
            ostr << "Trying to open a ";
            if (minusOneConnection)
            {
                ostr << "\"minus one\" ";
            }
            ostr <<  "connection in context " << m_contextId << ", but the system is configured for max context id "
                 << NodeParameters::NumberOfContexts()-1 << ". (The number of contexts is " << NodeParameters::NumberOfContexts()
                 << "). Consider changing parameter Safir.Dob.NodeParameters.NumberOfContexts.";
            throw Safir::Dob::Typesystem::SoftwareViolationException(ostr.str(),__WFILE__,__LINE__);
        }

        // If it is a garbage collected language the consumer reference counters must be incremented.
        // This must be done even if we already are connected.
        if (g_garbageCollected[lang])
        {
            m_consumerReferences.AddDispatcherReference(dispatcher);

            // The connectionOwner cunsumer is allowed to be null. If it is, its
            // reference counter must not be incremented.
            if (connectionOwner.consumer != NULL)
            {
                m_consumerReferences.AddStopHandlerReference(connectionOwner);
            }
        }

        if (m_isConnected)
        {
            return;
        }

        lllout << "Starting  with (" << connectionNameCommonPart << ", " << connectionNameInstancePart << ")" << std::endl;

        m_dispatcher.SetConnectionOwner(connectionOwner, onStopOrderCb);

        m_dispatcher.SetCallbacks(  lang,
                                    onNewEntityCb,
                                    onUpdatedEntityCb,
                                    onDeletedEntityCb,
                                    onCreateRequestCb,
                                    onUpdateRequestCb,
                                    onDeleteRequestCb,
                                    onServiceRequestCb,
                                    onResponseCb,
                                    onMessageCb,
                                    onRegisteredCb,
                                    onUnregisteredCb,
                                    onRevokedRegistrationCb,
                                    onCompletedRegistrationCb,
                                    onInjectedNewEntityCb,
                                    onInjectedUpdatedEntityCb,
                                    onInjectedDeletedEntityCb,
                                    onInitialInjectionsDoneCb,
                                    onNotRequestOverflowCb,
                                    onNotMessageOverflowCb,
                                    onDropReferenceCb);
        // Save the original name parts
        m_connectionNameCommonPart = connectionNameCommonPart;
        m_connectionNameInstancePart = connectionNameInstancePart;

        m_connectionName = ComposeName(m_contextId, m_connectionNameCommonPart, m_connectionNameInstancePart);

        ConnectResult result;
        ConnectionPtr connection;

        Connections::Instance().Connect(m_connectionName, m_contextId, minusOneConnection, result, connection);

        switch (result)
        {
        case Success:
            {
                m_isConnected = true;
                m_connection = connection;

                MessageTypes::Initialize();
                EndStates::Initialize();
                ServiceTypes::Initialize();
                InjectionKindTable::Initialize();
                NodeStatuses::Initialize();
                EntityTypes::Initialize();
                ContextSharedTable::Initialize();
            }
            break;
        case TooManyProcesses:
            {
                m_isConnected = false;
                std::wostringstream ostr;
                ostr << "Failed to open connection! There are too many processes connected to the Dob, please increase "
                     << "the parameter Safir.Dob.ProcessInfo.MaxNumberOfInstances. " << std::endl
                     << "While opening connection with name " << m_connectionName.c_str();
                throw Safir::Dob::NotOpenException(ostr.str(), __WFILE__,__LINE__);
            }
            break;
        case TooManyConnectionsInProcess:
            {
                m_isConnected = false;
                std::wostringstream ostr;
                ostr << "Failed to open connection! This process has too many connections, "
                     << "please increase the size of the ConnectionNames array in class Safir.Dob.ProcessInfo. " << std::endl
                     << "While opening Process pid = " << Safir::Utilities::ProcessInfo::GetPid() << ", ConnectionName = " << m_connectionName.c_str();
                throw Safir::Dob::NotOpenException(ostr.str(), __WFILE__,__LINE__);
            }
            break;

        case ConnectionNameAlreadyExists:
            {
                m_isConnected = false;
                std::wostringstream ostr;
                ostr << "Failed to open connection! The connection name '" << m_connectionName.c_str()
                     << "' is already in use. While opening connection from process with pid = " << Safir::Utilities::ProcessInfo::GetPid();
                throw Safir::Dob::NotOpenException(ostr.str(), __WFILE__,__LINE__);
            }
            break;

        case Undefined:
            ENSURE(result != Undefined, << "Got Undefined from Connetions::Instance().Connect().");
            break;
        }


        ENSURE(m_dispatchThread == NULL, << "DispatchThread was non-NULL when Connect was called!");
        m_dispatchThread.reset(new DispatchThread(m_connection->Id(), m_connectionName, dispatcher, onDispatchCb));

        m_dispatchThread->Start();

        lllout << " complete for " << m_connectionName.c_str() << std::endl;
    }

    void Controller::ConnectSecondary(  long lang,
                                        OnNewEntityCb* onNewEntityCb,
                                        OnUpdatedEntityCb* onUpdatedEntityCb,
                                        OnDeletedEntityCb* onDeletedEntityCb,
                                        OnCreateRequestCb* onCreateRequestCb,
                                        OnUpdateRequestCb* onUpdateRequestCb,
                                        OnDeleteRequestCb* onDeleteRequestCb,
                                        OnServiceRequestCb* onServiceRequestCb,
                                        OnResponseCb* onResponseCb,
                                        OnMessageCb* onMessageCb,
                                        OnRegisteredCb* onRegisteredCb,
                                        OnUnregisteredCb* onUnregisteredCb,
                                        OnRevokedRegistrationCb* onRevokedRegistrationCb,
                                        OnCompletedRegistrationCb* onCompletedRegistrationCb,
                                        OnInjectedNewEntityCb* onInjectedNewEntityCb,
                                        OnInjectedUpdatedEntityCb* onInjectedUpdatedEntityCb,
                                        OnInjectedDeletedEntityCb* onInjectedDeletedEntityCb,
                                        OnInitialInjectionsDoneCb* onInitialInjectionsDoneCb,
                                        OnNotRequestOverflowCb* onNotRequestOverflowCb,
                                        OnNotMessageOverflowCb* onNotMessageOverflowCb,
                                        OnDropReferenceCb* onDropReferenceCb)
    {
        if (!m_isConnected) //Must already be attached to dose_main, to be able to attach secondary
        {
            throw Safir::Dob::NotOpenException(L"Attach: The connection that SecondaryConnection is trying to attach to is not open",__WFILE__,__LINE__);
        }

        m_dispatcher.SetCallbacks(lang,
                                  onNewEntityCb,
                                  onUpdatedEntityCb,
                                  onDeletedEntityCb,
                                  onCreateRequestCb,
                                  onUpdateRequestCb,
                                  onDeleteRequestCb,
                                  onServiceRequestCb,
                                  onResponseCb,
                                  onMessageCb,
                                  onRegisteredCb,
                                  onUnregisteredCb,
                                  onRevokedRegistrationCb,
                                  onCompletedRegistrationCb,
                                  onInjectedNewEntityCb,
                                  onInjectedUpdatedEntityCb,
                                  onInjectedDeletedEntityCb,
                                  onInitialInjectionsDoneCb,
                                  onNotRequestOverflowCb,
                                  onNotMessageOverflowCb,
                                  onDropReferenceCb);
        lllout << "ConnectSecondary complete for " << m_connectionName.c_str() << std::endl;
    }


    void Controller::Disconnect()
    {
        if (m_isConnected)
        {
            // The app must not get any callbacks after a disconnect
            m_exitDispatch = true;
        }

        //stop m_dispatchThread first of all
        if (m_dispatchThread != NULL)
        {
            m_dispatchThread->Stop();
            m_dispatchThread.reset();
        }

        lllout << "Controller::Disconnect() - m_isConnected: " << std::boolalpha << m_isConnected << std::endl;

        if (m_isConnected)
        {
            lllout << "Controller::Disconnect() - Disconnecting " << m_connectionName.c_str() << std::endl;
            Connections::Instance().Disconnect(m_connection);
        }
        m_isConnected = false;

        //-----------------------------
        //clean up local resources
        //-----------------------------
        m_requestQueueInOverflowState = false;
        m_messageQueueInOverflowState = false;

        //delete m_dispatcher's resources
        m_dispatcher.Clear();

        //shared queues are deleted by dose_main.

        //AWI:? Can be removed?
        //m_contextId = 0;
        
        m_connection.reset();

        // Drop any reference corresponding to a saved consumer (will be saved for
        // garbage colleted languages only).
        m_consumerReferences.DropAllReferences(boost::bind(&Dispatcher::InvokeDropReferenceCb,
                                                           m_dispatcher,
                                                           _1,
                                                           _2));
    }

    const char * const
    Controller::GetConnectionName() const
    {
        if (!m_isConnected)
        {
            throw Safir::Dob::NotOpenException(L"This connection to the DOB is not open. (While calling GetConnectionName)",__WFILE__,__LINE__);
        }
        return m_connection->NameWithCounter();
    }

    const char * const
    Controller::GetConnectionNameCommonPart() const
    {
        if (!m_isConnected)
        {
            throw Safir::Dob::NotOpenException(L"This connection to the DOB is not open. (While calling GetConnectionNameCommonPart)",__WFILE__,__LINE__);
        }
        return m_connectionNameCommonPart.c_str();
    }

    const char * const
    Controller::GetConnectionNameInstancePart() const
    {
        if (!m_isConnected)
        {
            throw Safir::Dob::NotOpenException(L"This connection to the DOB is not open. (While calling GetConnectionNameInstancePart)",__WFILE__,__LINE__);
        }
        return m_connectionNameInstancePart.c_str();
    }


    bool Controller::NameIsEqual (const std::string & connectionNameCommonPart,
                                  const std::string & connectionNameInstancePart) const
    {
        if (!m_isConnected)
        {
            return false;
        }

        return m_connectionNameCommonPart   == connectionNameCommonPart &&
            m_connectionNameInstancePart == connectionNameInstancePart;
    }

    void Controller::RegisterServiceHandler(const Dob::Typesystem::TypeId       typeId,
                                            const Dob::Typesystem::HandlerId&   handlerId,
                                            const bool                          overrideRegistration,
                                            const ConsumerId&                   consumer)
    {
        if (!m_isConnected)
        {
            std::wostringstream ostr;
            ostr << "This connection to the DOB is not open. (While calling RegisterServiceHandler for type "
                 << Typesystem::Operations::GetName(typeId)
                 << " with handlerId " << handlerId << ")";
            throw Safir::Dob::NotOpenException(ostr.str(),__WFILE__,__LINE__);
        }

        if (!Dob::Typesystem::Operations::IsOfType(typeId, Dob::Service::ClassTypeId))
        {
            std::wostringstream ostr;
            ostr << "Type used in RegisterServiceHandler is not a Service type. typeId = " << Typesystem::Operations::GetName(typeId) << ")";
            throw Safir::Dob::Typesystem::SoftwareViolationException(ostr.str(),__WFILE__,__LINE__);
        }

        if (overrideRegistration)
        {
            ServiceTypes::Instance().Register(m_connection,
                                               typeId,
                                               handlerId,
                                               overrideRegistration,
                                               consumer);
        }
        else
        {
            m_connection->AddPendingRegistration(PendingRegistration(typeId, handlerId, consumer));
            m_connection->SignalOut();
        }

        // Add a reference for garbage collected languages
        if (g_garbageCollected[consumer.lang])
        {
            m_consumerReferences.AddHandlerRegistrationReference(typeId, handlerId, consumer);
        }
    }

    void Controller::RegisterEntityHandler(const Dob::Typesystem::TypeId            typeId,
                                           const Dob::Typesystem::HandlerId&        handlerId,
                                           const Dob::InstanceIdPolicy::Enumeration instanceIdPolicy,
                                           const bool                               overrideRegistration,
                                           const bool                               isInjectionHandler,
                                           const ConsumerId&                        consumer)
    {
        if (!m_isConnected)
        {
            std::wostringstream ostr;
            ostr << "This connection to the DOB is not open. (While calling RegisterEntityHandler for type "
                 << Typesystem::Operations::GetName(typeId)
                 << " with handlerId " << handlerId << ")";
            throw Safir::Dob::NotOpenException(ostr.str(),__WFILE__,__LINE__);
        }

        if (!Dob::Typesystem::Operations::IsOfType(typeId, Dob::Entity::ClassTypeId))
        {
            std::wostringstream ostr;
            ostr << "Type used in RegisterEntityHandler is not an Entity type. typeId = " << Typesystem::Operations::GetName(typeId) << ")";
            throw Safir::Dob::Typesystem::SoftwareViolationException(ostr.str(),__WFILE__,__LINE__);
        }

        //if the type has any kind of injection the handler must be able to handle injections.
        if (!InjectionKindTable::Instance().IsNone(typeId) && !isInjectionHandler)
        {
            std::wostringstream ostr;
            ostr << "Handler used in RegisterEntityHandler is not of a kind that can handle injections, "
                << "but the type has InjectionKind = "
                << InjectionKind::ToString(InjectionKindTable::Instance().GetInjectionKind(typeId));
            throw Safir::Dob::Typesystem::SoftwareViolationException(ostr.str(),__WFILE__,__LINE__);
        }

        if (overrideRegistration)
        {
            EntityTypes::Instance().Register(m_connection,
                                             typeId,
                                             handlerId,
                                             instanceIdPolicy,
                                             isInjectionHandler,
                                             overrideRegistration,
                                             consumer);
        }
        else
        {
            m_connection->AddPendingRegistration(PendingRegistration(typeId, handlerId, instanceIdPolicy, isInjectionHandler, consumer));
            m_connection->SignalOut();
        }

        // Add a reference for garbage collected languages
        if (g_garbageCollected[consumer.lang])
        {
            m_consumerReferences.AddHandlerRegistrationReference(typeId, handlerId, consumer);
        }
    }

    void Controller::UnregisterHandler(const Dob::Typesystem::TypeId typeId,
                                       const Dob::Typesystem::HandlerId& handlerId)
    {
        if (!m_isConnected)
        {
            std::wostringstream ostr;
            ostr << "This connection to the DOB is not open. (While calling UnregisterHandler for type "
                 << Typesystem::Operations::GetName(typeId)
                 << " with handlerId " << handlerId << ")";
            throw Safir::Dob::NotOpenException(ostr.str(),__WFILE__,__LINE__);
        }

        if (m_connection->RemovePendingRegistrations(typeId,handlerId))
        {
            m_connection->SignalOut();
        }

        if (Dob::Typesystem::Operations::IsOfType(typeId, Dob::Entity::ClassTypeId))
        {
            EntityTypes::Instance().Unregister(m_connection,
                                               typeId,
                                               handlerId);
        }
        else if (Dob::Typesystem::Operations::IsOfType(typeId, Dob::Service::ClassTypeId))
        {
            ServiceTypes::Instance().Unregister(m_connection,
                                                typeId,
                                                handlerId);
        }
        else
        {
            std::wostringstream ostr;
            ostr << "Type used in UnregisterHandler is not an Entity type or Service type. typeId = "
                 << Typesystem::Operations::GetName(typeId) << ")";
            throw Safir::Dob::Typesystem::SoftwareViolationException(ostr.str(),__WFILE__,__LINE__);
        }

        m_connection->RemoveRevokedRegistration(typeId, handlerId);

        // For garbage collected languages the references must be dropped.
        // Since UnregisterHandler doesn't have a consumer parameter, we don't know which language
        // we are dealing with. However, it is ok to call the check routine anyway, a non GC language
        // just won't have any references.
        m_consumerReferences.DropAllHandlerRegistrationReferences(typeId,
                                                                  handlerId,
                                                                  boost::bind(&Dispatcher::InvokeDropReferenceCb,
                                                                              m_dispatcher,
                                                                              _1,
                                                                              _2));
    }

    //----------------------------
    // Subscription methods
    //----------------------------
    void Controller::SubscribeMessage(const Dob::Typesystem::TypeId typeId,
                                      const Dob::Typesystem::ChannelId & channelId,
                                      const bool includeSubclasses,
                                      const ConsumerId & messageSubscriber)
    {
        if (!m_isConnected)
        {
            std::wostringstream ostr;
            ostr << "This connection to the DOB is not open. (While calling SubscribeMessage for type "
                 << Typesystem::Operations::GetName(typeId)
                 << " with channelId " << channelId << ")";
            throw Safir::Dob::NotOpenException(ostr.str(),__WFILE__,__LINE__);
        }

        if (!Dob::Typesystem::Operations::IsOfType(typeId, Dob::Message::ClassTypeId))
        {
            std::wostringstream ostr;
            ostr << "Type used in SubscribeMessage is not a Message type. typeId = " << Typesystem::Operations::GetName(typeId) << ")";
            throw Safir::Dob::Typesystem::SoftwareViolationException(ostr.str(),__WFILE__,__LINE__);
        }

        MessageTypes::Instance().Subscribe(m_connection,
                                           typeId,
                                           channelId,
                                           includeSubclasses,
                                           messageSubscriber);

        // Add a reference for garbage collected languages
        if (g_garbageCollected[messageSubscriber.lang])
        {
            m_consumerReferences.AddMessageSubscriptionReference(messageSubscriber);
        }
    }

    void Controller::UnsubscribeMessage(const Dob::Typesystem::TypeId typeId,
                                        const Dob::Typesystem::ChannelId & channelId,
                                        const bool includeSubclasses,
                                        const ConsumerId & messageSubscriber)
    {
        if (!m_isConnected)
        {
            std::wostringstream ostr;
            ostr << "This connection to the DOB is not open. (While calling UnsubscribeMessage for type "
                 << Typesystem::Operations::GetName(typeId)
                 << " with channelId " << channelId << ")";
            throw Safir::Dob::NotOpenException(ostr.str(),__WFILE__,__LINE__);
        }

        if (!Dob::Typesystem::Operations::IsOfType(typeId, Dob::Message::ClassTypeId))
        {
            std::wostringstream ostr;
            ostr << "Type used in SubscribeMessage is not a Message type. typeId = " << Typesystem::Operations::GetName(typeId) << ")";
            throw Safir::Dob::Typesystem::SoftwareViolationException(ostr.str(),__WFILE__,__LINE__);
        }

        MessageTypes::Instance().Unsubscribe(m_connection,
                                             typeId,
                                             channelId,
                                             includeSubclasses,
                                             messageSubscriber);

        // For garbage collected languages check if the consumer is used in any message subscription.
        if (g_garbageCollected[messageSubscriber.lang])
        {
            m_consumerReferences.DropMessageSubscriptionReferences(m_connection,
                                                                   messageSubscriber,
                                                                   boost::bind(&Dispatcher::InvokeDropReferenceCb,
                                                                               m_dispatcher,
                                                                               _1,
                                                                               _2));
        }
    }

    void Controller::SubscribeEntity(const Typesystem::EntityId& entityId,
                                     const bool allInstances,
                                     const bool includeSubclasses,
                                     const bool restartSubscription,
                                     const SubscriptionOptionsPtr& subscriptionOptions,
                                     const ConsumerId& consumer)
    {
        if (!m_isConnected)
        {
            std::wostringstream ostr;
            ostr << "This connection to the DOB is not open. (While calling SubscribeEntity for ";
            if (allInstances)
            {
                ostr << "all instances of type " << Typesystem::Operations::GetName(entityId.GetTypeId())
                    << ").";
            }
            else
            {
                ostr << entityId << ").";
            }
            throw Safir::Dob::NotOpenException(ostr.str(),__WFILE__,__LINE__);
        }

        if (!Dob::Typesystem::Operations::IsOfType(entityId.GetTypeId(), Dob::Entity::ClassTypeId))
        {
            std::wostringstream ostr;
            ostr << "Type used in SubscribeEntity is not an Entity type. type = "
                << Typesystem::Operations::GetName(entityId.GetTypeId());
            throw Typesystem::SoftwareViolationException(ostr.str(),__WFILE__,__LINE__);
        }

        // Set subscription type to EntitySubscription.
        // The subscription type makes it possible for dose to set up an internal injection subscription for the same connection/consumer
        // that doesn't interfere with the "normal" subscription.
        SubscriptionId subscriptionId(ConnectionConsumerPair(m_connection, consumer), EntitySubscription, 0);

        EntityTypes::Instance().Subscribe(subscriptionId,
                                          entityId,
                                          allInstances,
                                          includeSubclasses,
                                          restartSubscription,
                                          subscriptionOptions);

        // Add a reference for garbage collected languages
        if (g_garbageCollected[consumer.lang])
        {
            m_consumerReferences.AddEntitySubscriptionReference(consumer);
        }
    }

    void Controller::UnsubscribeEntity(const Typesystem::EntityId& entityId,
                                       const bool allInstances,
                                       const bool includeSubclasses,
                                       const ConsumerId& consumer)
    {
        if (!m_isConnected)
        {
            std::wostringstream ostr;
            ostr << "This connection to the DOB is not open. (While calling UnsubscribeEntity for ";
            if (allInstances)
            {
                ostr << "all instances of type " << Typesystem::Operations::GetName(entityId.GetTypeId())
                    << ").";
            }
            else
            {
                ostr << entityId << ").";
            }
            throw Safir::Dob::NotOpenException(ostr.str(),__WFILE__,__LINE__);
        }

        // See comment for the SubscribeEntity method above.
        SubscriptionId subscriptionId(ConnectionConsumerPair(m_connection, consumer), EntitySubscription, 0);

        EntityTypes::Instance().Unsubscribe(subscriptionId,
                                            entityId,
                                            allInstances,
                                            includeSubclasses);

        // For garbage collected languages check if the consumer is used in any entity subscription.
        if (g_garbageCollected[consumer.lang])
        {
            m_consumerReferences.DropEntitySubscriptionReferences(m_connection,
                                                                  consumer,
                                                                  boost::bind(&Dispatcher::InvokeDropReferenceCb,
                                                                              m_dispatcher,
                                                                              _1,
                                                                              _2));
        }
    }

    void Controller::SubscribeRegistration(const Safir::Dob::Typesystem::TypeId typeId,
                                           const Dob::Typesystem::HandlerId& handlerId,
                                           const bool includeSubclasses,
                                           const bool restartSubscription,
                                           const SubscriptionOptionsPtr& subscriptionOptions,
                                           const ConsumerId& consumer)
    {
        if (!m_isConnected)
        {
            std::wostringstream ostr;
            ostr << "This connection to the DOB is not open. (While calling SubscribeRegistration for type "
                 << Typesystem::Operations::GetName(typeId)
                 << " with handlerId " << handlerId << ")";
            throw Safir::Dob::NotOpenException(ostr.str(),__WFILE__,__LINE__);
        }

        if (Dob::Typesystem::Operations::IsOfType(typeId, Dob::Entity::ClassTypeId))
        {
            SubscriptionId subscriptionId(ConnectionConsumerPair(m_connection, consumer), EntityRegistrationSubscription, 0);

            EntityTypes::Instance().SubscribeRegistration(subscriptionId,
                                                          typeId,
                                                          handlerId,
                                                          includeSubclasses,
                                                          restartSubscription,
                                                          subscriptionOptions);

            // Add a reference for garbage collected languages
            if (g_garbageCollected[consumer.lang])
            {
                m_consumerReferences.AddEntityRegistrationSubscriptionReference(consumer);
            }
        }
        else if (Dob::Typesystem::Operations::IsOfType(typeId, Dob::Service::ClassTypeId))
        {
            SubscriptionId subscriptionId(ConnectionConsumerPair(m_connection, consumer), ServiceRegistrationSubscription, 0);

            ServiceTypes::Instance().SubscribeRegistration(subscriptionId,
                                                           typeId,
                                                           handlerId,
                                                           includeSubclasses,
                                                           restartSubscription,
                                                           subscriptionOptions);

            // Add a reference for garbage collected languages
            if (g_garbageCollected[consumer.lang])
            {
                m_consumerReferences.AddServiceRegistrationSubscriptionReference(consumer);
            }
        }
        else
        {
            std::wostringstream ostr;
            ostr << "Type " << Typesystem::Operations::GetName(typeId)
                 << " used in SubscribeRegistration is not an Entity or Service type.";
            throw Typesystem::SoftwareViolationException(ostr.str(),__WFILE__,__LINE__);
        }
    }

    void Controller::UnsubscribeRegistration(const Safir::Dob::Typesystem::TypeId typeId,
                                             const Dob::Typesystem::HandlerId& handlerId,
                                             const bool includeSubclasses,
                                             const ConsumerId& consumer)
    {
        if (!m_isConnected)
        {
            std::wostringstream ostr;
            ostr << "This connection to the DOB is not open. (While calling UnsubscribeRegistration for type "
                 << Typesystem::Operations::GetName(typeId)
                 << " with handlerId " << handlerId << ")";
            throw Safir::Dob::NotOpenException(ostr.str(),__WFILE__,__LINE__);
        }

        if (Dob::Typesystem::Operations::IsOfType(typeId, Dob::Entity::ClassTypeId))
        {
            SubscriptionId subscriptionId(ConnectionConsumerPair(m_connection, consumer), EntityRegistrationSubscription, 0);

            EntityTypes::Instance().UnsubscribeRegistration(subscriptionId,
                                                            typeId,
                                                            handlerId,
                                                            includeSubclasses);
            // For garbage collected languages check if the consumer is used in any registration subscription.
            if (g_garbageCollected[consumer.lang])
            {
                m_consumerReferences.DropEntityRegistrationSubscriptionReferences(m_connection,
                                                                                  consumer,
                                                                                  boost::bind(&Dispatcher::InvokeDropReferenceCb,
                                                                                              m_dispatcher,
                                                                                              _1,
                                                                                              _2));
            }
        }
        else if (Dob::Typesystem::Operations::IsOfType(typeId, Dob::Service::ClassTypeId))
        {
            SubscriptionId subscriptionId(ConnectionConsumerPair(m_connection, consumer), ServiceRegistrationSubscription, 0);

            ServiceTypes::Instance().UnsubscribeRegistration(subscriptionId,
                                                             typeId,
                                                             handlerId,
                                                             includeSubclasses);
            // For garbage collected languages check if the consumer is used in any registration subscription.
            if (g_garbageCollected[consumer.lang])
            {
                m_consumerReferences.DropServiceRegistrationSubscriptionReferences(m_connection,
                                                                                   consumer,
                                                                                   boost::bind(&Dispatcher::InvokeDropReferenceCb,
                                                                                                m_dispatcher,
                                                                                                _1,
                                                                                                _2));
            }
        }
        else
        {
            std::wostringstream ostr;
            ostr << "Type " << Typesystem::Operations::GetName(typeId)
                 << " used in UnsubscribeRegistration is not an Entity or Service type.";
            throw Typesystem::SoftwareViolationException(ostr.str(),__WFILE__,__LINE__);
        }
    }

    //------------------------------------
    // Entity methods
    //------------------------------------
    void Controller::SetEntity(const char* const                blob,
                               const Typesystem::InstanceId&    instanceId,
                               const Typesystem::HandlerId&     handlerId,
                               const bool                       considerChangeFlags,
                               const bool                       initialInjection)
    {
        Dob::Typesystem::TypeId typeId = Dob::Typesystem::Internal::BlobOperations::GetTypeId(blob);

        Dob::Typesystem::EntityId entityId(typeId, instanceId);

        if (!m_isConnected)
        {
            std::wostringstream ostr;
            ostr << "This connection to the DOB is not open. (While calling SetEntity with type = "
                 << Typesystem::Operations::GetName(typeId) << " and instance = " << instanceId
                 << " and handler = " << handlerId << ")";
            throw Safir::Dob::NotOpenException(ostr.str(),__WFILE__,__LINE__);
        }

        if (!Dob::Typesystem::Operations::IsOfType(typeId, Dob::Entity::ClassTypeId))
        {
            std::wostringstream ostr;
            ostr << "Object passed to SetEntity is not an entity! (Type = "
                << Typesystem::Operations::GetName(typeId) << " and instance = " << instanceId
                << " and handler = " << handlerId << ")";
            throw Typesystem::SoftwareViolationException(ostr.str(),__WFILE__,__LINE__);
        }

        bool onInjNewCb = m_dispatcher.InCallback(CallbackId::OnInjectedNewEntity);
        bool onInjUpdCb = m_dispatcher.InCallback(CallbackId::OnInjectedUpdatedEntity);
        bool onInjDelCb = m_dispatcher.InCallback(CallbackId::OnInjectedDeletedEntity);

        if (onInjNewCb || onInjUpdCb || onInjDelCb)
        {
            if (typeId == m_dispatchedInjection.GetTypeId() &&
                instanceId == m_dispatchedInjection.GetInstanceId())
            {
                if (onInjNewCb || onInjUpdCb)
                {
                    if (!Dob::Typesystem::Internal::BlobOperations::IsChanged(blob))
                    {
                        // The app is calling Set in an OnInjectedNew or OnInjectedUpdate callback.
                        // In this case we check that the app really has changed some member and isn't
                        // just setting the same entity as received in the callback.
                        std::wostringstream ostr;
                        ostr << "Calling SetAll or SetChanges in an";
                        if (onInjNewCb)
                        {
                            ostr << " OnInjectedNewEntity";
                        }
                        else
                        {
                            ostr << " OnInjectedUpdatedEntity";
                        }
                        ostr << " callback with hasn't been changed!"
                             << " The app has probably not been modified to the new way of accepting"
                             << " an injected entity (the new way is to do nothing). (Type = "
                             << Typesystem::Operations::GetName(typeId) << " and instance = " << instanceId
                             << " and handler = " << handlerId << ")";
                        throw Safir::Dob::Typesystem::SoftwareViolationException(ostr.str(),__WFILE__,__LINE__);
                    }
                }
                // The app is calling set in an OnInject callback.
                m_setInjectedEntity = true;

                EntityTypes::Instance().SetInjection(m_connection,
                                                     handlerId,
                                                     entityId,
                                                     considerChangeFlags,
                                                     blob,
                                                     m_originalInjectionState);
                return;
            }
        }

        EntityTypes::Instance().SetEntity(m_connection,
                                          handlerId,
                                          entityId,
                                          blob,
                                          considerChangeFlags,
                                          initialInjection);
    }

    void Controller::DeleteEntity(const Dob::Typesystem::EntityId& entityId,
                                  const bool                       allInstances,
                                  const Typesystem::HandlerId&     handlerId)
    {
        if (!m_isConnected)
        {
            std::wostringstream ostr;
            ostr << "This connection to the DOB is not open. (While calling DeleteEntity with entityId = "
                 << entityId << ")";
            throw Safir::Dob::NotOpenException(ostr.str(),__WFILE__,__LINE__);
        }

        if (!Dob::Typesystem::Operations::IsOfType(entityId.GetTypeId(), Dob::Entity::ClassTypeId))
        {
            std::wostringstream ostr;
            ostr << "Entity id passed to DeleteEntity is not an entity! (entityId = "
                 << entityId << ")";
            throw Safir::Dob::Typesystem::SoftwareViolationException(ostr.str(),__WFILE__,__LINE__);
        }

        if (!allInstances &&
            (m_dispatcher.InCallback(CallbackId::OnInjectedNewEntity) ||
            m_dispatcher.InCallback(CallbackId::OnInjectedUpdatedEntity) ||
            m_dispatcher.InCallback(CallbackId::OnInjectedDeletedEntity)))
        {
            if (entityId.GetTypeId() == m_dispatchedInjection.GetTypeId() &&
                entityId.GetInstanceId() == m_dispatchedInjection.GetInstanceId())
            {
                // The app is deleting the injected entity in an OnInject callback.
                m_deleteInjectedEntity = true;

                EntityTypes::Instance().DeleteInjection(m_connection, handlerId, entityId, m_originalInjectionState);
                return;
            }
        }

        EntityTypes::Instance().DeleteEntity(m_connection,
                                             handlerId,
                                             entityId,
                                             allInstances);
    }

    void Controller::InjectEntity(const char* const                blob,
                                  const Typesystem::InstanceId&    instanceId,
                                  const Typesystem::HandlerId&     handlerId,
                                  const Dob::Typesystem::Int64     timestamp)
    {

        Dob::Typesystem::TypeId typeId = Dob::Typesystem::Internal::BlobOperations::GetTypeId(blob);

        if (!m_isConnected)
        {
            std::wostringstream ostr;
            ostr << "This connection to the DOB is not open. (While calling InjectEntity with type = "
                 << Typesystem::Operations::GetName(typeId) << " and instance = " << instanceId
                 << " and handler = " << handlerId << ")";
            throw Safir::Dob::NotOpenException(ostr.str(),__WFILE__,__LINE__);
        }

        if (!Dob::Typesystem::Operations::IsOfType(typeId, Dob::Entity::ClassTypeId))
        {
            std::wostringstream ostr;
            ostr << "Object passed to InjectEntity is not an entity! (Type = "
                 << Typesystem::Operations::GetName(typeId) << " and instance = " << instanceId
                 << " and handler = " << handlerId << ")";
            throw Safir::Dob::Typesystem::SoftwareViolationException(ostr.str(),__WFILE__,__LINE__);
        }

    EntityTypes::Instance().InjectEntity(m_connection,
                                         handlerId,
                                         Dob::Typesystem::EntityId(typeId, instanceId),
                                         blob,
                                         timestamp);
    }

    void Controller::InjectDeletedEntity(const Dob::Typesystem::EntityId& entityId,
                                         const Typesystem::HandlerId&     handlerId,
                                         const Dob::Typesystem::Int64     timestamp)
    {
        if (!m_isConnected)
        {
            std::wostringstream ostr;
            ostr << "This connection to the DOB is not open. (While calling InjectDeletedEntity with entityId = "
                 << entityId << ")";
            throw Safir::Dob::NotOpenException(ostr.str(),__WFILE__,__LINE__);
        }

        if (!Dob::Typesystem::Operations::IsOfType(entityId.GetTypeId(), Dob::Entity::ClassTypeId))
        {
            std::wostringstream ostr;
            ostr << "Entity id passed to DeleteEntity is not an entity! (entityId = "
                 << entityId << ")";
            throw Safir::Dob::Typesystem::SoftwareViolationException(ostr.str(),__WFILE__,__LINE__);
        }

        EntityTypes::Instance().InjectDeletedEntity(m_connection,
                                                    handlerId,
                                                    entityId,
                                                    timestamp);
    }

    void
    Controller::ReadEntity(const Typesystem::EntityId& entityId,
                           const char*& currentBlob,
                           const char*& currentState)
    {
        if (!m_isConnected)
        {
            std::wostringstream ostr;
            ostr << "This connection to the DOB is not open. (While calling ReadEntity for " << entityId << ")";
            throw Safir::Dob::NotOpenException(ostr.str(),__WFILE__,__LINE__);
        }

        const DistributionData state = EntityTypes::Instance().ReadEntity(entityId, m_connection->Id().m_contextId);
        currentBlob = state.GetBlob();
        currentState = state.GetReference();
        //Note that we count up the refcount by one here, which the proxy *has* to drop
        //on destruction!
    }

    bool Controller::IsCreated(const Dob::Typesystem::EntityId& entityId)
    {
        if (!m_isConnected)
        {
            std::wostringstream ostr;
            ostr << "This connection to the DOB is not open. (While calling IsCreated for " << entityId << ")";
            throw Safir::Dob::NotOpenException(ostr.str(),__WFILE__,__LINE__);
        }

        return EntityTypes::Instance().IsCreated(entityId, m_connection->Id().m_contextId);
    }

    InstanceIdPolicy::Enumeration Controller::GetInstanceIdPolicy(const Typesystem::TypeId typeId,
                                                      const Typesystem::HandlerId& handlerId) const
    {
        if (!m_isConnected)
        {
            std::wostringstream ostr;
            ostr << "This connection to the DOB is not open. (While calling GetInstanceIdPolicy for typeId=" << typeId << ")";
            throw Safir::Dob::NotOpenException(ostr.str(),__WFILE__,__LINE__);
        }
        if (!Dob::Typesystem::Operations::IsOfType(typeId, Dob::Entity::ClassTypeId))
        {
            std::wostringstream ostr;
            ostr << "TypeId passed to GetInstanceIdPolicy is not an entity! (typeId = "
                 << typeId << ")";
            throw Safir::Dob::Typesystem::SoftwareViolationException(ostr.str(),__WFILE__,__LINE__);
        }
        if (!EntityTypes::Instance().IsRegistered(typeId, handlerId, m_connection->Id().m_contextId))
        {
            std::wostringstream ostr;
            ostr << "TypeId passed to GetInstanceIdPolicy is not registered by the passed HandlerId! (typeId = "
                << typeId << "handlerId = " << handlerId.GetRawValue() << ")";
            throw Safir::Dob::NotFoundException(ostr.str(),__WFILE__,__LINE__);
        }

        
        ConnectionConsumerPair regOwner;
        InstanceIdPolicy::Enumeration policy;
        EntityTypes::Instance().GetRegisterer(typeId, handlerId, m_connection->Id().m_contextId, regOwner, policy);

        return policy;
    }


    Typesystem::Int32
    Controller::GetQueueCapacity(const ConnectionQueueId::Enumeration queue)
    {
        if (queue < 0 || queue >= ConnectionQueueId::Size())
        {
            std::wostringstream ostr;
            ostr << "Getting queue capacity for queue " << queue << "is not implemented (or that is not a queue!";
            throw Safir::Dob::Typesystem::SoftwareViolationException(ostr.str(),__WFILE__,__LINE__);
        }

        return static_cast<Typesystem::Int32>(m_connection->QueueCapacity(queue));
    }

    Typesystem::Int32
    Controller::GetQueueSize(const ConnectionQueueId::Enumeration queue)
    {
        if (!m_isConnected)
        {
            std::wostringstream ostr;
            ostr << "This connection to the DOB is not open. (While calling GetQueueSize for "
                << ConnectionQueueId::ToString(queue) << ")";
            throw Safir::Dob::NotOpenException(ostr.str(),__WFILE__,__LINE__);
        }
        switch (queue)
        {
        case ConnectionQueueId::MessageOutQueue:
            return static_cast<Typesystem::Int32>(m_connection->GetMessageOutQueue().size());

        case ConnectionQueueId::RequestOutQueue:
            return static_cast<Typesystem::Int32>(m_connection->GetRequestOutQueue().size());

        default:
            std::wostringstream ostr;
            ostr << "Getting queue size for queue " << queue << "is not implemented (or that is not a queue!";
            throw Safir::Dob::Typesystem::SoftwareViolationException(ostr.str(),__WFILE__,__LINE__);
        }
    }

    //----------------------------------------
    //Send message
    //----------------------------------------
    void Controller::SendMessage(const char * const blob,
                                 const Typesystem::ChannelId & channel,
                                 const ConsumerId & consumer)
    {
        const Dob::Typesystem::TypeId typeId = Dob::Typesystem::Internal::BlobOperations::GetTypeId(blob);
        if (!m_isConnected)
        {
            std::wostringstream ostr;
            ostr << "This connection to the DOB is not open. (While calling Send(Message) with "
                 << Typesystem::Operations::GetName(typeId) << ")";
            throw Safir::Dob::NotOpenException(ostr.str(),__WFILE__,__LINE__);
        }

        if (!Dob::Typesystem::Operations::IsOfType(typeId, Dob::Message::ClassTypeId))
        {
            std::wostringstream ostr;
            ostr << "Object passed to Send(Message) is not a Message. TypeId = "
                 << Typesystem::Operations::GetName(typeId);
             throw Typesystem::SoftwareViolationException(ostr.str(),__WFILE__,__LINE__);
        }

        if (channel == Dob::Typesystem::ChannelId::ALL_CHANNELS)
        {
            std::wostringstream ostr;
            ostr << "Not allowed to send a message on ALL_CHANNELS. TypeId = "
                 << Typesystem::Operations::GetName(typeId);
             throw Typesystem::SoftwareViolationException(ostr.str(),__WFILE__,__LINE__);
        }

        DistributionData msg(message_tag, m_connection->Id(), channel, blob);

        if (ContextSharedTable::Instance().IsContextShared(msg.GetTypeId()) &&
            m_connection->Id().m_contextId != 0)
        {
            std::wostringstream ostr;
            ostr << "A message that has the property ContextShared can only be sent from context 0 . TypeId = "
                 << Typesystem::Operations::GetName(typeId);
             throw Typesystem::SoftwareViolationException(ostr.str(),__WFILE__,__LINE__);            
        }

        const bool success = m_connection->GetMessageOutQueue().push(msg);

        if (success)
        {
            m_connection->SignalOut();
        }
        else
        {
            m_messageQueueInOverflowState = true;
            m_dispatcher.AddOverflowedMessageConsumer(consumer);
            throw Safir::Dob::OverflowException(L"Overflow when sending message.",__WFILE__,__LINE__);
        }
    }

    //-------------------------------------------
    // Request methods
    //-------------------------------------------
    void Controller::ServiceRequest(const char * const blob,
                                    const Typesystem::HandlerId& handlerId,
                                    const ConsumerId& consumer,
                                    RequestId & requestId)
    {
        const Typesystem::TypeId typeId=Dob::Typesystem::Internal::BlobOperations::GetTypeId(blob);

        if (!m_isConnected)
        {
            std::wostringstream ostr;
            ostr << "This connection to the DOB is not open. (While calling Send(ServiceRequest) with type = "
                << Typesystem::Operations::GetName(typeId) << " and handler = " << handlerId << ")";
            throw Safir::Dob::NotOpenException(ostr.str(),__WFILE__,__LINE__);
        }

        if (!Dob::Typesystem::Operations::IsOfType(typeId, Dob::Service::ClassTypeId))
        {
            std::wostringstream ostr;
            ostr << "Object passed to Send(ServiceRequest) is not a Service! (Type = "
                << Typesystem::Operations::GetName(typeId) << " and handler = " << handlerId << ")";
            throw Typesystem::SoftwareViolationException(ostr.str(),__WFILE__,__LINE__);
        }

        const InternalRequestId reqIdCounter = m_requestIds.GetNextRequestId();
        requestId = reqIdCounter.GetCounter();

        DistributionData request(service_request_tag,
                                 m_connection->Id(),
                                 handlerId,
                                 reqIdCounter,
                                 blob);

        SendRequest(request, consumer);
    }

    void Controller::CreateRequest(const char * const blob,
                                   bool hasInstanceId,
                                   Typesystem::InstanceId instanceId,
                                   const Typesystem::HandlerId& handlerId,
                                   const ConsumerId& consumer,
                                   RequestId& requestId)
    {

        const Typesystem::TypeId typeId = Dob::Typesystem::Internal::BlobOperations::GetTypeId(blob);

        if (!m_isConnected)
        {
            std::wostringstream ostr;
            ostr << "This connection to the DOB is not open. (While calling CreateRequest with type = "
                 << Typesystem::Operations::GetName(typeId) << " and handler = " << handlerId << ")";
            throw Safir::Dob::NotOpenException(ostr.str(),__WFILE__,__LINE__);
        }

        if (!Typesystem::Operations::IsOfType(typeId, Dob::Entity::ClassTypeId))
        {
            std::wostringstream ostr;
            ostr << "Object passed to CreateRequest is not an entity! (Type = "
                << Typesystem::Operations::GetName(typeId) << " and handler = " << handlerId << ")";
            throw Typesystem::SoftwareViolationException(ostr.str(),__WFILE__,__LINE__);
        }

        ConnectionConsumerPair regOwner;
        InstanceIdPolicy::Enumeration policy;
        const ContextId context =
            ContextSharedTable::Instance().IsContextShared(typeId) ? 0 : m_connection->Id().m_contextId;
        EntityTypes::Instance().GetRegisterer(typeId, handlerId, context, regOwner, policy);
        //Only check the policy if the handler is registered. Otherwise we let dose_main
        //send an unregistered-response.
        if (regOwner.connection != NULL)
        {
            switch(policy)
            {
            case InstanceIdPolicy::HandlerDecidesInstanceId:
                {
                    if (hasInstanceId)
                    {
                        std::wostringstream ostr;
                        ostr << "This handler is registered as HandlerDecidesInstanceId, so you "
                            << "cannot specify an instance in your CreateRequest. (Type = "
                            << Typesystem::Operations::GetName(typeId) << " and handler = " << handlerId << ")";
                        throw Typesystem::SoftwareViolationException(ostr.str(),__WFILE__,__LINE__);
                    }
                }
                break;

            case InstanceIdPolicy::RequestorDecidesInstanceId:
                {
                    if (!hasInstanceId)
                    {
                        instanceId = Typesystem::InstanceId::GenerateRandom();
                        hasInstanceId = true;
                    }
                }
                break;
            }
        }

        const InternalRequestId reqIdCounter = m_requestIds.GetNextRequestId();
        requestId = reqIdCounter.GetCounter();

        DistributionData request(entity_create_request_tag,
                                 m_connection->Id(),
                                 handlerId,
                                 reqIdCounter,
                                 hasInstanceId,
                                 instanceId,
                                 blob);

        SendRequest(request, consumer);
    }

    void Controller::UpdateRequest(const char * const blob,
                                   const Typesystem::InstanceId& instanceId,
                                   const ConsumerId& consumer,
                                   RequestId& requestId)
    {
        const Typesystem::TypeId typeId=Dob::Typesystem::Internal::BlobOperations::GetTypeId(blob);

        if (!m_isConnected)
        {
            std::wostringstream ostr;
            ostr << "This connection to the DOB is not open. (While calling UpdateRequest with type = "
                 << Typesystem::Operations::GetName(typeId) << " and instance = " << instanceId << ")";
            throw Safir::Dob::NotOpenException(ostr.str(),__WFILE__,__LINE__);
        }

        if (!Dob::Typesystem::Operations::IsOfType(typeId, Dob::Entity::ClassTypeId))
        {
            std::wostringstream ostr;
            ostr << "Object passed to UpdateRequest is not an entity! (Type = "
                << Typesystem::Operations::GetName(typeId) << " and instance = " << instanceId << ")";
            throw Typesystem::SoftwareViolationException(ostr.str(),__WFILE__,__LINE__);
        }


        const InternalRequestId reqIdCounter = m_requestIds.GetNextRequestId();
        requestId = reqIdCounter.GetCounter();

        DistributionData request(entity_update_request_tag,
                                 m_connection->Id(),
                                 reqIdCounter,
                                 instanceId,
                                 blob);

        SendRequest(request, consumer);
    }

    void Controller::DeleteRequest(const Typesystem::EntityId& entityId,
                                   const ConsumerId& consumer,
                                   RequestId & requestId)
    {
        if (!m_isConnected)
        {
            std::wostringstream ostr;
            ostr << "This connection to the DOB is not open. (While calling DeleteRequest with entityId = "
                << entityId << ")";
            throw Safir::Dob::NotOpenException(ostr.str(),__WFILE__,__LINE__);
        }

        if (!Dob::Typesystem::Operations::IsOfType(entityId.GetTypeId(), Dob::Entity::ClassTypeId))
        {
            std::wostringstream ostr;
            ostr << "Object passed to DeleteRequest is not an entity! entityId = "
                << entityId;
            throw Typesystem::SoftwareViolationException(ostr.str(),__WFILE__,__LINE__);
        }

        const InternalRequestId reqIdCounter = m_requestIds.GetNextRequestId();
        requestId = reqIdCounter.GetCounter();

        DistributionData request(entity_delete_request_tag,
                                 m_connection->Id(),
                                 reqIdCounter,
                                 entityId);

        SendRequest(request, consumer);
    }

    void CheckResponseType(const DistributionData& request, const DistributionData& response)
    {
        if (Typesystem::Operations::IsOfType(response.GetTypeId(),Safir::Dob::EntityIdResponse::ClassTypeId))
        {
            ConnectionConsumerPair regOwner;
            InstanceIdPolicy::Enumeration policy;
            const ContextId context =
                ContextSharedTable::Instance().IsContextShared(request.GetTypeId()) ? 0 : request.GetSenderId().m_contextId;
            EntityTypes::Instance().GetRegisterer(request.GetTypeId(), request.GetHandlerId(), context, regOwner, policy);

            if (policy == InstanceIdPolicy::RequestorDecidesInstanceId)
            {
                std::wostringstream ostr;
                ostr << "This handler is registered as RequestorDecidesInstanceId, so you "
                     << "cannot send a response that derives from EntityIdResponse. (ResponseType = "
                     << Typesystem::Operations::GetName(response.GetTypeId()) << " and handler = " << request.GetHandlerId() << ")";
                throw Typesystem::SoftwareViolationException(ostr.str(),__WFILE__,__LINE__);
            }
        }
    }

    //-----------------------------------------
    // Response method
    //-----------------------------------------
    void Controller::SendResponse(const char * const blob,
                                  const ConsumerId & consumer,
                                  const ResponseId responseId)
    {
        const Dob::Typesystem::TypeId typeId = Dob::Typesystem::Internal::BlobOperations::GetTypeId(blob);
        if (!m_isConnected)
        {
            std::wostringstream ostr;
            ostr << "This connection to the DOB is not open. (While calling SendResponse with "
                << Safir::Dob::Typesystem::Operations::GetName(typeId) << ")";
            throw Safir::Dob::NotOpenException(ostr.str(),__WFILE__,__LINE__);
        }

        if (!Dob::Typesystem::Operations::IsOfType(typeId, Dob::Response::ClassTypeId))
        {
            std::wostringstream ostr;
            ostr << "Object passed to Send(Response) is not a response! TypeId = "
                << Safir::Dob::Typesystem::Operations::GetName(typeId);
            throw Typesystem::SoftwareViolationException(ostr.str(),__WFILE__,__LINE__);
        }

        m_connection->ForSpecificRequestInQueue(consumer,
            boost::bind(&RequestInQueue::AttachResponse,_2,responseId,m_connection->Id(),blob, CheckResponseType));

        m_connection->SignalOut();
    }

    //--------------------------------------------
    // Compose name of the form <node name>:<common part>:<instance part>
    //--------------------------------------------
    const std::string Controller::ComposeName(const ContextId contextId,
                                              const std::string& commonPart,
                                              const std::string& instancePart)
    {
        std::string name(Dob::Typesystem::Utilities::ToUtf8(NodeParameters::Nodes(Dob::ThisNodeParameters::NodeNumber())->NodeName().GetVal()));
        name.append(";");
        name.append(boost::lexical_cast<std::string>(contextId));
        name.append(";");
        name.append(commonPart);
        name.append(";");
        name.append(instancePart);
        return name;
    }

    //---------------------------------------
    // Dispatch the queues
    //---------------------------------------
    void Controller::Dispatch()
    {
        m_exitDispatch=false;

        if (!m_isConnected)
        {
            return;
        }

        // First dispatch response in queue
        DispatchResponseInQueue();

        // The dispatching of the response in queue might have released space in the request out queue, tell
        // a user with waiting requests that it is worth trying to send them.
        if (m_requestQueueInOverflowState)
        {
            if(!m_connection->GetRequestOutQueue().full())
            {
                m_requestQueueInOverflowState = false;
                m_dispatcher.DispatchNotRequestOverflows();
                m_postponedTypes.Clear();
                //no need to signal, since we're already in dispatch, and
                //requests will be dispatched below
            }
        }

        // The message out queue isn't "related" to a corresponding in queue (as the request out queue) but
        // this could be a good place to notify the user anyway.
        if (m_messageQueueInOverflowState)
        {
            if(!m_connection->GetMessageOutQueue().full())
            {
                m_messageQueueInOverflowState = false;
                m_dispatcher.DispatchNotMessageOverflows();
                m_postponedTypes.Clear();
                //no need to signal, since we're already in dispatch, and
                //messages will be dispatched below
            }
        }

        DispatchRequestInQueues();
        DispatchMessageInQueues();

        // Check subscriptions
        if (!m_exitDispatch)
        {
            DispatchSubscriptions();
        }

        // Dispatch OnInitialInjectionsDone for injection handlers where the registration didn't
        // created any injection subscriptions (because there where no instances)
        if (!m_exitDispatch)
        {
            DispatchEmptyInitialInjections();
        }

        //Check pending ownerships
        if (!m_exitDispatch)
        {
            //Note that exitDispatch is not checked inside this call, so all prs will be dispatched.
            HandlePendingRegistrations();
        }

        // Check revoked registrations
        if (!m_exitDispatch)
        {
            HandleRevokedRegistrations();
        }

        //Stop order
        if (m_connection->StopOrderPending() && !m_exitDispatch)
        {
            lllog(3) << "Dispatching stop order" << std::endl;
            m_dispatcher.DispatchStopOrder();

            if (m_connection != NULL)
            {
                m_connection->SetStopOrderHandled();
            }
        }
    }

    void Controller::ExitDispatch()
    {
        m_exitDispatch=true;
        m_connection->SignalIn();
    }


    Safir::Dob::CallbackId::Enumeration
    Controller::CurrentCallback() const
    {
        const CallbackStack & cbStack = m_dispatcher.GetCallbackStack();
        if (cbStack.empty())
        {
            return Safir::Dob::CallbackId::None;
        }
        else
        {
            return cbStack.top().m_callbackId;
        }
    }

    void Controller::Postpone(const bool redispatchCurrent)
    {
        m_postpone = true;
        m_postponeCurrent = redispatchCurrent;
    }

    void Controller::ResumePostponed()
    {
        m_postpone = false;
        m_postponedTypes.Clear();
        m_connection->SignalIn();
    }

    void Controller::IncompleteInjectionState()
    {
        m_incompleteInjection = true;
    }


    void Controller::DispatchRequest(const ConsumerId& consumer,
                                     const DistributionData& request,
                                     bool& exitDispatch,
                                     bool& dontRemove)
    {
        dontRemove = false;
        m_postponeCurrent = false;
        m_postpone = false;

        switch (request.GetType())
        {
        case DistributionData::Request_Service:
            {
                if (m_postponedTypes.IsPostponed(consumer, request.GetTypeId(), CallbackId::OnServiceRequest))
                {
                    dontRemove = true;
                }
                else
                {
                    m_dispatcher.InvokeOnServiceRequestCb(consumer,
                                                          request,
                                                          m_ctrlId);

                    if (m_postpone)
                    {
                        m_postponedTypes.Postpone(consumer,request.GetTypeId(), CallbackId::OnServiceRequest);
                    }
                }
            }
            break;
        case DistributionData::Request_EntityCreate:
            {
                if (m_postponedTypes.IsPostponed(consumer, request.GetTypeId(), CallbackId::OnCreateRequest))
                {
                    dontRemove = true;
                }
                else
                {
                    m_dispatcher.InvokeOnCreateRequestCb(consumer,
                                                         request,
                                                         m_ctrlId);

                    if (m_postpone)
                    {
                        m_postponedTypes.Postpone(consumer,request.GetTypeId(), CallbackId::OnCreateRequest);
                    }
                }
            }
            break;
        case DistributionData::Request_EntityUpdate:
            {
                const bool isOwner = EntityTypes::Instance().IsOwner(request.GetEntityId(),
                                                                     request.GetHandlerId(),
                                                                     ConnectionConsumerPair(m_connection,consumer));

                if (isOwner)
                {
                    if (m_postponedTypes.IsPostponed(consumer, request.GetTypeId(), CallbackId::OnUpdateRequest))
                    {
                        dontRemove = true;
                    }
                    else
                    {
                        m_dispatcher.InvokeOnUpdateRequestCb(consumer,
                                                             request,
                                                             m_ctrlId);

                        if (m_postpone)
                        {
                            m_postponedTypes.Postpone(consumer,request.GetTypeId(), CallbackId::OnUpdateRequest);
                        }
                    }
                }
                else
                {
                    // No such instance, or the instance is owned by other handler/connection/consumer.
                    std::wostringstream ostr;
                    ostr << "The instance " << request.GetEntityId() << " does not exist, or isn't owned by handler "
                         << request.GetHandlerId() << " and/or connection " << m_connection->NameWithoutCounter();

                    Dob::ErrorResponsePtr errorResponse = Dob::ErrorResponse::CreateErrorResponse
                        (Dob::ResponseGeneralErrorCodes::SafirNotRegistered(),
                        ostr.str());

                    //TODO: write directly to shared memory
                    Typesystem::BinarySerialization bin;
                    Typesystem::Serialization::ToBinary(errorResponse,bin);

                    SendResponse(&bin[0],
                                 consumer,
                                 request.GetResponseId());
                }
            }
            break;
        case DistributionData::Request_EntityDelete:
            {
                const bool isOwner = EntityTypes::Instance().IsOwner(request.GetEntityId(),
                                                                     request.GetHandlerId(),
                                                                     ConnectionConsumerPair(m_connection,consumer));

                if (isOwner)
                {
                    if (m_postponedTypes.IsPostponed(consumer, request.GetTypeId(), CallbackId::OnDeleteRequest))
                    {
                        dontRemove = true;
                    }
                    else
                    {
                        m_dispatcher.InvokeOnDeleteRequestCb(consumer,
                                                             request,
                                                             m_ctrlId);

                        if (m_postpone)
                        {
                            m_postponedTypes.Postpone(consumer,request.GetTypeId(), CallbackId::OnDeleteRequest);
                        }
                    }
                }
                else
                {
                    // No such instance, or the instance is owned by other handler/connection/consumer.
                    std::wostringstream ostr;
                    ostr << "The instance " << request.GetEntityId() << " does not exist, or isn't owned by handler "
                         << request.GetHandlerId() << " and/or connection " << m_connection->NameWithoutCounter();

                    Dob::ErrorResponsePtr errorResponse = Dob::ErrorResponse::CreateErrorResponse
                        (Dob::ResponseGeneralErrorCodes::SafirNotRegistered(),
                        ostr.str());

                    //TODO: write directly to shared memory
                    Typesystem::BinarySerialization bin;
                    Typesystem::Serialization::ToBinary(errorResponse,bin);

                    SendResponse(&bin[0],
                                 consumer,
                                 request.GetResponseId());
                }
            }
            break;
        default:
            ENSURE(false, << "Unexpected DistributionData type in DispatchRequestInQueue: " << request.GetType());
        }

        if (m_postponeCurrent)
        {
            dontRemove = true;
        }
        exitDispatch = m_exitDispatch;
    }

    void Controller::DispatchRequestInQueue(const ConsumerId& consumer, RequestInQueue & queue)
    {
        queue.DispatchRequests(boost::bind(&Controller::DispatchRequest,this,boost::cref(consumer),_1,_2,_3),
                               boost::bind(&Connection::SignalOut,m_connection.get().get()));
    }


    void Controller::DispatchRequestInQueues()
    {
        lllout << "Dispatching all RequestInQueues" << std::endl;
        m_connection->ForEachRequestInQueue(boost::bind(&Controller::DispatchRequestInQueue,this,_1,_2));
    }

    void Controller::DispatchResponse(const DistributionData & response,
                                      const DistributionData & request,
                                      bool & exitDispatch)
    {
        m_dispatcher.InvokeOnResponseCb(response,request);
        exitDispatch = m_exitDispatch;
    }

    void Controller::DispatchResponseInQueue()
    {
        lllout << "Dispatching ResponseInQueue" << std::endl;
        m_connection->GetRequestOutQueue().DispatchResponses(boost::bind(&Controller::DispatchResponse,this,_1,_2,_3));
    }

    void Controller::DispatchMessage(const ConsumerId& consumer, const DistributionData& msg, bool& exitDispatch, bool& dontRemove)
    {
        dontRemove = false;
        if (m_postponedTypes.IsPostponed(consumer, msg.GetTypeId(), CallbackId::OnMessage))
        {
            dontRemove = true;
        }
        else
        {
            m_postponeCurrent = false;
            m_postpone = false;

            m_dispatcher.InvokeOnMessageCb(consumer, msg);

            if (m_postpone)
            {
                m_postponedTypes.Postpone(consumer,msg.GetTypeId(), CallbackId::OnMessage);
            }
            if (m_postponeCurrent)
            {
                dontRemove = true;
            }
        }
        exitDispatch = m_exitDispatch;
    }

    void Controller::DispatchMessageInQueue(const ConsumerId& consumer, MessageQueue& queue)
    {
        queue.Dispatch(boost::bind(&Controller::DispatchMessage,this,boost::cref(consumer),_1,_2,_3),NULL);
    }

    void Controller::DispatchMessageInQueues()
    {
        m_connection->ForEachMessageInQueue(boost::bind(&Controller::DispatchMessageInQueue,this,_1,_2));
    }

    bool ProcessHasBeenDeleted(bool & unreg, bool & reg)
    {
        unreg = true;
        reg = true;
        return true;
    }

    void CalculateRegistrationSubscriptionChanges(const SubscriptionPtr&  subscription,
                                                  const DistributionData& lastState,
                                                  const DistributionData& currState,
                                                  bool&                   unreg,
                                                  bool&                   reg)
    {
        unreg = false;
        reg = false;

        bool lastIsReg = !lastState.IsNoState() && lastState.IsRegistered();
        bool currIsReg = !currState.IsNoState() && currState.IsRegistered();

        if (!lastIsReg && currIsReg)
        {
            reg =true;
        }
        else if (lastIsReg && !currIsReg)
        {
            unreg = true;
        }
        else if (lastIsReg && currIsReg)
        {
            // Both last and current state is 'registered', but can it be that there has
            // been a unregister->register?
            subscription->HasBeenDeletedFlag().Process(boost::bind(ProcessHasBeenDeleted,boost::ref(unreg), boost::ref(reg)));
        }
    }

    bool Controller::ProcessRegistrationSubscription(const SubscriptionPtr& subscription, bool& exitDispatch)
    {
        const DistributionData currDistState = subscription->GetCurrentRealState();
        const DistributionData lastDistState = subscription->GetLastRealState();

        bool unreg = false;
        bool reg = false;

        // Get state changes by comparing the state that was last dispatched with current state
        CalculateRegistrationSubscriptionChanges(subscription, lastDistState, currDistState, unreg, reg);

        const ConsumerId consumer = subscription->GetSubscriptionId().connectionConsumer.consumer;

        if (unreg)
        {
            m_dispatcher.InvokeOnUnregisteredCb(consumer, currDistState.GetTypeId(), currDistState.GetHandlerId());
        }

        if (reg)
        {
            m_dispatcher.InvokeOnRegisteredCb(consumer, currDistState.GetTypeId(), currDistState.GetHandlerId());
        }

        subscription->SetLastRealState(currDistState);

        lllout << "Controller::ProcessRegistrationSubscription unreg:" << unreg << "reg:" << reg << std::endl;

        exitDispatch = m_exitDispatch;
        return true;
    }

    void Controller::DispatchRegistrationSubscription(const SubscriptionPtr& subscription, bool& exitDispatch, bool& dontRemove)
    {
        exitDispatch = false;
        dontRemove = false;  // not possible to postpone registration subscription callbacks

        if (!subscription->IsSubscribed())
        {
            // An unsubscribe has been invoked, drop this subscription.
            return;
        }

        subscription->DirtyFlag().Process(boost::bind(&Controller::ProcessRegistrationSubscription,
                                                      this,
                                                      boost::cref(subscription),
                                                      boost::ref(exitDispatch)));
    }

    namespace //anonymous namespace to avoid possible name collisions
    {
        enum StateKind {None, Real, Ghost};
        static const wchar_t * const StateKindNames [] = {L"None", L"Real", L"Ghost"};

        StateKind GetStateKind(const DistributionData& state)
        {
            if (state.IsNoState())
            {
                return None;
            }

            if (!state.HasBlob())
            {
                //A ghost without blob is actually a delete ghost, probably initiated by CleanGhosts
                //and state None is the correct one even in that case.
                return None;
            }

            if (state.GetEntityStateKind() == DistributionData::Ghost)
            {
                return Ghost;
            }

            return Real;
        }
    }

    void CalculateEntitySubscriptionChanges(const SubscriptionPtr&  subscription,
                                            const DistributionData& lastState,
                                            const DistributionData& currState,
                                            bool& created,
                                            bool& updated,
                                            bool& deleted,
                                            bool& deleteIsExplicit)
    {
        created = false;
        updated = false;
        deleted = false;
        deleteIsExplicit = false;

        if (lastState == currState)
        {
            return;
        }

        const StateKind lastKind = GetStateKind(lastState);
        const StateKind currKind = GetStateKind(currState);

        if (Safir::Utilities::Internal::Internal::LowLevelLogger::Instance().LogLevel() >= 9)
        {
            Safir::Dob::Typesystem::EntityId eid;
            if (!lastState.IsNoState())
            {
                eid = lastState.GetEntityId();
            }
            else if (!currState.IsNoState())
            {
                eid = currState.GetEntityId();
            }
            lllout << "Got state change " << StateKindNames[lastKind]
                   << " --> " << StateKindNames[currKind]
                   << " for entity " << eid << std::endl;
        }

        const EntitySubscriptionOptions & subscriptionOptions =
            *boost::interprocess::static_pointer_cast<const EntitySubscriptionOptions>(subscription->GetSubscriptionOptions());

        ENSURE(!subscriptionOptions.wantsAllStateChanges,
            << "CalculateEntitySubscriptionChanges is not intended to dispatch subscriptions "
            << "that has WantsAllStateChanges flag");

        //Filter out persistence handlers own sets (and the accepts from app).
        if (currKind != None && currState.SourceIsPermanentStore() && subscriptionOptions.doesntWantSourceIsPermanentStore)
        {
            return;
        }

        //Real --> Real
        if (lastKind == Real && currKind == Real && subscriptionOptions.includeUpdates)
        {
            if (lastState.GetCreationTime() != currState.GetCreationTime() ||
                lastState.GetVersion() != currState.GetUndecrementedVersion())
            {
                updated = true;
            }
            return;
        }

        //None --> Real
        if (lastKind == None && currKind == Real)
        {
            created = true;
            return;
        }

        //Real --> None
        if (lastKind == Real && currKind == None)
        {
            deleted = true;
            deleteIsExplicit = currState.IsExplicitlyDeleted();
            return;
        }

        //Ghost --> Real
        if (lastKind == Ghost && currKind == Real)
        {
            created = true;
            return;
        }

        //None --> Ghost
        if (lastKind == None && currKind == Ghost)
        {
            if (subscriptionOptions.wantsLastState)
            {
                created = true;
                deleted = true;
                deleteIsExplicit = false;
            }
            return;
        }

        //Real --> Ghost
        if (lastKind == Real && currKind == Ghost)
        {
            if (subscriptionOptions.wantsLastState && subscriptionOptions.includeUpdates)
            {
                if (lastState.GetCreationTime() != currState.GetCreationTime() ||
                    lastState.GetVersion().IsDiffGreaterThanOne(currState.GetUndecrementedVersion()))
                {
                    updated = true;
                }
            }

            deleted = true;
            deleteIsExplicit = false;
            return;
        }

        //Ghost --> Ghost
        if (lastKind == Ghost && currKind == Ghost)
        {
            if (subscriptionOptions.wantsLastState)
            {
                if (lastState.GetCreationTime() != currState.GetCreationTime())
                {
                    created = true;
                    deleted = true;
                    deleteIsExplicit = false;
                }
            }
            return;
        }

        //Ghost --> None
        if (lastKind == Ghost && currKind == None)
        {
            if (subscriptionOptions.wantsGhostDelete)
            {
                deleted = true;
                deleteIsExplicit = currState.IsExplicitlyDeleted();
            }

            return;
        }
    }

    bool Controller::ProcessEntitySubscription(const SubscriptionPtr& subscription,
                                               bool& exitDispatch,
                                               bool& dontRemove)
    {
        //TODO: this routine currently ignores the problem of getting postpones on an OnDeletedEntity
        //when it is a WantsLastState case that gets dispatched.
        //In this case the last state will be dispatched twice if the OnDeletedEntity gets postponed.
        //This must be fixed, somehow...

        const DistributionData currState = subscription->GetCurrentRealState();
        const DistributionData lastState = subscription->GetLastRealState();

        bool created;
        bool updated;
        bool deleted;
        bool deleteIsExplicit;

        // Get state changes by comparing the state that was last dispatched with current state
        CalculateEntitySubscriptionChanges(subscription, lastState, currState, created, updated, deleted, deleteIsExplicit);

        dontRemove = false;
        exitDispatch = false;
        m_postponeCurrent = false;
        m_postpone = false;

        const EntitySubscriptionOptions & subscriptionOptions =
            *boost::interprocess::static_pointer_cast<const EntitySubscriptionOptions>(subscription->GetSubscriptionOptions());

        const bool timestampChangeInfo = subscriptionOptions.timestampChangeInfo;

        const ConsumerId consumer = subscription->GetSubscriptionId().connectionConsumer.consumer;

        if(created)
        {
            if (m_postponedTypes.IsPostponed(consumer,
                                             currState.GetTypeId(),
                                             CallbackId::OnNewEntity))
            {
                dontRemove = true;
            }
            else
            {
                m_dispatcher.InvokeOnNewEntityCb(consumer, currState, timestampChangeInfo);

                if (m_postpone)
                {
                    m_postponedTypes.Postpone(consumer,
                                              currState.GetTypeId(),
                                              CallbackId::OnNewEntity);
                }
                if (m_postponeCurrent)
                {
                    dontRemove = true;
                }
            }
        }

        if (updated)
        {
            if (m_postponedTypes.IsPostponed(consumer,
                                             currState.GetTypeId(),
                                             CallbackId::OnUpdatedEntity))
            {
                dontRemove = true;
            }
            else
            {
                m_dispatcher.InvokeOnUpdatedEntityCb(consumer, currState, lastState, timestampChangeInfo);

                if (m_postpone)
                {
                    m_postponedTypes.Postpone(consumer,
                                              currState.GetTypeId(),
                                              CallbackId::OnUpdatedEntity);
                }
                if (m_postponeCurrent)
                {
                    dontRemove = true;
                }
            }
        }

        if (deleted)
        {
            if (m_postponedTypes.IsPostponed(consumer,
                                             currState.GetTypeId(),
                                             CallbackId::OnDeletedEntity))
            {
                dontRemove = true;
            }
            else
            {
                //if we're a WantsLastState subscription we may have just dispatched a create, so we need
                //to "fudge" this delete to have the correct states.
                if (created || updated)
                {
                    ENSURE(subscriptionOptions.wantsLastState, << "Unexpected combination of created, updated and deleted (" 
                           << created << ", " << updated << ", " << deleted << ") when wantsLastState is not set");
                    m_dispatcher.InvokeOnDeletedEntityCb(consumer, currState, currState, deleteIsExplicit, timestampChangeInfo);
                }
                else
                {
                    m_dispatcher.InvokeOnDeletedEntityCb(consumer, currState, lastState, deleteIsExplicit, timestampChangeInfo);
                }

                if (m_postpone)
                {
                    m_postponedTypes.Postpone(consumer,
                                              currState.GetTypeId(),
                                              CallbackId::OnDeletedEntity);
                }
                if (m_postponeCurrent)
                {
                    dontRemove = true;
                }
            }
        }

        exitDispatch = m_exitDispatch;
        if (!dontRemove)
        {
            subscription->SetLastRealState(currState);
        }

        return !dontRemove; //subscription is only clean if we're removing.
    }

    void Controller::DispatchEntitySubscription(const SubscriptionPtr& subscription,
                                                bool& exitDispatch,
                                                bool& dontRemove)
    {
        if (!subscription->IsSubscribed())
        {
            // An unsubscribe has been invoked, drop this subscription.
            return;
        }

        switch (subscription->GetSubscriptionId().subscriptionType)
        {
            case InjectionSubscription:
            {
                subscription->DirtyFlag().Process(boost::bind(&Controller::ProcessInjectionSubscription,
                                                              this,
                                                              boost::cref(subscription),
                                                              boost::ref(exitDispatch),
                                                              boost::ref(dontRemove)));
            }
            break;

            case EntitySubscription:
            {
                subscription->DirtyFlag().Process(boost::bind(&Controller::ProcessEntitySubscription,
                                                              this,
                                                              boost::cref(subscription),
                                                              boost::ref(exitDispatch),
                                                              boost::ref(dontRemove)));
            }
            break;

            default:
                ENSURE(false, << "Unexpectd subscription type!");
        }
    }

    void Controller::DispatchSubscription(const SubscriptionPtr& subscription, bool& exitDispatch, bool& dontRemove)
    {
        exitDispatch = false;
        dontRemove = false;
        lllout << "Controller::DispatchSubscription() called " << std::endl;

        switch (subscription->GetSubscriptionId().subscriptionType)
        {
            case EntityRegistrationSubscription:
            case ServiceRegistrationSubscription:
            {
                DispatchRegistrationSubscription(subscription, exitDispatch, dontRemove);
            }
            break;

            case EntitySubscription:
            case InjectionSubscription:
            {
                DispatchEntitySubscription(subscription, exitDispatch, dontRemove);
            }
            break;

            default:
                ENSURE(false, << "Unexpectd subscription type!");

        }
    }

    void Controller::DispatchSubscriptions()
    {
        m_connection->GetDirtySubscriptionQueue().Dispatch
                            (boost::bind(&Controller::DispatchSubscription, this, _1, _2, _3));
    }

    Controller::InjectionData Controller::CreateInjectionData(const SubscriptionPtr& subscription)
    {
        DistributionData dispatchedInjection = DistributionData(no_state_tag);
        DistributionData realState = subscription->GetCurrentRealState();
        DistributionData injectionState = subscription->GetCurrentInjectionState();
        const bool isGhost = !realState.IsNoState() && realState.GetEntityStateKind() == DistributionData::Ghost;

        if (!realState.IsNoState() && !injectionState.IsNoState())
        {
            // Both a real state and an injection state

            if (realState.GetHandlerId() != injectionState.GetHandlerId())
            {
                std::wostringstream ostr;
                ostr << "Handler Id of the injection doesn't match the Handler Id used in this system. typeId = "
                     << Typesystem::Operations::GetName(realState.GetTypeId())
                     << " handlerId = " << realState.GetHandlerId() << " injection handlerId = " << injectionState.GetHandlerId()
                     << " instanceId = " << realState.GetInstanceId() << ")";
                throw Safir::Dob::Typesystem::ConfigurationErrorException(ostr.str(),__WFILE__,__LINE__);
            }

            const TimestampOperations::MergeResult mergeResult = TimestampOperations::Merge(realState,
                                                                                            injectionState);

            // If a merge was not performed, then we are interested only if the real state is a ghost
            if (!mergeResult.second && !isGhost)
            {
                return std::make_pair<InjectionDispatchAction, DistributionData>(NoAction, DistributionData(no_state_tag));
            }
            dispatchedInjection = mergeResult.first;

        }
        else if (!injectionState.IsNoState())
        {
            // There is an injection state but no real state
            dispatchedInjection = injectionState.GetEntityStateCopy(true); // true means that the blob will be included.
        }
        else
        {
            // There is a real state but no injection state
            dispatchedInjection = realState.GetEntityStateCopy(true); // true means that the blob will be included.

            if (!isGhost)
            {
                // Then we are interested only if the real state is a ghost
                return std::make_pair<InjectionDispatchAction, DistributionData>(NoAction, DistributionData(no_state_tag));
            }
        }

        // At this point a merge has been performed and/or we are handling a ghost.

        InjectionDispatchAction dispatchAction = NoAction;

        if (realState.IsNoState() || !realState.HasBlob() || isGhost)
        {
            // There is no created entity ...

            if (dispatchedInjection.HasBlob())
            {
                // ... and the entity that we are about to dispatch has a blob.
                dispatchAction = NewCallback;
            }
        }
        else
        {
            // There is a created entity ...

            if (dispatchedInjection.HasBlob())
            {
                // ... and the entity that we are about to dispatch has a blob.
                dispatchAction = UpdateCallback;
            }
            else
            {
                // ... and the entity that we are about to dispatch has a no blob.
                dispatchAction = DeleteCallback;
            }
        }

        const ConnectionPtr connection = subscription->GetSubscriptionId().connectionConsumer.connection;

        // Update the header
        dispatchedInjection.SetEntityStateKind(DistributionData::Real);
        dispatchedInjection.ResetDecrementedFlag();
        if (dispatchedInjection.HasBlob())
        {
            dispatchedInjection.SetSenderId(connection->Id());
            dispatchedInjection.SetExplicitlyDeleted(false);
        }
        else
        {
            // Correct node number and context, but no connection id for delete states
            dispatchedInjection.SetSenderId(ConnectionId(ThisNodeParameters::NodeNumber(), connection->Id().m_contextId, -1));
            dispatchedInjection.SetExplicitlyDeleted(true);
        }

        return std::make_pair(dispatchAction, dispatchedInjection);
    }

    bool Controller::DispatchInjection(const InjectionData& injection, const SubscriptionPtr& subscription, bool& confirmInjection)
    {
        const InjectionDispatchAction& action = injection.first;
        const DistributionData& injectedEntity = injection.second;
        
        const ConsumerId consumer = subscription->GetSubscriptionId().connectionConsumer.consumer;
        DistributionData realState = subscription->GetCurrentRealState();
        DistributionData injectionState = subscription->GetCurrentInjectionState();

        bool dontRemove = false;

        m_postpone = false;
        m_postponeCurrent = false;
        m_incompleteInjection = false;
        m_setInjectedEntity = false;
        m_deleteInjectedEntity = false;
        m_dispatchedInjection = injection.second;
        m_originalInjectionState = injectionState;

        switch (action)
        {
        case NoAction:
            {
                confirmInjection = !injection.second.IsNoState();
                dontRemove=false; // Remove from queue and reset dirty flag
            }
            break;

        case NewCallback:
            {
                // We are going to dispatch an OnInjectedNewEntity

                if (m_postponedTypes.IsPostponed(consumer,
                                                 injectedEntity.GetTypeId(),
                                                 CallbackId::OnInjectedNewEntity))
                {
                    dontRemove = true;
                    confirmInjection = false;
                }
                else
                {
                    // Make the callback to the app
                    m_dispatcher.InvokeOnInjectedNewEntityCb(consumer, injectedEntity);

                    // Check that the app hasn't invoked multiple actions
                    CheckAppInjectionAction(m_postpone, m_incompleteInjection, m_setInjectedEntity, m_deleteInjectedEntity);

                    if (m_postpone)
                    {
                        m_postponedTypes.Postpone(consumer,
                                                  injectedEntity.GetTypeId(),
                                                  CallbackId::OnInjectedNewEntity);
                        if (m_postponeCurrent)
                        {
                            dontRemove = true;
                            confirmInjection = false;
                        }
                    }
                }
            }
            break;

        case UpdateCallback:
            {
                // We are going to dispatch an OnInjectedUpdatedEntity

                if (m_postponedTypes.IsPostponed(consumer,
                                                 injectedEntity.GetTypeId(),
                                                 CallbackId::OnInjectedUpdatedEntity))
                {
                    dontRemove = true;
                    confirmInjection = false;
                }
                else
                {
                    // Make the callback to the app
                    m_dispatcher.InvokeOnInjectedUpdatedEntityCb(consumer, injectedEntity, realState);

                    // Check that the app hasn't invoked multiple actions
                    CheckAppInjectionAction(m_postpone, m_incompleteInjection, m_setInjectedEntity, m_deleteInjectedEntity);

                    if (m_postpone)
                    {
                        m_postponedTypes.Postpone(consumer,
                                                  injectedEntity.GetTypeId(),
                                                  CallbackId::OnInjectedUpdatedEntity);
                        if (m_postponeCurrent)
                        {
                            dontRemove = true;
                            confirmInjection = false;
                        }
                    }
                }
            }
            break;

        case DeleteCallback:
            {
                // We are going to dispatch an OnInjectedDeletedEntity

                if (m_postponedTypes.IsPostponed(consumer,
                                                 injectedEntity.GetTypeId(),
                                                 CallbackId::OnInjectedDeletedEntity))
                {
                    dontRemove = true;
                    confirmInjection = false;
                }
                else
                {
                    // Make the callback to the app
                    m_dispatcher.InvokeOnInjectedDeletedEntityCb(consumer, injectedEntity, realState);

                    // Check that the app hasn't invoked multiple actions
                    CheckAppInjectionAction(m_postpone, m_incompleteInjection, m_setInjectedEntity, m_deleteInjectedEntity);

                     if (m_postpone)
                     {
                         m_postponedTypes.Postpone(consumer,
                                                   injectedEntity.GetTypeId(),
                                                   CallbackId::OnInjectedDeletedEntity);
                         if (m_postponeCurrent)
                         {
                             dontRemove = true;
                             confirmInjection = false;
                         }
                     }
                }
            }
            break;

        default:
            ENSURE(false, << "Unexpected dispatchAction!");
        }

        return dontRemove;
    }

    

    bool Controller::ProcessInjectionSubscription(const SubscriptionPtr& subscription, bool& exitDispatch, bool& dontRemove)
    {
        exitDispatch = false;
        dontRemove = false;

        DistributionData realState = subscription->GetCurrentRealState();
        DistributionData injectionState = subscription->GetCurrentInjectionState();

        // Check if this is an injection that is meant for the handler that has setup this subscription
        Dob::Typesystem::HandlerId stateHandlerId;
        Typesystem::EntityId entityId;
        if (realState.IsNoState() && injectionState.IsNoState())
        {
            // No real state and no injection state
            return true; // Remove from queue and reset dirty flag
        }
        else if (!realState.IsNoState())
        {
            entityId = realState.GetEntityId();
            stateHandlerId = realState.GetHandlerId();
        }
        else
        {
            entityId = injectionState.GetEntityId();
            stateHandlerId = injectionState.GetHandlerId();
        }

        if (stateHandlerId != Dob::Typesystem::HandlerId(subscription->GetSubscriptionId().id))
        {
            // The state is not for this handler
            return true;    // Remove from queue and reset dirty flag
        }

        bool injectionStateChanged = injectionState != subscription->GetLastInjectionState();

        const bool isGhost = !realState.IsNoState() && realState.GetEntityStateKind() == DistributionData::Ghost;

        if (isGhost)
        {
            if (subscription->GetState()->IsReleased())
            {
                // This ghost is "deleted" and should not be used.
                InitialInjectionHandled(Dob::Typesystem::HandlerId(subscription->GetSubscriptionId().id),
                                    subscription->GetSubscriptionId().connectionConsumer.consumer,
                                    entityId.GetTypeId(),
                                    entityId.GetInstanceId());
                return true; // Remove from queue and reset dirty flag
            }

            if (!subscription->GetSubscriptionId().connectionConsumer.connection->
                InitialInjectionInstanceExists(realState.GetTypeId(),
                                               realState.GetHandlerId(),
                                               realState.GetInstanceId()))
            {
                // This ghost is not expected to be dispatched.
                InitialInjectionHandled(Dob::Typesystem::HandlerId(subscription->GetSubscriptionId().id),
                                    subscription->GetSubscriptionId().connectionConsumer.consumer,
                                    entityId.GetTypeId(),
                                    entityId.GetInstanceId());
                return true; // Remove from queue and reset dirty flag
            }

            if (NodeStatuses::Instance().AnyNodeHasStatus(NodeStatus::Starting))
            {
                // We won't start to inject ghosts as long as there are one or more nodes that are
                // in state 'Starting'. This minimize the risk that we will signal OnInitialInjectionsDone
                // before all ghosts are received from remote nodes.
                dontRemove = true;
                return false;  // Don't remove from queue and don't reset dirty flag
            }
        }

        if (!injectionStateChanged && !isGhost)
        {
            // We are only interested if there is a change of the injection state or if the real state is a ghost.
            return true; // Remove from queue and reset dirty flag
        }

        bool confirmInjection = true;
        InjectionData injection =  CreateInjectionData(subscription);
        dontRemove = DispatchInjection(injection, subscription, confirmInjection);

        const InjectionDispatchAction& dispatchAction = injection.first; //Get a better name
        DistributionData& dispatchedInjection = injection.second; //Get a better name
        
        const ConsumerId consumer = subscription->GetSubscriptionId().connectionConsumer.consumer;
        const Safir::Dob::Internal::ConnectionPtr connection = subscription->GetSubscriptionId().connectionConsumer.connection;
        if (!dispatchedInjection.IsNoState())
        {
            entityId = dispatchedInjection.GetEntityId();
        }

        if (m_incompleteInjection)
        {
            confirmInjection = false;
        }
        else if (m_deleteInjectedEntity || m_setInjectedEntity)
        {
            // The app has called set and/or delete so we don't have to make a confirmation call.
            confirmInjection = false;
        }

        if (confirmInjection)
        {
            EntityTypes::Instance().AcceptInjection(connection, dispatchedInjection, injectionState);
        }

        if (
            (dispatchAction == NoAction) ||
            (dispatchAction == NewCallback && (confirmInjection || m_setInjectedEntity || m_deleteInjectedEntity))
           )
        {
            // A new instance has been accepted or rejected by the app, check if an OninitialInjectionDone
            // should be sent for this type
            InitialInjectionHandled(Dob::Typesystem::HandlerId(subscription->GetSubscriptionId().id),
                                    consumer,
                                    entityId.GetTypeId(),
                                    entityId.GetInstanceId());
        }

        exitDispatch = m_exitDispatch;

        // Release references
        m_dispatchedInjection = DistributionData(no_state_tag);
        m_originalInjectionState = DistributionData(no_state_tag);

        if (!dontRemove && !m_incompleteInjection)
        {
            // Only set last == current if subscription isn't saved for a redispatch.
            subscription->SetLastInjectionState(injectionState);
        }
        return !dontRemove;  // Reset dirty flag if removed from queue
    }

    void Controller::InitialInjectionHandled(const Dob::Typesystem::HandlerId&     handlerId,
                                             const ConsumerId&                     consumer,
                                             const Typesystem::TypeId              typeId,
                                             const Dob::Typesystem::InstanceId&    instanceId)
    {
        bool removedLastInstance = m_connection->RemoveInitialInjectionInstance(typeId,
                                                                                handlerId,
                                                                                instanceId); 
        if (removedLastInstance)
        {
             m_dispatcher.InvokeOnInitialInjectionsDoneCb(consumer, typeId, handlerId);
        }
    }

    void Controller::DispatchEmptyInitialInjections()
    {
        std::vector<TypeHandlerPair> emptyInitialInjections = m_connection->GetAndClearEmptyInitialInjections();

        if (emptyInitialInjections.empty())
        {
            return;
        }

        // We are looping from front to back, so there will be a copy of the remaining items for each
        // erase() call. Since this vector will be empty in the majority of calls we don't bother using
        // a more sophisticated algorithm.
        for (std::vector<TypeHandlerPair>::iterator it = emptyInitialInjections.begin();
             it != emptyInitialInjections.end();) // iterator incrementation is done below
        {
            const Typesystem::TypeId& typeId = it->first;
            const Typesystem::HandlerId& handlerId = it->second;

            const ConsumerId consumer = m_connection->GetInjectionHandlerConsumer(typeId, handlerId);

            if (consumer != ConsumerId())
            {
                m_dispatcher.InvokeOnInitialInjectionsDoneCb(consumer, typeId, handlerId);
            }

            it = emptyInitialInjections.erase(it);

            if (m_exitDispatch)
            {
                break;
            }
        }

        // Put back any non-dispatched injection
        for (std::vector<TypeHandlerPair>::iterator it = emptyInitialInjections.begin();
             it != emptyInitialInjections.end();
             ++it)
        {
            const Typesystem::TypeId& typeId = it->first;
            const Typesystem::HandlerId& handlerId = it->second;

            m_connection->AddEmptyInitialInjection(typeId,
                                                   handlerId);
        }
    }

    void Controller::CheckAppInjectionAction(const bool postpone,
                                             const bool incompleteInjectionState,
                                             const bool setInjectedEntity,
                                             const bool deleteInjectedEntity) const
    {
        int nbrOfActions = 0;
        if (postpone) ++nbrOfActions;
        if (incompleteInjectionState) ++nbrOfActions;
        if (setInjectedEntity || deleteInjectedEntity) ++nbrOfActions;

        if (nbrOfActions > 1)
        {
            std::wostringstream ostr;

            ostr << "The application is not allowed to call ";
            if (postpone)
            {
                ostr << " Postpone";
            }
            if (incompleteInjectionState)
            {
                ostr << " IncompleteInjectionState";
            }
            if (setInjectedEntity)
            {
                ostr << " SetEntity";
            }
            if (deleteInjectedEntity)
            {
                ostr << " DeleteEntity";
            }
            ostr << " in a single injection callback.";
            throw Typesystem::SoftwareViolationException(ostr.str(),__WFILE__,__LINE__);
        }
    }

    void Controller::SendRequest(const DistributionData & request,
                                 const ConsumerId & consumer)
    {
        // Must add the consumer before pushing the request on the queue, so we
        // can be sure that it is there no matter how things are dispatched.
        m_dispatcher.AddResponseConsumer(request.GetRequestId().GetCounter(), consumer);

        const bool success = m_connection->GetRequestOutQueue().PushRequest(request);

        if (success)
        {
            m_connection->SignalOut();
        }
        else
        {
            m_dispatcher.RemoveResponseConsumer(request.GetRequestId().GetCounter());

            m_requestQueueInOverflowState = true;
            m_dispatcher.AddOverflowedRequestConsumer(consumer);

            lllout << "Throwing overflow exception due to request overflow" << std::endl;
            throw Safir::Dob::OverflowException(L"Overflow when sending request",__WFILE__,__LINE__);
        }
    }

    void Controller::HandleRevokedRegistrations()
    {
        RegistrationVector revoked = m_connection->GetAndClearRevokedRegistrations();

        for (RegistrationVector::iterator it = revoked.begin(); it != revoked.end(); ++it)
        {
            long nbrOfRef = 0;
            long nbrOfRefAfter = 0;

            if (g_garbageCollected[it->consumer.lang])
            {
                // Since the user can make dob calls that adds references in the OnRevokedRegistration
                // callback, we must read the number of references before the callback and then remove
                // exactly that number of references after the callback.
                nbrOfRef = m_consumerReferences.GetHandlerRegistrationReferenceCounter(it->typeId,
                                                                                       it->handlerId,
                                                                                       it->consumer);
            }

            m_dispatcher.InvokeOnRevokedRegistrationCb(it->consumer,
                                                       it->typeId,
                                                       it->handlerId);

            // Since the user can make dob calls that removes references in the OnRevokedRegistration
            // callback, we must read the number of references after the callback and then remove
            // exactly that number of references that should be removed.
            nbrOfRefAfter = m_consumerReferences.GetHandlerRegistrationReferenceCounter(it->typeId,
                                                                                        it->handlerId,
                                                                                        it->consumer);

            if (nbrOfRefAfter < nbrOfRef)
            {
                nbrOfRef = nbrOfRefAfter;
            }

           // For garbage collected languages the references related to the revoked registration must be dropped.
            if (nbrOfRef > 0)
            {
                m_consumerReferences.DropHandlerRegistrationReferences(it->typeId,
                                                                       it->handlerId,
                                                                       it->consumer,
                                                                       nbrOfRef,
                                                                       boost::bind(&Dispatcher::InvokeDropReferenceCb,
                                                                                   m_dispatcher,
                                                                                   _1,
                                                                                   _2));
            }
        }
    }

    void Controller::HandlePendingRegistrations()
    {
        PendingRegistrationVector prv;
        bool needKick = false;

        ServiceTypes::Instance().RegisterAcceptedPendingRegistrations(m_connection, prv, needKick);
        EntityTypes::Instance().RegisterAcceptedPendingRegistrations(m_connection, prv, needKick);

        for (PendingRegistrationVector::iterator it = prv.begin();
             it != prv.end(); ++it)
        {
            lllout << "Got an accept for pending registration "<< it->id << std::endl;
            m_dispatcher.InvokeOnCompletedRegistrationCb(it->consumer,it->typeId,it->handlerId.GetHandlerId());
        }
        if (needKick)
        {
            m_connection->SignalOut();
        }
    }

    Typesystem::Int32 Controller::EntityIteratorCreate(const Typesystem::TypeId typeId,
                                                       const bool includeSubclasses,
                                                       bool& end)
    {
        ENSURE(m_entityIterators.size() < RAND_MAX, << "It seems that the application isn't releasing its Entity Iterators");

        //Generate a unique iterator id
        Typesystem::Int32 newIteratorId = rand();
        while (m_entityIterators.find(newIteratorId) != m_entityIterators.end())
        {
            newIteratorId = rand();
        }

        m_entityIterators.insert(std::make_pair(newIteratorId, EntityTypes::Instance().
            CreateEntityIterator(typeId, m_connection->Id().m_contextId, includeSubclasses, end)));

        return newIteratorId;
    }

    void Controller::EntityIteratorDestroy(const Typesystem::Int32 iteratorId)
    {
        m_entityIterators.erase(iteratorId);
    }

    Typesystem::Int32 Controller::EntityIteratorCopy(const Safir::Dob::Typesystem::Int32 iteratorId)
    {
        //Generate a unique iterator id
        Typesystem::Int32 newIteratorId = rand();
        while (m_entityIterators.find(newIteratorId) != m_entityIterators.end())
        {
            newIteratorId = rand();
        }

        EntityIteratorTable::iterator otherIterator = m_entityIterators.find(iteratorId);
        if (otherIterator == m_entityIterators.end())
        {
            throw Typesystem::SoftwareViolationException(L"Cannot copy that iterator!",__WFILE__,__LINE__);
        }

        m_entityIterators.insert(std::make_pair(newIteratorId,otherIterator->second));
        return newIteratorId;
    }

    void Controller::EntityIteratorIncrement(const Safir::Dob::Typesystem::Int32 iteratorId,
                                             bool& end)
    {
        EntityIteratorTable::iterator findIt = m_entityIterators.find(iteratorId);
        ENSURE(findIt != m_entityIterators.end(), << "Failed to find iterator with id " << iteratorId);
        end = !EntityTypes::Instance().IncrementIterator(findIt->second);
    }

    void Controller::EntityIteratorDereference(const Safir::Dob::Typesystem::Int32 iteratorId,
                                               const char *& entityBlob,
                                               const char *& entityState)
    {
        EntityIteratorTable::iterator findIt = m_entityIterators.find(iteratorId);
        ENSURE(findIt != m_entityIterators.end(), << "Failed to find iterator with id " << iteratorId);
        findIt->second.Dereference(entityBlob,entityState);
    }

    bool Controller::EntityIteratorEqual(const Safir::Dob::Typesystem::Int32 first,
                                         const Safir::Dob::Typesystem::Int32 second)
    {
        EntityIteratorTable::iterator firstIt = m_entityIterators.find(first);
        ENSURE(firstIt != m_entityIterators.end(), << "Failed to find iterator with id " << first);
        EntityIteratorTable::iterator secondIt = m_entityIterators.find(second);
        ENSURE(secondIt != m_entityIterators.end(), << "Failed to find iterator with id " << second);
        return firstIt->second == secondIt->second;
    }

    ContextId Controller::GetContext() const
    {
        return m_contextId; 
    }

    void Controller::SimulateOverflows(const bool inQueues, const bool outQueues)
    {
        m_connection->ForEachMessageInQueue(boost::bind(&MessageQueue::SimulateFull,_2,inQueues));
        m_connection->ForEachRequestInQueue(boost::bind(&RequestInQueue::SimulateFull,_2,inQueues));

        m_connection->GetMessageOutQueue().SimulateFull(outQueues);
        m_connection->GetRequestOutQueue().SimulateFull(outQueues);

        //Kick ourselves so that we dispatch the correct Overflow calls, if needed.
        m_connection->SignalIn();

        //Kick dose_main so that we stop blocking if we had an overflow-in.
        m_connection->SignalOut();
    }

}
}
}
