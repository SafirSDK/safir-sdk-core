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

#include <Safir/Dob/Typesystem/Internal/BlobOperations.h>
#include <Safir/Dob/Typesystem/Internal/InternalOperations.h>
#include <Safir/Dob/Typesystem/Internal/InternalUtils.h>
#include <Safir/Dob/Typesystem/LibraryExceptions.h>
#include <Safir/Dob/Typesystem/Utilities.h>
#include <Safir/Dob/Typesystem/Serialization.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/SystemLog.h>
#include <Safir/Dob/Internal/SubscriptionOptions.h>
#include <Safir/Dob/Internal/Connections.h>
#include <Safir/Dob/Internal/InjectionKindTable.h>
#include <Safir/Dob/Internal/TimestampOperations.h>
#include <Safir/Dob/ConnectionInfo.h>
#include <boost/current_function.hpp>

#include "dose_controller.h"
#include "dose_controller_table.h"
using namespace Safir::Dob::Internal;
using namespace Safir::Dob;

//-------------------------------------------------------------
// class Dose
//-------------------------------------------------------------
void DoseC_Constructor(DotsC_Int32 & ctrl,
                       bool & success)
{
    lllog(9) << "Entering " << BOOST_CURRENT_FUNCTION << std::endl;
    success = false;
    try
    {
        Safir::Dob::Internal::ControllerPtr controller(new Safir::Dob::Internal::Controller());
        ctrl = ControllerTable::Instance().AddController(controller);
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void DoseC_Destructor(const DotsC_Int32 ctrl)
{
    lllog(9) << "Entering " << BOOST_CURRENT_FUNCTION << std::endl;
    try
    {
        ControllerTable::Instance().RemoveController(ctrl);
    }
    catch (const std::exception & exc)
    {
        lllout << "DoseC_Destructor: Caught exception: " << exc.what() << std::endl;
        lllout << "Will return as if nothing has happened" << std::endl;
    }
    catch (...)
    {
        lllout << "DoseC_Destructor: Caught ... exception: " << std::endl;
        lllout << "Will return as if nothing has happened" << std::endl;
    }
}


void DoseC_IsConnected(const DotsC_Int32 ctrl, bool & isConnected, bool & success)
{
    lllog(9) << "Entering " << BOOST_CURRENT_FUNCTION << std::endl;
    success = false;
    try
    {
        ControllerPtr controller = ControllerTable::Instance().GetController(ctrl);
        isConnected = controller != NULL && controller->IsConnected();
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void DoseC_Connect(const DotsC_Int32 ctrl,
                   const char* connectionNameCommonPart,
                   const char* connectionNameInstancePart,
                   const DotsC_Int32 contextId,
                   const DotsC_Int32 lang,
                   void* const connectionOwner,
                   void* const dispatcher,
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
                   OnDropReferenceCb* onDropReferenceCb,
                   bool & success)
{
    lllog(9) << "Entering " << BOOST_CURRENT_FUNCTION << std::endl;
    success = false;
    try
    {
        //AWI:todo Here we should check that a program doesn't call open with a common part and an instance part
        //      that it has already used for another connection in any context (software violation). This means that a
        //      secondary attach using common part and an instance part identifies a unique connection in a certain context.

        ControllerPtr controller = ControllerTable::Instance().GetController(ctrl);

        ControllerTable::Instance().SetThread(ctrl);

        controller->Connect(connectionNameCommonPart,
                            connectionNameInstancePart,
                            contextId,
                            lang,
                            ConsumerId(connectionOwner, lang),
                            ConsumerId(dispatcher, lang),
                            onDispatchCb,
                            onStopOrderCb,
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
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void DoseC_ConnectSecondary(const char* connectionNameCommonPart,
                            const char* connectionNameInstancePart,
                            const DotsC_Int32 lang,
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
                            OnDropReferenceCb* onDropReferenceCb,
                            DotsC_Int32 & newCtrlId,
                            bool & success)
{
    lllog(9) << "Entering " << BOOST_CURRENT_FUNCTION << std::endl;
    success = false;
    try
    {
        DotsC_Int32 ctrl=-1;

        //Is it an attach-without-arguments?
        if (strlen(connectionNameCommonPart) == 0 && strlen(connectionNameInstancePart) == 0)
        {
            ctrl = ControllerTable::Instance().GetFirstControllerInThread();
        }
        else
        {
            ctrl = ControllerTable::Instance().GetNamedController(connectionNameCommonPart,connectionNameInstancePart);
        }

        ControllerPtr controller = ControllerTable::Instance().GetController(ctrl);
        ENSURE (controller != NULL, << "Strange error in getting thread controllers!!!");

        controller->ConnectSecondary(lang,
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
        newCtrlId = ctrl;
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}


void DoseC_Disconnect(const DotsC_Int32 ctrl, bool & success)
{
    lllog(9) << "Entering " << BOOST_CURRENT_FUNCTION << std::endl;
    success = false;
    try
    {
        ControllerPtr controller = ControllerTable::Instance().GetController(ctrl);
        ControllerTable::Instance().UnsetThread(ctrl);

        if (controller->IsConnected())
        {
            controller->Disconnect();
        }
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void DoseC_GetConnectionName(const DotsC_Int32 ctrl, const char* &name, bool & success)
{
    lllog(9) << "Entering " << BOOST_CURRENT_FUNCTION << std::endl;
    success = false;
    try
    {
        name = ControllerTable::Instance().GetController(ctrl)->GetConnectionName();
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void DoseC_GetConnectionNameCommonPart(const DotsC_Int32 ctrl, const char* &name, bool & success)
{
    lllog(9) << "Entering " << BOOST_CURRENT_FUNCTION << std::endl;
    success = false;
    try
    {
        name = ControllerTable::Instance().GetController(ctrl)->
            GetConnectionNameCommonPart();
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void DoseC_GetConnectionNameInstancePart(const DotsC_Int32 ctrl, const char* &name, bool & success)
{
    lllog(9) << "Entering " << BOOST_CURRENT_FUNCTION << std::endl;
    success = false;
    try
    {
        name = ControllerTable::Instance().GetController(ctrl)->
            GetConnectionNameInstancePart();
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void DoseC_RegisterServiceHandler(const DotsC_Int32 ctrl,
                                  const Safir::Dob::Typesystem::TypeId typeId,
                                  const Safir::Dob::Typesystem::Int64 handlerId,
                                  const char* const handlerIdStr,
                                  const bool overrideRegistration,
                                  const DotsC_Int32 lang,
                                  void* const consumer,
                                  bool& success)
{
    lllog(9) << "Entering " << BOOST_CURRENT_FUNCTION << std::endl;
    success = false;
    try
    {
        ControllerTable::Instance().GetController(ctrl)->
            RegisterServiceHandler(typeId,
                                   Typesystem::HandlerId(handlerId,
                                   Typesystem::Utilities::ToWstring(handlerIdStr)),
                                   overrideRegistration,
                                   ConsumerId(consumer,lang));

        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void DoseC_RegisterEntityHandler(const DotsC_Int32 ctrl,
                                 const Safir::Dob::Typesystem::TypeId typeId,
                                 const Safir::Dob::Typesystem::Int64 handlerId,
                                 const char* const handlerIdStr,
                                 const Safir::Dob::Typesystem::EnumerationValue instanceIdPolicy,
                                 const bool overrideRegistration,
                                 const bool injectionHandler,
                                 const DotsC_Int32 lang,
                                 void* const consumer,
                                 bool& success)
{
    lllog(9) << "Entering " << BOOST_CURRENT_FUNCTION << std::endl;
    success = false;
    try
    {
        ControllerTable::Instance().GetController(ctrl)->
            RegisterEntityHandler(typeId,
                                  Typesystem::HandlerId(handlerId,
                                                        Typesystem::Utilities::ToWstring(handlerIdStr)),
                                  static_cast<Safir::Dob::InstanceIdPolicy::Enumeration>(instanceIdPolicy),
                                  overrideRegistration,
                                  injectionHandler,
                                  ConsumerId(consumer,lang));

        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}


void DoseC_UnregisterHandler(const DotsC_Int32 ctrl,
                             const Safir::Dob::Typesystem::TypeId typeId,
                             const Safir::Dob::Typesystem::Int64 handlerId,
                             const char* const handlerIdStr,
                             bool& success)
{
    lllog(9) << "Entering " << BOOST_CURRENT_FUNCTION << std::endl;
    success = false;
    try
    {
        ControllerTable::Instance().GetController(ctrl)->
            UnregisterHandler(typeId,
                              Typesystem::HandlerId(handlerId,
                                                    Typesystem::Utilities::ToWstring(handlerIdStr)));
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}


void DoseC_SubscribeMessage(const DotsC_Int32 ctrl,
                            const Safir::Dob::Typesystem::TypeId typeId,
                            const Safir::Dob::Typesystem::Int64 channelId,
                            const char * const channelIdStr,
                            const bool includeSubclasses,
                            const DotsC_Int32 lang,
                            void* const consumer,
                            bool & success)
{
    lllog(9) << "Entering " << BOOST_CURRENT_FUNCTION << std::endl;
    success = false;
    try
    {
        ControllerTable::Instance().GetController(ctrl)->
            SubscribeMessage(typeId,
                             Typesystem::ChannelId(channelId,
                                                   Typesystem::Utilities::ToWstring(channelIdStr)),
                             includeSubclasses,
                             ConsumerId(consumer,lang));
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void DoseC_UnsubscribeMessage(const DotsC_Int32 ctrl,
                              const Safir::Dob::Typesystem::TypeId typeId,
                              const Safir::Dob::Typesystem::Int64 channelId,
                              const char * const channelIdStr,
                              const bool includeSubclasses,
                              const DotsC_Int32 lang,
                              void* const consumer,
                              bool & success)
{
    lllog(9) << "Entering " << BOOST_CURRENT_FUNCTION << std::endl;
    success = false;
    try
    {
        ControllerTable::Instance().GetController(ctrl)->
            UnsubscribeMessage(typeId,
                               Typesystem::ChannelId(channelId,
                                                     Typesystem::Utilities::ToWstring(channelIdStr)),
                               includeSubclasses,
                               ConsumerId(consumer, lang));
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void DoseC_SubscribeEntity(const DotsC_Int32 ctrl,
                           const Safir::Dob::Typesystem::TypeId typeId,
                           const Safir::Dob::Typesystem::Int64 instanceId,
                           const char* const instanceIdStr,
                           const bool allInstances,
                           const bool includeUpdates,
                           const bool includeSubclasses,
                           const bool restartSubscription,
                           const DotsC_Int32 lang,
                           void* const consumer,
                           bool& success)
{
    lllog(9) << "Entering " << BOOST_CURRENT_FUNCTION << std::endl;
    success = false;
    try
    {
        ControllerTable::Instance().GetController(ctrl)->SubscribeEntity
            (Typesystem::EntityId(typeId,Typesystem::InstanceId
                                           (instanceId,Typesystem::Utilities::ToWstring(instanceIdStr))),
             allInstances,
             includeSubclasses,
             restartSubscription,
             SubscriptionOptions::EntitySubscriptionOptions::Create(includeUpdates,
                                                                    false,              // wantsGhostDelete
                                                                    false,              // wantsLastState
                                                                    false,              // doesntWantSourceIsPermanentStore
                                                                    false,              // wantsAllStateChanges
                                                                    false),             // timestampChangeInfo
             ConsumerId(consumer,lang));
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void DoseC_InjectorSubscribeEntity(const DotsC_Int32 ctrl,
                                   const Safir::Dob::Typesystem::TypeId typeId,
                                   const bool includeUpdates,
                                   const bool includeSubclasses,
                                   const bool restartSubscription,
                                   const bool wantsGhostDelete,
                                   const bool wantsLastState,
                                   const bool doesntWantSourceIsPermanentStore,
                                   const bool wantsAllStateChanges,
                                   const bool timestampChangeInfo,
                                   const DotsC_Int32 lang,
                                   void* const consumer,
                                   bool& success)
{
    lllog(9) << "Entering " << BOOST_CURRENT_FUNCTION << std::endl;
    success = false;
    try
    {
        ControllerTable::Instance().GetController(ctrl)->
            SubscribeEntity(Typesystem::EntityId(typeId,Typesystem::InstanceId(0)),
                            true, //allInstances,
                            includeSubclasses,
                            restartSubscription,
                            SubscriptionOptions::EntitySubscriptionOptions::Create(includeUpdates,
                                                                                   wantsGhostDelete,
                                                                                   wantsLastState,
                                                                                   doesntWantSourceIsPermanentStore,
                                                                                   wantsAllStateChanges,
                                                                                   timestampChangeInfo),
                            ConsumerId(consumer,lang));
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void DoseC_UnsubscribeEntity(const DotsC_Int32 ctrl,
                             const Safir::Dob::Typesystem::TypeId typeId,
                             const Safir::Dob::Typesystem::Int64 instanceId,
                             const char* const instanceIdStr,
                             const bool allInstances,
                             const bool includeSubclasses,
                             const DotsC_Int32 lang,
                             void* const consumer,
                             bool& success)
{
    lllog(9) << "Entering " << BOOST_CURRENT_FUNCTION << std::endl;
    success = false;
    try
    {
        ControllerTable::Instance().GetController(ctrl)->UnsubscribeEntity
            (Typesystem::EntityId(typeId,Typesystem::InstanceId
                                           (instanceId,Typesystem::Utilities::ToWstring(instanceIdStr))),
             allInstances,
             includeSubclasses,
             ConsumerId(consumer,lang));
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void DoseC_SubscribeRegistration(const DotsC_Int32 ctrl,
                                 const Safir::Dob::Typesystem::TypeId typeId,
                                 const Safir::Dob::Typesystem::Int64 handlerId,
                                 const char* const handlerIdStr,
                                 const bool includeSubclasses,
                                 const bool restartSubscription,
                                 const DotsC_Int32 lang,
                                 void* const consumer,
                                 bool & success)
{
    lllog(9) << "Entering " << BOOST_CURRENT_FUNCTION << std::endl;
    success = false;
    try
    {
        ControllerTable::Instance().GetController(ctrl)->
            SubscribeRegistration(typeId,
                                  Typesystem::HandlerId(handlerId,
                                                        Typesystem::Utilities::ToWstring(handlerIdStr)),
                                  includeSubclasses,
                                  restartSubscription,
                                  SubscriptionOptionsPtr(NULL),
                                  ConsumerId(consumer, lang));
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void DoseC_UnsubscribeRegistration(const DotsC_Int32 ctrl,
                                   const Safir::Dob::Typesystem::TypeId typeId,
                                   const Safir::Dob::Typesystem::Int64 handlerId,
                                   const char* const handlerIdStr,
                                   const bool includeSubclasses,
                                   const DotsC_Int32 lang,
                                   void* const consumer,
                                   bool & success)
{
    lllog(9) << "Entering " << BOOST_CURRENT_FUNCTION << std::endl;
    success = false;
    try
    {
        ControllerTable::Instance().GetController(ctrl)->
            UnsubscribeRegistration(typeId,
                                   Typesystem::HandlerId(handlerId,
                                                         Typesystem::Utilities::ToWstring(handlerIdStr)),
                                   includeSubclasses,
                                   ConsumerId(consumer, lang));
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

//---------------------------------
// Dispatch methods
//---------------------------------

void DoseC_Dispatch(const DotsC_Int32 ctrl,
                    bool & success)
{
    lllog(9) << "Entering " << BOOST_CURRENT_FUNCTION << std::endl;
    success = false;
    try
    {
        const ControllerPtr controller = ControllerTable::Instance().GetController(ctrl);
        if (controller->IsConnected())
        {
                controller->Dispatch();
        }

        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void DoseC_ExitDispatch(const DotsC_Int32 ctrl,
                        bool & success)
{
    lllog(9) << "Entering " << BOOST_CURRENT_FUNCTION << std::endl;
    success = false;
    try
    {
        ControllerTable::Instance().GetController(ctrl)->ExitDispatch();
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void DoseC_GetCurrentCallbackId(const DotsC_Int32 ctrl,
                                DotsC_Int32& callbackId,
                                bool& success)
{
    lllog(9) << "Entering " << BOOST_CURRENT_FUNCTION << std::endl;
    success = false;
    try
    {
        callbackId = ControllerTable::Instance().GetController(ctrl)->CurrentCallback();
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}


//--------------------------------
// Message methods
//--------------------------------
// Send Message
void DoseC_SendMessage(const DotsC_Int32 ctrl,
                       const char * const message,
                       const Safir::Dob::Typesystem::Int64 channelId,
                       const char * const channelIdStr,
                       const DotsC_Int32 lang,
                       void* const consumer,
                       bool & success)
{
    lllog(9) << "Entering " << BOOST_CURRENT_FUNCTION << std::endl;
    success = false;
    try
    {
        ControllerTable::Instance().GetController(ctrl)->
            SendMessage(message,
                        Typesystem::ChannelId(channelId,
                                              Typesystem::Utilities::ToWstring(channelIdStr)),
                        ConsumerId(consumer, lang));
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

//--------------------------------
// Response methods
//--------------------------------
void DoseC_SendResponse(const DotsC_Int32 ctrl,
                        const char * const blob,
                        void * const consumer,
                        const DotsC_Int32 lang,
                        const Safir::Dob::Typesystem::Int32 responseId,
                        bool& success)
{
    lllog(9) << "Entering " << BOOST_CURRENT_FUNCTION << std::endl;
    success = false;
    try
    {
        ControllerTable::Instance().GetController(ctrl)->
            SendResponse(blob, ConsumerId(consumer,lang), ResponseId(responseId));
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

//----------------------------
// Request methods
//----------------------------
// Send Request
void DoseC_ServiceRequest(const DotsC_Int32 ctrl,
                          const char* const request,
                          const Safir::Dob::Typesystem::Int64 handlerId,
                          const char* const handlerIdStr,
                          const DotsC_Int32 lang,
                          void* const consumer,
                          Safir::Dob::RequestId& reqId,
                          bool& success)
{
    lllog(9) << "Entering " << BOOST_CURRENT_FUNCTION << std::endl;
    success = false;
    try
    {
        ControllerTable::Instance().GetController(ctrl)->
            ServiceRequest(request,
                           Typesystem::HandlerId(handlerId,Typesystem::Utilities::ToWstring(handlerIdStr)),
                           ConsumerId(consumer,lang),
                           reqId);
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}



void DoseC_CreateRequest(const DotsC_Int32 ctrl,
                         const char* const request,
                         const bool hasInstanceId,
                         const Safir::Dob::Typesystem::Int64 instanceId,
                         const char* const instanceIdStr,
                         const Safir::Dob::Typesystem::Int64 handlerId,
                         const char* const handlerIdStr,
                         const DotsC_Int32 lang,
                         void* const consumer,
                         Safir::Dob::RequestId& reqId,
                         bool& success)
{
    lllog(9) << "Entering " << BOOST_CURRENT_FUNCTION << std::endl;
    success = false;
    try
    {
        ControllerTable::Instance().GetController(ctrl)->
            CreateRequest(request,
                          hasInstanceId,
                          Typesystem::InstanceId(instanceId,Typesystem::Utilities::ToWstring(instanceIdStr)),
                          Typesystem::HandlerId(handlerId,Typesystem::Utilities::ToWstring(handlerIdStr)),
                          ConsumerId(consumer,lang),
                          reqId);
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

// Send Entity Request update
void DoseC_UpdateRequest(const DotsC_Int32 ctrl,
                         const char* const request,
                         const Safir::Dob::Typesystem::Int64 instanceId,
                         const char* const instanceIdStr,
                         const DotsC_Int32 lang,
                         void* const consumer,
                         Safir::Dob::RequestId& reqId,
                         bool& success)
{
    lllog(9) << "Entering " << BOOST_CURRENT_FUNCTION << std::endl;
    success = false;
    try
    {
        ControllerTable::Instance().GetController(ctrl)->
            UpdateRequest(request,
                          Typesystem::InstanceId(instanceId,Typesystem::Utilities::ToWstring(instanceIdStr)),
                          ConsumerId(consumer,lang),
                          reqId);
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

// Send Entity Request delete
void DoseC_DeleteRequest(const DotsC_Int32 ctrl,
                         const Safir::Dob::Typesystem::TypeId typeId,
                         const Safir::Dob::Typesystem::Int64 instanceId,
                         const char* const instanceIdStr,
                         const DotsC_Int32 lang,
                         void* const consumer,
                         Safir::Dob::RequestId& reqId,
                         bool& success)
{
    lllog(9) << "Entering " << BOOST_CURRENT_FUNCTION << std::endl;
    success = false;
    try
    {
        ControllerTable::Instance().GetController(ctrl)->
            DeleteRequest(Typesystem::EntityId(typeId,Typesystem::InstanceId(instanceId,Typesystem::Utilities::ToWstring(instanceIdStr))),
                          ConsumerId(consumer,lang),
                          reqId);
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}



//----------------------------
// Entity methods
//----------------------------
void DoseC_SetEntity(const DotsC_Int32 ctrl,
                     const char* const entity,
                     const Safir::Dob::Typesystem::Int64 instanceId,
                     const char* const instanceIdStr,
                     const Safir::Dob::Typesystem::Int64 handlerId,
                     const char* const handlerIdStr,
                     const bool considerChangeFlags,
                     const bool initialInjection,
                     bool& success)
{
    lllog(9) << "Entering " << BOOST_CURRENT_FUNCTION << std::endl;
    success = false;
    try
    {
        ControllerTable::Instance().GetController(ctrl)->
            SetEntity(entity,
                      Typesystem::InstanceId(instanceId,Typesystem::Utilities::ToWstring(instanceIdStr)),
                      Typesystem::HandlerId(handlerId,Typesystem::Utilities::ToWstring(handlerIdStr)),
                      considerChangeFlags,
                      initialInjection);
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void DoseC_DeleteEntity(const DotsC_Int32 ctrl,
                        const Safir::Dob::Typesystem::TypeId typeId,
                        const Safir::Dob::Typesystem::Int64 instanceId,
                        const char* const instanceIdStr,
                        const bool allInstances,
                        const Safir::Dob::Typesystem::Int64 handlerId,
                        const char* const handlerIdStr,
                        bool& success)
{
    lllog(9) << "Entering " << BOOST_CURRENT_FUNCTION << std::endl;
    success = false;
    try
    {
        ControllerTable::Instance().GetController(ctrl)->
            DeleteEntity(Typesystem::EntityId(typeId,
                                              Typesystem::InstanceId(instanceId, Typesystem::Utilities::ToWstring(instanceIdStr))),
                         allInstances,
                         Typesystem::HandlerId(handlerId,Typesystem::Utilities::ToWstring(handlerIdStr)));
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void DoseC_InjectEntity(const DotsC_Int32 ctrl,
                        const char* const entity,
                        const Safir::Dob::Typesystem::Int64 instanceId,
                        const char* const instanceIdStr,
                        const Safir::Dob::Typesystem::Int64 handlerId,
                        const char* const handlerIdStr,
                        const Safir::Dob::Typesystem::Int64 timestamp,
                        bool& success)
{
    lllog(9) << "Entering " << BOOST_CURRENT_FUNCTION << std::endl;
    success = false;
    try
    {
        ControllerTable::Instance().GetController(ctrl)->
            InjectEntity(entity,
                         Typesystem::InstanceId(instanceId,Typesystem::Utilities::ToWstring(instanceIdStr)),
                         Typesystem::HandlerId(handlerId,Typesystem::Utilities::ToWstring(handlerIdStr)),
                         timestamp);
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void DoseC_InjectDeletedEntity(const DotsC_Int32 ctrl,
                               const Safir::Dob::Typesystem::TypeId typeId,
                               const Safir::Dob::Typesystem::Int64 instanceId,
                               const char* const instanceIdStr,
                               const Safir::Dob::Typesystem::Int64 handlerId,
                               const char* const handlerIdStr,
                               const Safir::Dob::Typesystem::Int64 timestamp,
                               bool& success)
{
    lllog(9) << "Entering " << BOOST_CURRENT_FUNCTION << std::endl;
    success = false;
    try
    {
        ControllerTable::Instance().GetController(ctrl)->
            InjectDeletedEntity(Typesystem::EntityId(typeId,
                                                     Typesystem::InstanceId(instanceId, Typesystem::Utilities::ToWstring(instanceIdStr))),
                                Typesystem::HandlerId(handlerId,Typesystem::Utilities::ToWstring(handlerIdStr)),
                                timestamp);
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}


void DoseC_ReadEntity(const DotsC_Int32 ctrl,
                      const Safir::Dob::Typesystem::TypeId typeId,
                      const Safir::Dob::Typesystem::Int64 instanceId,
                      const char* const instanceIdStr,
                      const char*& currentBlob,
                      const char*& currentState,
                      bool& success)
{
    lllog(9) << "Entering " << BOOST_CURRENT_FUNCTION << std::endl;
    success = false;
    try
    {
        ControllerTable::Instance().GetController(ctrl)->
            ReadEntity(Typesystem::EntityId(typeId,
                                            Typesystem::InstanceId(instanceId, Typesystem::Utilities::ToWstring(instanceIdStr))),
                       currentBlob,
                       currentState);

        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void DoseC_IsCreated(const DotsC_Int32 ctrl,
                     const Safir::Dob::Typesystem::TypeId typeId,
                     const Safir::Dob::Typesystem::Int64 instanceId,
                     const char* const instanceIdStr,
                     bool& isCreated,
                     bool& success)
{
    lllog(9) << "Entering " << BOOST_CURRENT_FUNCTION << std::endl;
    success = false;
    try
    {
        isCreated = ControllerTable::Instance().GetController(ctrl)->
            IsCreated(Typesystem::EntityId(typeId,
                                           Typesystem::InstanceId(instanceId, Typesystem::Utilities::ToWstring(instanceIdStr))));
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void DoseC_GetNumberOfInstances(const DotsC_Int32 ctrl,
                                const Safir::Dob::Typesystem::TypeId typeId,
                                const Safir::Dob::Typesystem::Int64 handlerId,
                                const char* const /*handlerIdStr*/,
                                const bool includeSubclasses,
                                Safir::Dob::Typesystem::Int64& numberOfInstances,
                                bool& success)
{
    lllog(9) << "Entering " << BOOST_CURRENT_FUNCTION << std::endl;
    success = false;
    try
    {
        const Typesystem::HandlerId handler(handlerId);
        numberOfInstances = 0;

        const ControllerPtr controller = ControllerTable::Instance().GetController(ctrl);

        bool end;
        const Safir::Dob::Typesystem::Int32 iteratorId =
            controller->EntityIteratorCreate(typeId, includeSubclasses, end);

        while (!end)
        {
            if (handler == Typesystem::HandlerId::ALL_HANDLERS)
            {
                ++numberOfInstances;
            }
            else
            {
                //if we're only counting certain handlers we need to dereference and look at the state
                const char* entityBlob;
                const char* entityState;

                controller->EntityIteratorDereference(iteratorId, entityBlob, entityState);

                const DistributionData data = DistributionData::ConstConstructor(new_data_tag_t(), entityState);
                if (data.GetHandlerId() == handler)
                {
                    ++numberOfInstances;
                }
            }
            controller->EntityIteratorIncrement(iteratorId, end);
        }
        controller->EntityIteratorDestroy(iteratorId);

        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void DoseC_GetInstanceIdPolicy(const DotsC_Int32 ctrl,
                               const Safir::Dob::Typesystem::TypeId typeId,
                               const Safir::Dob::Typesystem::Int64 handlerId,
                               Safir::Dob::Typesystem::EnumerationValue& instanceIdPolicy,
                               bool& success)
{
    lllog(9) << "Entering " << BOOST_CURRENT_FUNCTION << std::endl;
    success = false;
    try
    {

        const ControllerPtr controller = ControllerTable::Instance().GetController(ctrl);

        instanceIdPolicy = static_cast<Safir::Dob::Typesystem::EnumerationValue>(controller->GetInstanceIdPolicy(typeId, Typesystem::HandlerId(handlerId)));
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void DoseC_Postpone(const DotsC_Int32 ctrl,
                    const bool redispatchCurrent,
                    bool & success)
{
    lllog(9) << "Entering " << BOOST_CURRENT_FUNCTION << std::endl;
    success = false;
    try
    {
        ControllerTable::Instance().GetController(ctrl)->Postpone(redispatchCurrent);
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void DoseC_ResumePostponed(const DotsC_Int32 ctrl,
                           bool & success)
{
    lllog(9) << "Entering " << BOOST_CURRENT_FUNCTION << std::endl;
    success = false;
    try
    {
        ControllerTable::Instance().GetController(ctrl)->ResumePostponed();
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void DoseC_IncompleteInjectionState(const DotsC_Int32 ctrl,
                                    bool & success)
{
    lllog(9) << "Entering " << BOOST_CURRENT_FUNCTION << std::endl;
    success = false;
    try
    {
        ControllerTable::Instance().GetController(ctrl)->IncompleteInjectionState();
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void DoseC_GetChannelId(const char * const state,
                        Safir::Dob::Typesystem::Int64 & channelId,
                        bool & success)
{
    lllog(9) << "Entering " << BOOST_CURRENT_FUNCTION << std::endl;
    success = false;
    try
    {
        const DistributionData data = DistributionData::ConstConstructor(new_data_tag_t(), state);
        channelId = data.GetChannelId().GetRawValue();
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void DoseC_GetTypeId(const char* const state,
                     Safir::Dob::Typesystem::Int64& typeId,
                     bool& success)
{
    lllog(9) << "Entering " << BOOST_CURRENT_FUNCTION << std::endl;
    success = false;
    try
    {
        const DistributionData data = DistributionData::ConstConstructor(new_data_tag_t(), state);
        typeId = data.GetTypeId();
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void DoseC_GetInstanceId(const char* const state,
                         Safir::Dob::Typesystem::Int64& instanceId,
                         bool& success)
{
    lllog(9) << "Entering " << BOOST_CURRENT_FUNCTION << std::endl;
    success = false;
    try
    {
        const DistributionData data = DistributionData::ConstConstructor(new_data_tag_t(), state);
        instanceId = data.GetInstanceId().GetRawValue();
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void DoseC_GetHandlerId(const char* const state,
                        Safir::Dob::Typesystem::Int64& handlerId,
                        bool& success)
{
    lllog(9) << "Entering " << BOOST_CURRENT_FUNCTION << std::endl;
    success = false;
    try
    {
        const DistributionData data = DistributionData::ConstConstructor(new_data_tag_t(), state);
        handlerId = data.GetHandlerId().GetRawValue();
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void DoseC_GetConnectionInfo(const char* const state,
                             char*& blob,
                             DoseC_BlobDeleter & deleter,
                             bool& success)
{
    lllog(9) << "Entering " << BOOST_CURRENT_FUNCTION << std::endl;
    success = false;
    try
    {
        const DistributionData data = DistributionData::ConstConstructor(new_data_tag_t(), state);

        Safir::Dob::ConnectionInfoPtr connInfo = Safir::Dob::ConnectionInfo::Create();

        const ConnectionId sender = data.GetSenderId();

        connInfo->NodeId() = sender.m_node;
        connInfo->ConnectionId() = sender.m_id;

        const std::string name = Connections::Instance().GetConnectionName(sender);
        if (!name.empty())
        {
            connInfo->ConnectionName() = Typesystem::Utilities::ToWstring(name);
        }

        Safir::Dob::Typesystem::BinarySerialization binary;
        Safir::Dob::Typesystem::Serialization::ToBinary(connInfo,binary);
        blob = Safir::Dob::Typesystem::Internal::BlobOperations::CreateCopy(&binary[0]);
        deleter = Safir::Dob::Typesystem::Internal::BlobOperations::Delete;

        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void DoseC_GetTopTimestamp(const char* const state,
                           Safir::Dob::Typesystem::Int64& timestamp,
                           bool& success)
{
    lllog(9) << "Entering " << BOOST_CURRENT_FUNCTION << std::endl;
    success = false;
    try
    {
        const DistributionData data = DistributionData::ConstConstructor(new_data_tag_t(), state);
        timestamp = data.GetTopTimestamp();
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void DoseC_GetMemberTimestamp(const char* const state,
                              const Safir::Dob::Typesystem::MemberIndex member,
                              Safir::Dob::Typesystem::Int64& timestamp,
                              bool& success)
{
    lllog(9) << "Entering " << BOOST_CURRENT_FUNCTION << std::endl;
    success = false;
    try
    {
        const DistributionData data = DistributionData::ConstConstructor(new_data_tag_t(), state);
        const Typesystem::Int64 * const timestamps = data.GetMemberTimestamps();
        if (timestamps == NULL)
        {
            std::wostringstream ostr;
            ostr << "This state (of entity "
                 << data.GetEntityId()
                 << ") does not have any timestamps! (hasBlob = "
                 << data.HasBlob();
            throw Safir::Dob::Typesystem::SoftwareViolationException(ostr.str(),__WFILE__,__LINE__);
        }
        timestamp = timestamps[member];
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}


void DoseC_GetQueueCapacity(const DotsC_Int32 ctrl,
                            const Safir::Dob::Typesystem::EnumerationValue queue,
                            Safir::Dob::Typesystem::Int32 & queueCapacity,
                            bool & success)
{
    lllog(9) << "Entering " << BOOST_CURRENT_FUNCTION << std::endl;
    success = false;
    try
    {
        queueCapacity = ControllerTable::Instance().GetController(ctrl)->
            GetQueueCapacity(static_cast<Safir::Dob::ConnectionQueueId::Enumeration>(queue));
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void DoseC_GetQueueSize(const DotsC_Int32 ctrl,
                        const Safir::Dob::Typesystem::EnumerationValue queue,
                        Safir::Dob::Typesystem::Int32 & queueSize,
                        bool & success)
{
    lllog(9) << "Entering " << BOOST_CURRENT_FUNCTION << std::endl;
    success = false;
    try
    {
        queueSize = ControllerTable::Instance().GetController(ctrl)->
            GetQueueSize(static_cast<Safir::Dob::ConnectionQueueId::Enumeration>(queue));
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void DoseC_Diff(const char* const previousState,
                const char* const currentState,
                const bool wantCurrent,
                const bool timestampDiff,
                char* & diffBlob,
                DoseC_BlobDeleter & deleter,
                bool& success)
{
    lllog(9) << "Entering " << BOOST_CURRENT_FUNCTION << std::endl;
    success = false;
    diffBlob = NULL;
    deleter = Safir::Dob::Typesystem::Internal::BlobOperations::Delete;
    try
    {
        const DistributionData previous = DistributionData::ConstConstructor(new_data_tag_t(), previousState);
        const DistributionData current = DistributionData::ConstConstructor(new_data_tag_t(), currentState);

        if (wantCurrent)
        {
            Typesystem::Internal::BlobWriteHelper writer(current.GetBlob());

            if (!previous.IsCreated())
            {
                writer.SetAllChanged(true);
            }
            else
            {
                if (timestampDiff)
                {
                    TimestampOperations::SetChangeFlags(previous, current, writer);
                }
                else
                {
                    //ordinary blob diff
                    writer.Diff(previous.GetBlob());
                }
            }

            diffBlob=writer.ToBlob();
        }
        else //wantPrevious
        {
            Typesystem::Internal::BlobWriteHelper writer(previous.GetBlob());

            if (!current.IsCreated())
            {
                writer.SetAllChanged(true);
            }
            else
            {
                if (timestampDiff)
                {
                    TimestampOperations::SetChangeFlags(previous, current, writer);
                }
                else
                {
                    //ordinary blob diff
                    writer.Diff(current.GetBlob());
                }
            }

            diffBlob=writer.ToBlob();
        }

        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS_AND_RUN(deleter(diffBlob)); //delete the blob if we got an exception
}

void DoseC_AddReference(const char* const state)
{
    lllog(9) << "Entering " << BOOST_CURRENT_FUNCTION << std::endl;
    DistributionData::AddReference(state);
}

void DoseC_DropReference(const char* const state)
{
    lllog(9) << "Entering " << BOOST_CURRENT_FUNCTION << std::endl;
    DistributionData::DropReference(state);
}


void DoseC_EntityIteratorCreate(const DotsC_Int32 ctrl,
                                const Safir::Dob::Typesystem::TypeId typeId,
                                const bool includeSubclasses,
                                Safir::Dob::Typesystem::Int32& iteratorId,
                                bool& end,
                                bool& success)
{
    lllog(9) << "Entering " << BOOST_CURRENT_FUNCTION << std::endl;
    success = false;
    try
    {
        iteratorId = ControllerTable::Instance().GetController(ctrl)->
            EntityIteratorCreate(typeId, includeSubclasses, end);
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void DoseC_EntityIteratorDestroy(const DotsC_Int32 ctrl,
                                 const Safir::Dob::Typesystem::Int32 iteratorId)
{
    lllog(9) << "Entering " << BOOST_CURRENT_FUNCTION << std::endl;
    try
    {
        ControllerTable::Instance().GetController(ctrl)->
            EntityIteratorDestroy(iteratorId);
    }
    catch (const std::exception& e)
    {
        SEND_SYSTEM_LOG(Alert, << "Got an exception while destroying an entity iterator: " << e.what())
    }
    catch (...)
    {
        SEND_SYSTEM_LOG(Alert, << "Got a ... exception while destroying an entity iterator. What are you doing?")
    }
}

void DoseC_EntityIteratorCopy(const DotsC_Int32 ctrl,
                              const Safir::Dob::Typesystem::Int32 iteratorId,
                              Safir::Dob::Typesystem::Int32& iteratorIdCopy,
                              bool& success)
{
    lllog(9) << "Entering " << BOOST_CURRENT_FUNCTION << std::endl;
    success = false;
    try
    {
        iteratorIdCopy = ControllerTable::Instance().GetController(ctrl)->
            EntityIteratorCopy(iteratorId);
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void DoseC_EntityIteratorIncrement(const DotsC_Int32 ctrl,
                                   const Safir::Dob::Typesystem::Int32 iteratorId,
                                   bool& end,
                                   bool& success)
{
    lllog(9) << "Entering " << BOOST_CURRENT_FUNCTION << std::endl;
    success = false;
    try
    {
        ControllerTable::Instance().GetController(ctrl)->
            EntityIteratorIncrement(iteratorId, end);
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void DoseC_EntityIteratorDereference(const DotsC_Int32 ctrl,
                                     const Safir::Dob::Typesystem::Int32 iteratorId,
                                     const char *& entityBlob,
                                     const char *& entityState,
                                     bool& success)
{
    lllog(9) << "Entering " << BOOST_CURRENT_FUNCTION << std::endl;
    success = false;
    try
    {
        ControllerTable::Instance().GetController(ctrl)->
            EntityIteratorDereference(iteratorId, entityBlob, entityState);

        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void DoseC_EntityIteratorEqual(const DotsC_Int32 ctrl,
                               const Safir::Dob::Typesystem::Int32 first,
                               const Safir::Dob::Typesystem::Int32 second,
                               bool& equal,
                               bool& success)
{
    lllog(9) << "Entering " << BOOST_CURRENT_FUNCTION << std::endl;
    success = false;
    try
    {
        equal = ControllerTable::Instance().GetController(ctrl)->
            EntityIteratorEqual(first,second);
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void DoseC_GetContext(const DotsC_Int32 ctrl,
                      DotsC_Int32& context,
                      bool& success)
{
    lllog(9) << "Entering " << BOOST_CURRENT_FUNCTION << std::endl;
    success = false;
    try
    {
        context = ControllerTable::Instance().GetController(ctrl)->GetContext();
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void DoseC_GetNodeId(DotsC_Int64& nodeId,
                     bool& success)
{
    lllog(9) << "Entering " << BOOST_CURRENT_FUNCTION << std::endl;
    success = false;
    try
    {
        nodeId = Connections::Instance().NodeId();
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}


void DoseC_SimulateOverflows(const DotsC_Int32 ctrl,
                             const bool inQueues,
                             const bool outQueues,
                             bool & success)
{
    lllog(9) << "Entering " << BOOST_CURRENT_FUNCTION << std::endl;
    success = false;
    try
    {
        ControllerTable::Instance().GetController(ctrl)->
            SimulateOverflows(inQueues,outQueues);
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}
