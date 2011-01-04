/******************************************************************************
*
* Copyright Saab AB, 2008 (http://www.safirsdk.com)
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

#include <Safir/Dob/Typesystem/Internal/InternalOperations.h>
#include <Safir/Dob/Typesystem/Internal/InternalUtils.h>
#include <Safir/Dob/Typesystem/LibraryExceptions.h>
#include <Safir/Dob/Typesystem/Utilities.h>
#include <Safir/Dob/Typesystem/Serialization.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Dob/Internal/SubscriptionOptions.h>
#include <Safir/Dob/Internal/Connections.h>
#include <Safir/Dob/Internal/InjectionKindTable.h>
#include <Safir/Dob/Internal/TimestampOperations.h>
#include <Safir/Dob/ConnectionInfo.h>

#include "dose_controller.h"
#include "dose_controller_table.h"
using namespace Safir::Dob::Internal;
using namespace Safir::Dob;

//-------------------------------------------------------------
// class Dose
//-------------------------------------------------------------
void DoseC_Constructor(long & ctrl,
                       bool & success)
{
    success = false;
    try
    {
        Safir::Dob::Internal::ControllerPtr controller(new Safir::Dob::Internal::Controller());
        ctrl = ControllerTable::Instance().AddController(controller);
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void DoseC_Destructor(const long ctrl)
{
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


void DoseC_IsConnected(const long ctrl, bool & isConnected, bool & success)
{
    success = false;
    try
    {
        ControllerPtr controller = ControllerTable::Instance().GetController(ctrl);
        isConnected = controller != NULL && controller->IsConnected();
        if (isConnected)
        {
            ControllerTable::Instance().ControllerTable::Instance().CheckThread(ctrl); //check the threading if we're in debug build
        }
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void DoseC_Connect(const long ctrl,
                   const char* connectionNameCommonPart,
                   const char* connectionNameInstancePart,
                   const DotsC_Int32 contextId,
                   const long lang,
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
                            const long lang,
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
                            long & newCtrlId,
                            bool & success)
{
    success = false;
    try
    {
        long ctrl=-1;

        //Is it an attach-without-arguments?
        if (strlen(connectionNameCommonPart) == 0 && strlen(connectionNameInstancePart) == 0)
        {
            ctrl = ControllerTable::Instance().GetFirstControllerInThread();
        }
        else
        {
            ctrl = ControllerTable::Instance().GetNamedControllerInThread(connectionNameCommonPart,connectionNameInstancePart);
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


void DoseC_Disconnect(const long ctrl, const bool checkThread, bool & success)
{
    success = false;
    try
    {
        ControllerPtr controller = ControllerTable::Instance().GetController(ctrl);
        ControllerTable::Instance().UnsetThread(ctrl, checkThread);

        if (controller->IsConnected())
        {
            controller->Disconnect();
        }
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void DoseC_GetConnectionName(const long ctrl, const char* &name, bool & success)
{
    success = false;
    try
    {
        ControllerTable::Instance().CheckThread(ctrl); //check the threading if we're in debug build
        name = ControllerTable::Instance().GetController(ctrl)->GetConnectionName();
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void DoseC_GetConnectionNameCommonPart(const long ctrl, const char* &name, bool & success)
{
    success = false;
    try
    {
        ControllerTable::Instance().CheckThread(ctrl); //check the threading if we're in debug build
        name = ControllerTable::Instance().GetController(ctrl)->
            GetConnectionNameCommonPart();
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void DoseC_GetConnectionNameInstancePart(const long ctrl, const char* &name, bool & success)
{
    success = false;
    try
    {
        ControllerTable::Instance().CheckThread(ctrl); //check the threading if we're in debug build
        name = ControllerTable::Instance().GetController(ctrl)->
            GetConnectionNameInstancePart();
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void DoseC_RegisterServiceHandler(const long ctrl,
                                  const Safir::Dob::Typesystem::TypeId typeId,
                                  const Safir::Dob::Typesystem::Int64 handlerId,
                                  const char* const handlerIdStr,
                                  const bool overrideRegistration,
                                  const long lang,
                                  void* const consumer,
                                  bool& success)
{
    success = false;
    try
    {
        ControllerTable::Instance().CheckThread(ctrl); //check the threading if we're in debug build
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

void DoseC_RegisterEntityHandler(const long ctrl,
                                 const Safir::Dob::Typesystem::TypeId typeId,
                                 const Safir::Dob::Typesystem::Int64 handlerId,
                                 const char* const handlerIdStr,
                                 const Safir::Dob::Typesystem::EnumerationValue instanceIdPolicy,
                                 const bool overrideRegistration,
                                 const bool injectionHandler,
                                 const long lang,
                                 void* const consumer,
                                 bool& success)
{
    success = false;
    try
    {
        ControllerTable::Instance().CheckThread(ctrl); //check the threading if we're in debug build
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


void DoseC_UnregisterHandler(const long ctrl,
                             const Safir::Dob::Typesystem::TypeId typeId,
                             const Safir::Dob::Typesystem::Int64 handlerId,
                             const char* const handlerIdStr,
                             bool& success)
{
    success = false;
    try
    {
        ControllerTable::Instance().CheckThread(ctrl); //check the threading if we're in debug build
        ControllerTable::Instance().GetController(ctrl)->
            UnregisterHandler(typeId,
                              Typesystem::HandlerId(handlerId,
                                                    Typesystem::Utilities::ToWstring(handlerIdStr)));
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}


void DoseC_SubscribeMessage(const long ctrl,
                            const Safir::Dob::Typesystem::TypeId typeId,
                            const Safir::Dob::Typesystem::Int64 channelId,
                            const char * const channelIdStr,
                            const bool includeSubclasses,
                            const long lang,
                            void* const consumer,
                            bool & success)
{
    success = false;
    try
    {
        ControllerTable::Instance().CheckThread(ctrl); //check the threading if we're in debug build
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

void DoseC_UnsubscribeMessage(const long ctrl,
                              const Safir::Dob::Typesystem::TypeId typeId,
                              const Safir::Dob::Typesystem::Int64 channelId,
                              const char * const channelIdStr,
                              const bool includeSubclasses,
                              const long lang,
                              void* const consumer,
                              bool & success)
{
    success = false;
    try
    {
        ControllerTable::Instance().CheckThread(ctrl); //check the threading if we're in debug build
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

void DoseC_SubscribeEntity(const long ctrl,
                           const Safir::Dob::Typesystem::TypeId typeId,
                           const Safir::Dob::Typesystem::Int64 instanceId,
                           const char* const instanceIdStr,
                           const bool allInstances,
                           const bool includeUpdates,
                           const bool includeSubclasses,
                           const bool restartSubscription,
                           const long lang,
                           void* const consumer,
                           bool& success)
{
    success = false;
    try
    {
        ControllerTable::Instance().CheckThread(ctrl); //check the threading if we're in debug build
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

void DoseC_InjectorSubscribeEntity(const long ctrl,
                                   const Safir::Dob::Typesystem::TypeId typeId,
                                   const bool includeUpdates,
                                   const bool includeSubclasses,
                                   const bool restartSubscription,
                                   const bool wantsGhostDelete,
                                   const bool wantsLastState,
                                   const bool doesntWantSourceIsPermanentStore,
                                   const bool wantsAllStateChanges,
                                   const bool timestampChangeInfo,
                                   const long lang,
                                   void* const consumer,
                                   bool& success)
{
    success = false;
    try
    {
        ControllerTable::Instance().CheckThread(ctrl); //check the threading if we're in debug build
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

void DoseC_UnsubscribeEntity(const long ctrl,
                             const Safir::Dob::Typesystem::TypeId typeId,
                             const Safir::Dob::Typesystem::Int64 instanceId,
                             const char* const instanceIdStr,
                             const bool allInstances,
                             const bool includeSubclasses,
                             const long lang,
                             void* const consumer,
                             bool& success)
{
    success = false;
    try
    {
        ControllerTable::Instance().CheckThread(ctrl); //check the threading if we're in debug build
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

void DoseC_SubscribeRegistration(const long ctrl,
                                 const Safir::Dob::Typesystem::TypeId typeId,
                                 const Safir::Dob::Typesystem::Int64 handlerId,
                                 const char* const handlerIdStr,
                                 const bool includeSubclasses,
                                 const bool restartSubscription,
                                 const long lang,
                                 void* const consumer,
                                 bool & success)
{
    success = false;
    try
    {
        ControllerTable::Instance().CheckThread(ctrl); //check the threading if we're in debug build
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

void DoseC_UnsubscribeRegistration(const long ctrl,
                                   const Safir::Dob::Typesystem::TypeId typeId,
                                   const Safir::Dob::Typesystem::Int64 handlerId,
                                   const char* const handlerIdStr,
                                   const bool includeSubclasses,
                                   const long lang,
                                   void* const consumer,
                                   bool & success)
{
    success = false;
    try
    {
        ControllerTable::Instance().CheckThread(ctrl); //check the threading if we're in debug build
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

void DoseC_Dispatch(const long ctrl,
                    bool & success)
{
    success = false;
    try
    {
        ControllerTable::Instance().CheckThread(ctrl); //check the threading if we're in debug build
        ControllerTable::Instance().GetController(ctrl)->Dispatch();
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void DoseC_ExitDispatch(const long ctrl,
                        bool & success)
{
    success = false;
    try
    {
        ControllerTable::Instance().CheckThread(ctrl); //check the threading if we're in debug build
        ControllerTable::Instance().GetController(ctrl)->ExitDispatch();
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void DoseC_GetCurrentCallbackId(const long ctrl,
                                DotsC_Int32& callbackId,
                                bool& success)
{
    success = false;
    try
    {
        ControllerTable::Instance().CheckThread(ctrl); //check the threading if we're in debug build
        callbackId = ControllerTable::Instance().GetController(ctrl)->CurrentCallback();
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}


//--------------------------------
// Message methods
//--------------------------------
// Send Message
void DoseC_SendMessage(const long ctrl,
                       const char * const message,
                       const Safir::Dob::Typesystem::Int64 channelId,
                       const char * const channelIdStr,
                       const long lang,
                       void* const consumer,
                       bool & success)
{
    success = false;
    try
    {
        ControllerTable::Instance().CheckThread(ctrl); //check the threading if we're in debug build
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
void DoseC_SendResponse(const long ctrl,
                        const char * const blob,
                        void * const consumer,
                        const long lang,
                        const Safir::Dob::Typesystem::Int32 responseId,
                        bool& success)
{
    success = false;
    try
    {
        ControllerTable::Instance().CheckThread(ctrl); //check the threading if we're in debug build
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
void DoseC_ServiceRequest(const long ctrl,
                          const char* const request,
                          const Safir::Dob::Typesystem::Int64 handlerId,
                          const char* const handlerIdStr,
                          const long lang,
                          void* const consumer,
                          Safir::Dob::RequestId& reqId,
                          bool& success)
{
    success = false;
    try
    {
        ControllerTable::Instance().CheckThread(ctrl); //check the threading if we're in debug build
        ControllerTable::Instance().GetController(ctrl)->
            ServiceRequest(request,
                           Typesystem::HandlerId(handlerId,Typesystem::Utilities::ToWstring(handlerIdStr)),
                           ConsumerId(consumer,lang),
                           reqId);
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}



void DoseC_CreateRequest(const long ctrl,
                         const char* const request,
                         const bool hasInstanceId,
                         const Safir::Dob::Typesystem::Int64 instanceId,
                         const char* const instanceIdStr,
                         const Safir::Dob::Typesystem::Int64 handlerId,
                         const char* const handlerIdStr,
                         const long lang,
                         void* const consumer,
                         Safir::Dob::RequestId& reqId,
                         bool& success)
{
    success = false;
    try
    {
        ControllerTable::Instance().CheckThread(ctrl); //check the threading if we're in debug build
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
void DoseC_UpdateRequest(const long ctrl,
                         const char* const request,
                         const Safir::Dob::Typesystem::Int64 instanceId,
                         const char* const instanceIdStr,
                         const long lang,
                         void* const consumer,
                         Safir::Dob::RequestId& reqId,
                         bool& success)
{
    success = false;
    try
    {
        ControllerTable::Instance().CheckThread(ctrl); //check the threading if we're in debug build
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
void DoseC_DeleteRequest(const long ctrl,
                         const Safir::Dob::Typesystem::TypeId typeId,
                         const Safir::Dob::Typesystem::Int64 instanceId,
                         const char* const instanceIdStr,
                         const long lang,
                         void* const consumer,
                         Safir::Dob::RequestId& reqId,
                         bool& success)
{
    success = false;
    try
    {
        ControllerTable::Instance().CheckThread(ctrl); //check the threading if we're in debug build
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
void DoseC_SetEntity(const long ctrl,
                     const char* const entity,
                     const Safir::Dob::Typesystem::Int64 instanceId,
                     const char* const instanceIdStr,
                     const Safir::Dob::Typesystem::Int64 handlerId,
                     const char* const handlerIdStr,
                     const bool considerChangeFlags,
                     const bool initialInjection,
                     bool& success)
{
    success = false;
    try
    {
        ControllerTable::Instance().CheckThread(ctrl); //check the threading if we're in debug build
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

void DoseC_DeleteEntity(const long ctrl,
                        const Safir::Dob::Typesystem::TypeId typeId,
                        const Safir::Dob::Typesystem::Int64 instanceId,
                        const char* const instanceIdStr,
                        const bool allInstances,
                        const Safir::Dob::Typesystem::Int64 handlerId,
                        const char* const handlerIdStr,
                        bool& success)
{
    success = false;
    try
    {
        ControllerTable::Instance().CheckThread(ctrl); //check the threading if we're in debug build
        ControllerTable::Instance().GetController(ctrl)->
            DeleteEntity(Typesystem::EntityId(typeId,
                                              Typesystem::InstanceId(instanceId, Typesystem::Utilities::ToWstring(instanceIdStr))),
                         allInstances,
                         Typesystem::HandlerId(handlerId,Typesystem::Utilities::ToWstring(handlerIdStr)));
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void DoseC_InjectEntity(const long ctrl,
                        const char* const entity,
                        const Safir::Dob::Typesystem::Int64 instanceId,
                        const char* const instanceIdStr,
                        const Safir::Dob::Typesystem::Int64 handlerId,
                        const char* const handlerIdStr,
                        const Safir::Dob::Typesystem::Int64 timestamp,
                        bool& success)
{
    success = false;
    try
    {
        ControllerTable::Instance().CheckThread(ctrl); //check the threading if we're in debug build
        ControllerTable::Instance().GetController(ctrl)->
            InjectEntity(entity,
                         Typesystem::InstanceId(instanceId,Typesystem::Utilities::ToWstring(instanceIdStr)),
                         Typesystem::HandlerId(handlerId,Typesystem::Utilities::ToWstring(handlerIdStr)),
                         timestamp);
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void DoseC_InjectDeletedEntity(const long ctrl,
                               const Safir::Dob::Typesystem::TypeId typeId,
                               const Safir::Dob::Typesystem::Int64 instanceId,
                               const char* const instanceIdStr,
                               const Safir::Dob::Typesystem::Int64 handlerId,
                               const char* const handlerIdStr,
                               const Safir::Dob::Typesystem::Int64 timestamp,
                               bool& success)
{
    success = false;
    try
    {
        ControllerTable::Instance().CheckThread(ctrl); //check the threading if we're in debug build
        ControllerTable::Instance().GetController(ctrl)->
            InjectDeletedEntity(Typesystem::EntityId(typeId,
                                                     Typesystem::InstanceId(instanceId, Typesystem::Utilities::ToWstring(instanceIdStr))),
                                Typesystem::HandlerId(handlerId,Typesystem::Utilities::ToWstring(handlerIdStr)),
                                timestamp);
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}


void DoseC_ReadEntity(const long ctrl,
                      const Safir::Dob::Typesystem::TypeId typeId,
                      const Safir::Dob::Typesystem::Int64 instanceId,
                      const char* const instanceIdStr,
                      const char*& currentBlob,
                      const char*& currentState,
                      bool& success)
{
    success = false;
    try
    {
        ControllerTable::Instance().CheckThread(ctrl); //check the threading if we're in debug build
        ControllerTable::Instance().GetController(ctrl)->
            ReadEntity(Typesystem::EntityId(typeId,
                                            Typesystem::InstanceId(instanceId, Typesystem::Utilities::ToWstring(instanceIdStr))),
                       currentBlob,
                       currentState);

        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void DoseC_IsCreated(const long ctrl,
                     const Safir::Dob::Typesystem::TypeId typeId,
                     const Safir::Dob::Typesystem::Int64 instanceId,
                     const char* const instanceIdStr,
                     bool& isCreated,
                     bool& success)
{
    success = false;
    try
    {
        ControllerTable::Instance().CheckThread(ctrl); //check the threading if we're in debug build
        isCreated = ControllerTable::Instance().GetController(ctrl)->
            IsCreated(Typesystem::EntityId(typeId,
                                           Typesystem::InstanceId(instanceId, Typesystem::Utilities::ToWstring(instanceIdStr))));
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void DoseC_GetNumberOfInstances(const long ctrl,
                                const Safir::Dob::Typesystem::TypeId typeId,
                                const Safir::Dob::Typesystem::Int64 handlerId,
                                const char* const /*handlerIdStr*/,
                                const bool includeSubclasses,
                                Safir::Dob::Typesystem::Int64& numberOfInstances,
                                bool& success)
{
    success = false;
    try
    {
        const Typesystem::HandlerId handler(handlerId);
        numberOfInstances = 0;

        ControllerTable::Instance().CheckThread(ctrl); //check the threading if we're in debug build
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

void DoseC_GetInstanceIdPolicy(const long ctrl,
                               const Safir::Dob::Typesystem::TypeId typeId,
                               const Safir::Dob::Typesystem::Int64 handlerId,
                               Safir::Dob::Typesystem::EnumerationValue& instanceIdPolicy,
                               bool& success)
{
    success = false;
    try
    {
        ControllerTable::Instance().CheckThread(ctrl); //check the threading if we're in debug build

        const ControllerPtr controller = ControllerTable::Instance().GetController(ctrl);

        instanceIdPolicy = static_cast<Safir::Dob::Typesystem::EnumerationValue>(controller->GetInstanceIdPolicy(typeId, Typesystem::HandlerId(handlerId)));
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void DoseC_Postpone(const long ctrl,
                    const bool redispatchCurrent,
                    bool & success)
{
    success = false;
    try
    {
        ControllerTable::Instance().CheckThread(ctrl); //check the threading if we're in debug build
        ControllerTable::Instance().GetController(ctrl)->Postpone(redispatchCurrent);
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void DoseC_ResumePostponed(const long ctrl,
                           bool & success)
{
    success = false;
    try
    {
        ControllerTable::Instance().CheckThread(ctrl); //check the threading if we're in debug build
        ControllerTable::Instance().GetController(ctrl)->ResumePostponed();
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void DoseC_IncompleteInjectionState(const long ctrl,
                                    bool & success)
{
    success = false;
    try
    {
        ControllerTable::Instance().CheckThread(ctrl); //check the threading if we're in debug build
        ControllerTable::Instance().GetController(ctrl)->IncompleteInjectionState();
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void DoseC_GetChannelId(const char * const state,
                        Safir::Dob::Typesystem::Int64 & channelId,
                        bool & success)
{
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
    success = false;
    try
    {
        const DistributionData data = DistributionData::ConstConstructor(new_data_tag_t(), state);

        Safir::Dob::ConnectionInfoPtr connInfo = Safir::Dob::ConnectionInfo::Create();

        const ConnectionId sender = data.GetSenderId();

        connInfo->NodeNumber() = sender.m_node;
        connInfo->ConnectionId() = sender.m_id;

        const std::string name = Connections::Instance().GetConnectionName(sender);
        if (!name.empty())
        {
            connInfo->ConnectionName() = Typesystem::Utilities::ToWstring(name);
        }

        Safir::Dob::Typesystem::BinarySerialization binary;
        Safir::Dob::Typesystem::Serialization::ToBinary(connInfo,binary);
        blob = Safir::Dob::Typesystem::Internal::CreateCopy(&binary[0]);
        deleter = Safir::Dob::Typesystem::Internal::Delete;

        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void DoseC_GetTopTimestamp(const char* const state,
                           Safir::Dob::Typesystem::Int64& timestamp,
                           bool& success)
{
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


void DoseC_GetQueueCapacity(const long ctrl,
                            const Safir::Dob::Typesystem::EnumerationValue queue,
                            Safir::Dob::Typesystem::Int32 & queueCapacity,
                            bool & success)
{
    success = false;
    try
    {
        ControllerTable::Instance().CheckThread(ctrl); //check the threading if we're in debug build
        queueCapacity = ControllerTable::Instance().GetController(ctrl)->
            GetQueueCapacity(static_cast<Safir::Dob::ConnectionQueueId::Enumeration>(queue));
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void DoseC_GetQueueSize(const long ctrl,
                        const Safir::Dob::Typesystem::EnumerationValue queue,
                        Safir::Dob::Typesystem::Int32 & queueSize,
                        bool & success)
{
    success = false;
    try
    {
        ControllerTable::Instance().CheckThread(ctrl); //check the threading if we're in debug build
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
    success = false;
    diffBlob = NULL;
    deleter = Safir::Dob::Typesystem::Internal::Delete;
    try
    {
        const DistributionData previous = DistributionData::ConstConstructor(new_data_tag_t(), previousState);
        const DistributionData current = DistributionData::ConstConstructor(new_data_tag_t(), currentState);

        if (wantCurrent)
        {
            diffBlob = Safir::Dob::Typesystem::Internal::CreateCopy(current.GetBlob());

            if (!previous.IsCreated())
            {
                Safir::Dob::Typesystem::Internal::SetChanged(diffBlob,true);
            }
            else
            {
                if (timestampDiff)
                {
                    TimestampOperations::SetChangeFlags(previous, current, diffBlob);
                }
                else
                {
                    //ordinary blob diff
                    Safir::Dob::Typesystem::Internal::Diff(previous.GetBlob(),diffBlob);
                }
            }
        }
        else //wantPrevious
        {
            diffBlob = Safir::Dob::Typesystem::Internal::CreateCopy(previous.GetBlob());

            if (!current.IsCreated())
            {
                Safir::Dob::Typesystem::Internal::SetChanged(diffBlob,true);
            }
            else
            {
                if (timestampDiff)
                {
                    TimestampOperations::SetChangeFlags(previous, current, diffBlob);
                }
                else
                {
                    //ordinary blob diff
                    Safir::Dob::Typesystem::Internal::Diff(current.GetBlob(),diffBlob);
                }
            }
        }

        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS_AND_RUN(deleter(diffBlob)); //delete the blob if we got an exception
}

void DoseC_AddReference(const char* const state)
{
    DistributionData::AddReference(state);
}

void DoseC_DropReference(const char* const state)
{
    DistributionData::DropReference(state);
}


void DoseC_EntityIteratorCreate(const long ctrl,
                                const Safir::Dob::Typesystem::TypeId typeId,
                                const bool includeSubclasses,
                                Safir::Dob::Typesystem::Int32& iteratorId,
                                bool& end,
                                bool& success)
{
    success = false;
    try
    {
        ControllerTable::Instance().CheckThread(ctrl); //check the threading if we're in debug build
        iteratorId = ControllerTable::Instance().GetController(ctrl)->
            EntityIteratorCreate(typeId, includeSubclasses, end);
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void DoseC_EntityIteratorDestroy(const long ctrl,
                                 const Safir::Dob::Typesystem::Int32 iteratorId)
{
    try
    {
        ControllerTable::Instance().CheckThread(ctrl); //check the threading if we're in debug build
        ControllerTable::Instance().GetController(ctrl)->
            EntityIteratorDestroy(iteratorId);
    }
    catch (...)
    {
        //TODO: do something useful here, crash maybe?
    }
}

void DoseC_EntityIteratorCopy(const long ctrl,
                              const Safir::Dob::Typesystem::Int32 iteratorId,
                              Safir::Dob::Typesystem::Int32& iteratorIdCopy,
                              bool& success)
{
    success = false;
    try
    {
        ControllerTable::Instance().CheckThread(ctrl); //check the threading if we're in debug build
        iteratorIdCopy = ControllerTable::Instance().GetController(ctrl)->
            EntityIteratorCopy(iteratorId);
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void DoseC_EntityIteratorIncrement(const long ctrl,
                                   const Safir::Dob::Typesystem::Int32 iteratorId,
                                   bool& end,
                                   bool& success)
{
    success = false;
    try
    {
        ControllerTable::Instance().CheckThread(ctrl); //check the threading if we're in debug build
        ControllerTable::Instance().GetController(ctrl)->
            EntityIteratorIncrement(iteratorId, end);
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void DoseC_EntityIteratorDereference(const long ctrl,
                                     const Safir::Dob::Typesystem::Int32 iteratorId,
                                     const char *& entityBlob,
                                     const char *& entityState,
                                     bool& success)
{
    success = false;
    try
    {
        ControllerTable::Instance().CheckThread(ctrl); //check the threading if we're in debug build
        ControllerTable::Instance().GetController(ctrl)->
            EntityIteratorDereference(iteratorId, entityBlob, entityState);

        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void DoseC_EntityIteratorEqual(const long ctrl,
                               const Safir::Dob::Typesystem::Int32 first,
                               const Safir::Dob::Typesystem::Int32 second,
                               bool& equal,
                               bool& success)
{
    success = false;
    try
    {
        ControllerTable::Instance().CheckThread(ctrl); //check the threading if we're in debug build
        equal = ControllerTable::Instance().GetController(ctrl)->
            EntityIteratorEqual(first,second);
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void DoseC_GetContext(const long ctrl,
                      DotsC_Int32& context,
                      bool& success)
{
    success = false;
    try
    {
        ControllerTable::Instance().CheckThread(ctrl); 
        context = ControllerTable::Instance().GetController(ctrl)->GetContext();
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}






// //-------------------------------
// // Debug
// //-------------------------------
// void DoseC_TestSupp_GetConnectionId(const char* name,
//                                     Safir::Dob::Typesystem::Int32& node,
//                                     Safir::Dob::Typesystem::Int64& id,
//                                     bool& nameExists,
//                                     bool & success)
// {
//    success = false;
//    try
//    {
//       Safir::Dob::Internal::Controller::GetConnectionId(name, node, id, nameExists);
//       success = true;
//    }
//    CATCH_LIBRARY_EXCEPTIONS;
// }

void DoseC_SimulateOverflows(const long ctrl,
                             const bool inQueues,
                             const bool outQueues,
                             bool & success)
{
    success = false;
    try
    {
        ControllerTable::Instance().CheckThread(ctrl); //check the threading if we're in debug build
        ControllerTable::Instance().GetController(ctrl)->
            SimulateOverflows(inQueues,outQueues);
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}




// void DoseC_TestSupp_GetObjectStats(Safir::Dob::Typesystem::TypeId tid[],
//                                    int writes[],
//                                    int& size,
//                                    bool & success)
// {
//    success = false;
//    try
//    {
//       Safir::Dob::Internal::Controller::GetObjectStats(tid, writes, size);
//       success = true;
//    }
//    CATCH_LIBRARY_EXCEPTIONS;
// }

// void DoseC_TestSupp_GetConnectionStats( const char* connName[],
//                                         int reqSent[],
//                                         int reqRecv[],
//                                         int reqSendOverflow[],
//                                         int reqRecvOverflow[],
//                                         int reqTimedOut[],
//                                         int respSent[],
//                                         int respRecv[],
//                                         int msgSent[],
//                                         int msgRecv[],
//                                         int msgSendOverflow[],
//                                         int msgRecvOverflow[],
//                                         int arrSize,
//                                         int& results,
//                                         bool& gotAll,
//                                         bool & success)
// {
//    success = false;
//    try
//    {
//       Safir::Dob::Internal::Controller::GetConnectionStats(connName,
//                                                            reqSent,
//                                                            reqRecv,
//                                                            reqSendOverflow,
//                                                            reqRecvOverflow,
//                                                            reqTimedOut,
//                                                            respSent,
//                                                            respRecv,
//                                                            msgSent,
//                                                            msgRecv,
//                                                            msgSendOverflow,
//                                                            msgRecvOverflow,
//                                                            arrSize,
//                                                            results,
//                                                            gotAll);
//       success = true;
//    }
//    CATCH_LIBRARY_EXCEPTIONS;
// }

// void DoseC_TestSupp_GetSharedMemoryGeneralInfo(  int& numberOfBlockGroups,
//                                                  int& numberOfMemoryBlocks,
//                                                  unsigned int& poolSizeInBytes,
//                                                  bool & success)
// {
//    success = false;
//    try
//    {
//       Safir::Dob::Internal::Controller::GetSharedMemoryGeneralInfo(numberOfBlockGroups,
//                                                                    numberOfMemoryBlocks,
//                                                                    poolSizeInBytes);
//       success = true;
//    }
//    CATCH_LIBRARY_EXCEPTIONS;
// }

// void DoseC_TestSupp_GetSharedMemoryDetailedStats(int blockSize[],
//                                                  int noBlocks[],
//                                                  int noAllocs[],
//                                                  int noDeallocs[],
//                                                  int highWaterMark[],
//                                                  bool & success)
// {
//    success = false;
//    try
//    {
//       Safir::Dob::Internal::Controller::GetSharedMemoryDetailedStats(blockSize, noBlocks, noAllocs, noDeallocs, highWaterMark);
//       success = true;
//    }
//    CATCH_LIBRARY_EXCEPTIONS;
// }

// void DoseC_TestSupp_GetQueueSize(Safir::Dob::Typesystem::Int64 connId,
//                                  int queue,
//                                  int& size,
//                                  bool & success)
// {
//    success = false;
//    try
//    {
//       Safir::Dob::Internal::Controller::GetQueueSize(connId, queue, size);
//       success = true;
//    }
//    CATCH_LIBRARY_EXCEPTIONS;
// }

// void DoseC_TestSupp_GetMessageQueueContent( Safir::Dob::Typesystem::Int64 connId,
//                                             bool outQ, //true if messageOutQ, false if messageInQ
//                                             Safir::Dob::Typesystem::TypeId types[],
//                                             Safir::Dob::Typesystem::InstanceNumber instances[],
//                                             int size,
//                                             int& noResults,
//                                             bool & success)
// {
//    success = false;
//    try
//    {
//       Safir::Dob::Internal::Controller::GetMessageQueueContent(connId, outQ, types, instances, size, noResults);
//       success = true;
//    }
//    CATCH_LIBRARY_EXCEPTIONS;
// }

// void DoseC_TestSupp_GetRequestQueueContent( Safir::Dob::Typesystem::Int64 connId,
//                                             bool outQ, //true if requestOut/responseIn, false if requestIn/responseOut
//                                             Safir::Dob::Typesystem::TypeId reqTypes[],
//                                             Safir::Dob::Typesystem::InstanceNumber reqInstances[],
//                                             Safir::Dob::Typesystem::TypeId respTypes[],
//                                             Safir::Dob::Typesystem::InstanceNumber respInstances[],
//                                             char typeOfReq[],
//                                             bool reqHandled[],
//                                             bool hasResponse[],
//                                             bool& validContent,
//                                             bool & success)
// {
//    success = false;
//    try
//    {
//       Safir::Dob::Internal::Controller::GetRequestQueueContent(connId, outQ, reqTypes, reqInstances, respTypes,
//                                                                respInstances, typeOfReq, reqHandled, hasResponse, validContent);
//       success = true;
//    }
//    CATCH_LIBRARY_EXCEPTIONS;
// }

// void DoseC_TestSupp_GetQueueItemDetails( Safir::Dob::Typesystem::Int64 connId,
//                                          int queue,
//                                          Safir::Dob::Typesystem::Int64& sender,
//                                          char * & blob,
//                                          bool & success)
// {
//    success = false;
//    try
//    {
//       Safir::Dob::Internal::Controller::GetQueueItemDetails(connId, queue, sender, blob);
//       success = true;
//    }
//    CATCH_LIBRARY_EXCEPTIONS;
// }

// void DoseC_TestSupp_GetSimulateFullQueue(Safir::Dob::Typesystem::Int64 connId,
//                                          int queue,
//                                          bool& simulateFull,
//                                          bool & success)
// {
//    success = false;
//    try
//    {
//       Safir::Dob::Internal::Controller::GetSimulateFullQueue(connId, queue, simulateFull);
//       success = true;
//    }
//    CATCH_LIBRARY_EXCEPTIONS;
// }

// void DoseC_TestSupp_SetSimulateFullQueue(Safir::Dob::Typesystem::Int64 connId,
//                                          int queue,
//                                          bool simulateFull,
//                                          bool & success)
// {
//    success = false;
//    try
//    {
//       Safir::Dob::Internal::Controller::SetSimulateFullQueue(connId, queue, simulateFull);
//       success = true;
//    }
//    CATCH_LIBRARY_EXCEPTIONS;
// }

