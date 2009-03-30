/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
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

#ifndef _dose_interface_h
#define _dose_interface_h

#include <Safir/Dob/Internal/ConnectionId.h>
#include <Safir/Dob/Typesystem/Defs.h>
#include <Safir/Dob/Defs.h>
#include <Safir/Dob/Typesystem/HandlerId.h>


#if defined _MSC_VER
#  ifdef DOSE_EXPORTS
#    define DOSE_API __declspec(dllexport)
#  else
#    define DOSE_API __declspec(dllimport)
#    ifndef NDEBUG
#      pragma comment( lib, "dose_dlld.lib" )
#    else
#      pragma comment( lib, "dose_dll.lib" )
#    endif
#  endif
#  define CALLING_CONVENTION __cdecl
#elif defined __GNUC__
#  define DOSE_API
#  if defined (__i386)
#    define CALLING_CONVENTION __attribute__((cdecl))
#  else
#    define CALLING_CONVENTION
#  endif
#endif


//--------------------------------------------------------------------
// Constants used to define from which language a call comes from
//
// All in-parameters called 'lang' is used by dose_dll to distinguish
// which set of callbacks to use for a specific consumer. Make sure
// you send the correct value of lang when using this API. See list
// below.
//--------------------------------------------------------------------
const long DOSE_LANGUAGE_CPP = 0;
//#define DOSE_LANGUAGE_ADA       1
//#define DOSE_LANGUAGE_DOTNET    2
//#define DOSE_LANGUAGE_JAVA      3
//--------------------------------------------------------------------

#ifdef __cplusplus
extern "C"
{
#endif


    //---------------------------------------------------------------------------
    // Callback functions
    //---------------------------------------------------------------------------
    typedef void CALLING_CONVENTION OnDispatchCb(void* const consumer,
                                                 bool& success);

    typedef void CALLING_CONVENTION OnStopOrderCb(void* const consumer,
                                                  bool& success);

    typedef void CALLING_CONVENTION OnNewEntityCb(const char* const currentBlob,
                                                  const char* const currentState,
                                                  void* const consumer,
                                                  const bool timestampDiff,
                                                  bool& success);

    typedef void CALLING_CONVENTION OnUpdatedEntityCb(const char* const currentBlob,
                                                      const char* const currentState,
                                                      const char* const previousBlob,
                                                      const char* const previousState,
                                                      void* const consumer,
                                                      const bool timestampDiff,
                                                      bool& success);

    typedef void CALLING_CONVENTION OnDeletedEntityCb(const char* const currentState,
                                                      const char* const previousBlob,
                                                      const char* const previousState,
                                                      const bool explicitlyDeleted,
                                                      void* const consumer,
                                                      const bool timestampDiff,
                                                      bool& success);

    typedef void CALLING_CONVENTION OnCreateRequestCb(const char* const requestBlob,
                                                      const char* const state,
                                                      const long ctrl,
                                                      const Safir::Dob::Typesystem::Int32 responseId,
                                                      void* const consumer,
                                                      bool& success);

    typedef void CALLING_CONVENTION OnUpdateRequestCb(const char* const requestBlob,
                                                      const char* const state,
                                                      const long ctrl,
                                                      const Safir::Dob::Typesystem::Int32 responseId,
                                                      void* const consumer,
                                                      bool& success);

    typedef void CALLING_CONVENTION OnDeleteRequestCb(const char* const state,
                                                      const long ctrl,
                                                      const Safir::Dob::Typesystem::Int32 responseId,
                                                      void* const consumer,
                                                      bool& success);

    typedef void CALLING_CONVENTION OnServiceRequestCb(const char* const requestBlob,
                                                       const char* const state,
                                                       const long ctrl,
                                                       const Safir::Dob::Typesystem::Int32 responseId,
                                                       void* const consumer,
                                                       bool& success);

    typedef void CALLING_CONVENTION OnResponseCb(const Safir::Dob::RequestId requestId,
                                                 const char* const responseBlob,
                                                 const char* const responseState,
                                                 const char* const requestBlob,
                                                 const char* const requestState,
                                                 void* const consumer,
                                                 bool& success);

    typedef void CALLING_CONVENTION OnMessageCb(const char* const message,
                                                const char* const state,
                                                void* const consumer,
                                                bool& success);

    typedef void CALLING_CONVENTION OnRegisteredCb(const Safir::Dob::Typesystem::TypeId typeId,
                                                   const Safir::Dob::Typesystem::Int64 handlerId,
                                                   const char* const handlerIdStr,
                                                   void* const consumer,
                                                   bool& success);

    typedef void CALLING_CONVENTION OnUnregisteredCb(const Safir::Dob::Typesystem::TypeId typeId,
                                                     const Safir::Dob::Typesystem::Int64 handlerId,
                                                     const char* const handlerIdStr,
                                                     void* const consumer,
                                                     bool& success);

    typedef void CALLING_CONVENTION OnRevokedRegistrationCb(const Safir::Dob::Typesystem::TypeId typeId,
                                                            const Safir::Dob::Typesystem::Int64 handlerId,
                                                            const char* const handlerIdStr,
                                                            void* const consumer,
                                                            bool& success);

    typedef void CALLING_CONVENTION OnCompletedRegistrationCb(const Safir::Dob::Typesystem::TypeId typeId,
                                                              const Safir::Dob::Typesystem::Int64 handlerId,
                                                              const char* const handlerIdStr,
                                                              void* const consumer,
                                                              bool& success);

    typedef void CALLING_CONVENTION OnInjectedNewEntityCb(const char* const injectionBlob,
                                                          const char* const injectionState,
                                                          void* const consumer,
                                                          bool& success);

    typedef void CALLING_CONVENTION OnInjectedUpdatedEntityCb(const char* const injectionBlob,
                                                              const char* const injectionState,
                                                              const char* const currentBlob,
                                                              const char* const currentState,
                                                              void* const consumer,
                                                              bool& success);

    typedef void CALLING_CONVENTION OnInjectedDeletedEntityCb(const char* const injectionState,
                                                              const char* const currentBlob,
                                                              const char* const currentState,
                                                              void* const consumer,
                                                              bool& success);

    typedef void CALLING_CONVENTION OnInitialInjectionsDoneCb(const Safir::Dob::Typesystem::TypeId typeId,
                                                              const Safir::Dob::Typesystem::Int64 handlerId,
                                                              const char* const handlerIdStr,
                                                              void* const consumer,
                                                              bool& success);

    typedef void CALLING_CONVENTION OnNotRequestOverflowCb(void* const consumer,
                                                           bool& success);

    typedef void CALLING_CONVENTION OnNotMessageOverflowCb(void* const consumer,
                                                           bool& success);


    //----------------------------------------------
    // Startup and Initialization methods
    //----------------------------------------------
    DOSE_API void CALLING_CONVENTION DoseC_Constructor(long& ctrl,
                                                       bool& success);

    DOSE_API void CALLING_CONVENTION DoseC_Destructor(const long ctrl);

    // Connect to local DOB
    DOSE_API void CALLING_CONVENTION DoseC_IsConnected(const long ctrl,
                                                       bool& isConnected,
                                                       bool& success);

    DOSE_API void CALLING_CONVENTION DoseC_Connect(const long ctrl,
                                                   const char* connectionNameCommonPart,
                                                   const char* connectionNameInstancePart,
                                                   const long context,
                                                   const long lang,
                                                   void* const connectionOwner,
                                                   void* const dispatcher,
                                                   OnDispatchCb* onDispatchCb,
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
                                                   bool& success);

    DOSE_API void CALLING_CONVENTION DoseC_ConnectSecondary(const char* connectionNameCommonPart,
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
                                                            long& newCtrlId,
                                                            bool& success);

    DOSE_API void CALLING_CONVENTION DoseC_Disconnect(const long ctrl,
                                                      const bool checkThread,
                                                      bool& success);


    DOSE_API void CALLING_CONVENTION DoseC_GetConnectionName(long ctrl,
                                                             const char*& name,
                                                             bool& success);

    DOSE_API void CALLING_CONVENTION DoseC_GetConnectionNameCommonPart(long ctrl,
                                                                       const char*& name,
                                                                       bool& success);

    DOSE_API void CALLING_CONVENTION DoseC_GetConnectionNameInstancePart(long ctrl,
                                                                         const char*& name,
                                                                         bool& success);

    DOSE_API  void CALLING_CONVENTION DoseC_RegisterServiceHandler(const long ctrl,
                                                                   const Safir::Dob::Typesystem::TypeId typeId,
                                                                   const Safir::Dob::Typesystem::Int64 handlerId,
                                                                   const char* const handlerIdStr,
                                                                   const bool overrideRegistration,
                                                                   const long lang,
                                                                   void* const consumer,
                                                                   bool& success);

    DOSE_API  void CALLING_CONVENTION DoseC_RegisterEntityHandler(const long ctrl,
                                                                  const Safir::Dob::Typesystem::TypeId typeId,
                                                                  const Safir::Dob::Typesystem::Int64 handlerId,
                                                                  const char* const handlerIdStr,
                                                                  const Safir::Dob::Typesystem::EnumerationValue instanceIdPolicy,
                                                                  const bool overrideRegistration,
                                                                  const bool injectionHandler,
                                                                  const long lang,
                                                                  void* const consumer,
                                                                  bool& success);

    DOSE_API void CALLING_CONVENTION DoseC_UnregisterHandler(const long ctrl,
                                                             const Safir::Dob::Typesystem::TypeId typeId,
                                                             const Safir::Dob::Typesystem::Int64 handlerId,
                                                             const char* const handlerIdStr,
                                                             bool& success);

    //---------------------------------------------------------------------------------------
    // Subscription methods
    //---------------------------------------------------------------------------------------

    // Subscribe on specific class type
    DOSE_API void CALLING_CONVENTION DoseC_SubscribeMessage(const long ctrl,
                                                            const Safir::Dob::Typesystem::TypeId typeId,
                                                            const Safir::Dob::Typesystem::Int64 channelId,
                                                            const char* const channelIdStr,
                                                            const bool includeSubclasses,
                                                            const long lang,
                                                            void* const consumer,
                                                            bool& success);

    DOSE_API void CALLING_CONVENTION DoseC_UnsubscribeMessage(const long ctrl,
                                                              const Safir::Dob::Typesystem::TypeId typeId,
                                                              const Safir::Dob::Typesystem::Int64 channelId,
                                                              const char* const channelIdStr,
                                                              const bool includeSubclasses,
                                                              const long lang,
                                                              void* const consumer,
                                                              bool& success);

    DOSE_API void CALLING_CONVENTION DoseC_SubscribeEntity(const long ctrl,
                                                           const Safir::Dob::Typesystem::TypeId typeId,
                                                           const Safir::Dob::Typesystem::Int64 instanceId,
                                                           const char* const instanceIdStr,
                                                           const bool allInstances,
                                                           const bool includeUpdates,
                                                           const bool includeSubclasses,
                                                           const bool restartSubscription,
                                                           const long lang,
                                                           void* const consumer,
                                                           bool& success);

    DOSE_API void CALLING_CONVENTION DoseC_InjectorSubscribeEntity(const long ctrl,
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
                                                                   bool& success);

    DOSE_API void CALLING_CONVENTION DoseC_UnsubscribeEntity(const long ctrl,
                                                             const Safir::Dob::Typesystem::TypeId typeId,
                                                             const Safir::Dob::Typesystem::Int64 instanceId,
                                                             const char* const instanceIdStr,
                                                             const bool allInstances,
                                                             const bool includeSubclasses,
                                                             const long lang,
                                                             void* const consumer,
                                                             bool& success);

    DOSE_API void CALLING_CONVENTION DoseC_SubscribeRegistration(const long ctrl,
                                                                 const Safir::Dob::Typesystem::TypeId typeId,
                                                                 const Safir::Dob::Typesystem::Int64 handlerId,
                                                                 const char* const handlerIdStr,
                                                                 const bool includeSubclasses,
                                                                 const bool restartSubscription,
                                                                 const long lang,
                                                                 void* const consumer,
                                                                 bool& success);

    DOSE_API void CALLING_CONVENTION DoseC_UnsubscribeRegistration(const long ctrl,
                                                                   const Safir::Dob::Typesystem::TypeId typeId,
                                                                   const Safir::Dob::Typesystem::Int64 handlerId,
                                                                   const char* const handlerIdStr,
                                                                   const bool includeSubclasses,
                                                                   const long lang,
                                                                   void* const consumer,
                                                                   bool& success);



    //---------------------------
    // Dispatch method
    //---------------------------
    // Perform dispatch
    DOSE_API void CALLING_CONVENTION DoseC_Dispatch(const long ctrl,
                                                    bool& success);

    // Interrupts current call to dispatch
    DOSE_API void CALLING_CONVENTION DoseC_ExitDispatch(const long ctrl,
                                                        bool& success);

    //---------------------------
    // Message method
    //---------------------------
    DOSE_API void CALLING_CONVENTION DoseC_SendMessage(const long ctrl,
                                                       const char* const message,
                                                       const Safir::Dob::Typesystem::Int64 channelId,
                                                       const char* const channelIdStr,
                                                       const long lang,
                                                       void* const consumer,
                                                       bool& success);

    //----------------------------
    // Request methods
    //----------------------------
    DOSE_API void CALLING_CONVENTION DoseC_ServiceRequest(const long ctrl,
                                                          const char* const request,
                                                          const Safir::Dob::Typesystem::Int64 handlerId,
                                                          const char* const handlerIdStr,
                                                          const long lang,
                                                          void* const consumer,
                                                          Safir::Dob::RequestId& reqId,
                                                          bool& success);

    DOSE_API void CALLING_CONVENTION DoseC_CreateRequest(const long ctrl,
                                                         char const* const request,
                                                         const bool hasInstanceId,
                                                         const Safir::Dob::Typesystem::Int64 instanceId,
                                                         const char* const instanceIdStr,
                                                         const Safir::Dob::Typesystem::Int64 handlerId,
                                                         const char* const handlerIdStr,
                                                         const long lang,
                                                         void* const consumer,
                                                         Safir::Dob::RequestId& reqId,
                                                         bool& success);

    DOSE_API void CALLING_CONVENTION DoseC_UpdateRequest(const long ctrl,
                                                         char const* const request,
                                                         const Safir::Dob::Typesystem::Int64 instanceId,
                                                         const char* const instanceIdStr,
                                                         const long lang,
                                                         void* const consumer,
                                                         Safir::Dob::RequestId& reqId,
                                                         bool& success);

    DOSE_API void CALLING_CONVENTION DoseC_DeleteRequest(const long ctrl,
                                                         const Safir::Dob::Typesystem::TypeId typeId,
                                                         const Safir::Dob::Typesystem::Int64 instanceId,
                                                         const char* const instanceIdStr,
                                                         const long lang,
                                                         void* const consumer,
                                                         Safir::Dob::RequestId& reqId,
                                                         bool& success);

    //---------------------------
    // Response method
    //---------------------------
    // Send Response
    DOSE_API void CALLING_CONVENTION DoseC_SendResponse(const long ctrl,
                                                        const char * const blob,
                                                        void * const consumer,
                                                        const long lang,
                                                        const Safir::Dob::Typesystem::Int32 responseId,
                                                        bool& success);

    //----------------------------
    // Entity methods
    //----------------------------
    //Change flags will be set to false on SetEntity
    DOSE_API void CALLING_CONVENTION DoseC_SetEntity(const long ctrl,
                                                     const char* const blob,
                                                     const Safir::Dob::Typesystem::Int64 instanceId,
                                                     const char* const instanceIdStr,
                                                     const Safir::Dob::Typesystem::Int64 handlerId,
                                                     const char* const handlerIdStr,
                                                     const bool considerChangeFlags,
                                                     const bool initialInjection,
                                                     bool& success);

    DOSE_API void CALLING_CONVENTION DoseC_DeleteEntity(const long ctrl,
                                                        const Safir::Dob::Typesystem::TypeId typeId,
                                                        const Safir::Dob::Typesystem::Int64 instanceId,
                                                        const char* const instanceIdStr,
                                                        const bool allInstances,
                                                        const Safir::Dob::Typesystem::Int64 handlerId,
                                                        const char* const handlerIdStr,
                                                        bool& success);

    DOSE_API void CALLING_CONVENTION DoseC_InjectEntity(const long ctrl,
                                                        const char* const blob,
                                                        const Safir::Dob::Typesystem::Int64 instanceId,
                                                        const char* const instanceIdStr,
                                                        const Safir::Dob::Typesystem::Int64 handlerId,
                                                        const char* const handlerIdStr,
                                                        const Safir::Dob::Typesystem::Int64 timestamp,
                                                        bool& success);

    DOSE_API void CALLING_CONVENTION DoseC_InjectDeletedEntity(const long ctrl,
                                                               const Safir::Dob::Typesystem::TypeId typeId,
                                                               const Safir::Dob::Typesystem::Int64 instanceId,
                                                               const char* const instanceIdStr,
                                                               const Safir::Dob::Typesystem::Int64 handlerId,
                                                               const char* const handlerIdStr,
                                                               const Safir::Dob::Typesystem::Int64 timestamp,
                                                               bool& success);


    DOSE_API void CALLING_CONVENTION DoseC_ReadEntity(const long ctrl,
                                                      const Safir::Dob::Typesystem::TypeId typeId,
                                                      const Safir::Dob::Typesystem::Int64 instanceId,
                                                      const char* const instanceIdStr,
                                                      const char*& currentBlob,
                                                      const char*& currentState,
                                                      bool& success);

    DOSE_API void CALLING_CONVENTION DoseC_IsCreated(const long ctrl,
                                                     const Safir::Dob::Typesystem::TypeId typeId,
                                                     const Safir::Dob::Typesystem::Int64 instanceId,
                                                     const char* const instanceIdStr,
                                                     bool& isCreated,
                                                     bool& success);

    DOSE_API void CALLING_CONVENTION DoseC_GetNumberOfInstances(const long ctrl,
                                                                const Safir::Dob::Typesystem::TypeId typeId,
                                                                const Safir::Dob::Typesystem::Int64 handlerId,
                                                                const char* const handlerIdStr,
                                                                const bool includeSubclasses,
                                                                Safir::Dob::Typesystem::Int64& numberOfInstances,
                                                                bool& success);

    DOSE_API void CALLING_CONVENTION DoseC_Postpone(const long ctrl,
                                                    const bool redispatchCurrent,
                                                    bool& success);

    DOSE_API void CALLING_CONVENTION DoseC_ResumePostponed(const long ctrl,
                                                           bool& success);

    DOSE_API void CALLING_CONVENTION DoseC_IncompleteInjectionState(const long ctrl,
                                                                    bool& success);

    DOSE_API void CALLING_CONVENTION DoseC_GetChannelId(const char* const state,
                                                        Safir::Dob::Typesystem::Int64& channelId,
                                                        bool& success);

    DOSE_API void CALLING_CONVENTION DoseC_GetTypeId(const char* const state,
                                                     Safir::Dob::Typesystem::TypeId& typeId,
                                                     bool& success);

    DOSE_API void CALLING_CONVENTION DoseC_GetInstanceId(const char* const state,
                                                        Safir::Dob::Typesystem::Int64& instanceId,
                                                        bool& success);

    DOSE_API void CALLING_CONVENTION DoseC_GetHandlerId(const char* const state,
                                                        Safir::Dob::Typesystem::Int64& handlerId,
                                                        bool& success);


    typedef void (CALLING_CONVENTION * DoseC_BlobDeleter)(char * &);

    DOSE_API void CALLING_CONVENTION DoseC_GetConnectionInfo(const char* const state,
                                                             char*& blob,
                                                             DoseC_BlobDeleter & deleter,
                                                             bool& success);

    DOSE_API void CALLING_CONVENTION DoseC_GetTopTimestamp(const char* const state,
                                                           Safir::Dob::Typesystem::Int64& timestamp,
                                                           bool& success);

    DOSE_API void CALLING_CONVENTION DoseC_GetMemberTimestamp(const char* const state,
                                                              const Safir::Dob::Typesystem::MemberIndex member,
                                                              Safir::Dob::Typesystem::Int64& timestamp,
                                                              bool& success);

    DOSE_API void CALLING_CONVENTION DoseC_GetQueueCapacity(const long ctrl,
                                                            const Safir::Dob::Typesystem::EnumerationValue queue,
                                                            Safir::Dob::Typesystem::Int32& queueCapacity,
                                                            bool& success);

    DOSE_API void CALLING_CONVENTION DoseC_GetQueueSize(const long ctrl,
                                                        const Safir::Dob::Typesystem::EnumerationValue queue,
                                                        Safir::Dob::Typesystem::Int32& queueSize,
                                                        bool& success);

    /**
     * Compare two states and return a new blob with change flags set accordingly.
     *
     * @param wantsCurrent - if true it is the blob from currentState that will be returned
     *                       if false it is the blob from previousState that will be returned
     *                          (with the diff made in "reverse")
     * @param timestampDiff - Do a diff using the timestamps, not the actual data.
     */
    DOSE_API void CALLING_CONVENTION DoseC_Diff(const char* const previousState,
                                                const char* const currentState,
                                                const bool wantCurrent,
                                                const bool timestampDiff,
                                                char* & diffBlob,
                                                DoseC_BlobDeleter & deleter,
                                                bool& success);

    DOSE_API void CALLING_CONVENTION DoseC_AddReference(const char* const state);

    DOSE_API void CALLING_CONVENTION DoseC_DropReference(const char* const state);


    //-------------------------------
    // EntityIterator functions
    //-------------------------------
    //destroy needs to be called even if end is true!
    DOSE_API void CALLING_CONVENTION DoseC_EntityIteratorCreate(const long ctrl,
                                                                const Safir::Dob::Typesystem::TypeId typeId,
                                                                const bool includeSubclasses,
                                                                Safir::Dob::Typesystem::Int32& iteratorId,
                                                                bool& end,
                                                                bool& success);

    DOSE_API void CALLING_CONVENTION DoseC_EntityIteratorDestroy(const long ctrl,
                                                                 const Safir::Dob::Typesystem::Int32 iteratorId);

    DOSE_API void CALLING_CONVENTION DoseC_EntityIteratorCopy(const long ctrl,
                                                              const Safir::Dob::Typesystem::Int32 iteratorId,
                                                              Safir::Dob::Typesystem::Int32& iteratorIdCopy,
                                                              bool& success);

    DOSE_API void CALLING_CONVENTION DoseC_EntityIteratorIncrement(const long ctrl,
                                                                   const Safir::Dob::Typesystem::Int32 iteratorId,
                                                                   bool& end,
                                                                   bool& success);

    DOSE_API void CALLING_CONVENTION DoseC_EntityIteratorDereference(const long ctrl,
                                                                     const Safir::Dob::Typesystem::Int32 iteratorId,
                                                                     const char *& entityBlob,
                                                                     const char *& entityState,
                                                                     bool& success);

    DOSE_API void CALLING_CONVENTION DoseC_EntityIteratorEqual(const long ctrl,
                                                               const Safir::Dob::Typesystem::Int32 first,
                                                               const Safir::Dob::Typesystem::Int32 second,
                                                               bool& equal,
                                                               bool& success);

    //-------------------------------
    // Debug and statistics
    //-------------------------------
    DOSE_API void CALLING_CONVENTION DoseC_SimulateOverflows(const long ctrl,
                                                             const bool inQueues,
                                                             const bool outQueues,
                                                             bool& success);

    /*     //\************************************************************************ */
    /*     //No need to implement in all languages - currently only used by SATE */
    /*     //\************************************************************************ */
    /*     DOSE_API void CALLING_CONVENTION DoseC_TestSupp_GetConnectionStats(const char* connName[], */
    /*                                                             int reqSent[], */
    /*                                                             int reqRecv[], */
    /*                                                             int reqSendOverflow[], */
    /*                                                             int reqRecvOverflow[], */
    /*                                                             int reqTimedOut[], */
    /*                                                             int respSent[], */
    /*                                                             int respRecv[], */
    /*                                                             int msgSent[], */
    /*                                                             int msgRecv[], */
    /*                                                             int msgSendOverflow[], */
    /*                                                             int msgRecvOverflow[], */
    /*                                                             int arrSize, */
    /*                                                             int& results, */
    /*                                                             bool& gotAll, */
    /*                                                             bool& success); */

    /*     //arrays are assumed to be large enough, use Dots to find out number of typeIds */
    /*     DOSE_API void CALLING_CONVENTION DoseC_TestSupp_GetObjectStats(Safir::Dob::Typesystem::TypeId tid[], int writes[], int& size, bool& success); */

    /*     DOSE_API void CALLING_CONVENTION DoseC_TestSupp_GetSharedMemoryGeneralInfo(int& numberOfBlockGroups, */
    /*                                                                     int& numberOfMemoryBlocks, */
    /*                                                                     unsigned int& poolSizeInBytes, */
    /*                                                                     bool& success); */

    /*     //arrays are assumed to be at least of size numberOfBlockGroups */
    /*     DOSE_API void CALLING_CONVENTION DoseC_TestSupp_GetSharedMemoryDetailedStats(int blockSize[], */
    /*                                                                       int noBlocks[], */
    /*                                                                       int noAllocs[], */
    /*                                                                       int noDeallocs[], */
    /*                                                                       int highWaterMark[], */
    /*                                                                       bool& success); */

    /*     //---------------------------------------------------------------------------- */
    /*     //reading content in queues */
    /*     // param queue: 0=msg_in, 1=msg_out, 2=req_in, 3=req_out */
    /*     //---------------------------------------------------------------------------- */
    /*     DOSE_API void CALLING_CONVENTION DoseC_TestSupp_GetConnectionId(const char* name, */
    /*                                                          Safir::Dob::Typesystem::Int32& node, */
    /*                                                          Safir::Dob::Typesystem::Int64& id, */
    /*                                                          bool& nameExists , */
    /*                                                          bool& success); */

    /*     DOSE_API void CALLING_CONVENTION DoseC_TestSupp_GetQueueSize(Safir::Dob::Typesystem::Int64 connId, */
    /*                                                       int queue, */
    /*                                                       int& size, */
    /*                                                       bool& success); */

    /*     DOSE_API void CALLING_CONVENTION DoseC_TestSupp_GetMessageQueueContent( */
    /*                                                                 Safir::Dob::Typesystem::Int64 connId, */
    /*                                                                 bool outQ, //true if messageOutQ, false if messageInQ */
    /*                                                                 Safir::Dob::Typesystem::TypeId types[], */
    /*                                                                 Safir::Dob::Typesystem::InstanceNumber instances[], */
    /*                                                                 int size, */
    /*                                                                 int& noResults, */
    /*                                                                 bool& success); */

    /*     DOSE_API void CALLING_CONVENTION DoseC_TestSupp_GetRequestQueueContent( */
    /*                                                                 Safir::Dob::Typesystem::Int64 connId, */
    /*                                                                 bool outQ, //true if requestOut/responseIn, false if requestIn/responseOut */
    /*                                                                 Safir::Dob::Typesystem::TypeId reqTypes[], */
    /*                                                                 Safir::Dob::Typesystem::InstanceNumber reqInstances[], */
    /*                                                                 Safir::Dob::Typesystem::TypeId respTypes[], */
    /*                                                                 Safir::Dob::Typesystem::InstanceNumber respInstances[], */
    /*                                                                 char typeOfReq[], */
    /*                                                                 bool reqHandled[], */
    /*                                                                 bool hasResponse[], */
    /*                                                                 bool& validContent, */
    /*                                                                 bool& success); */

    /*     DOSE_API void CALLING_CONVENTION DoseC_TestSupp_GetQueueItemDetails(Safir::Dob::Typesystem::Int64 connId, */
    /*                                                              int queue, */
    /*                                                              Safir::Dob::Typesystem::Int64& sender, */
    /*                                                              char *& blob, */
    /*                                                              bool& success); */

    /*     DOSE_API void CALLING_CONVENTION DoseC_TestSupp_GetSimulateFullQueue(Safir::Dob::Typesystem::Int64 connId, */
    /*                                                               int queue, */
    /*                                                               bool& simulateFull, */
    /*                                                               bool& success); */

    /*     DOSE_API void CALLING_CONVENTION DoseC_TestSupp_SetSimulateFullQueue(Safir::Dob::Typesystem::Int64 connId, */
    /*                                                               int queue, */
    /*                                                               bool simulateFull, */
    /*                                                               bool& success); */



#ifdef __cplusplus
}
#endif

#endif //_dose_h
