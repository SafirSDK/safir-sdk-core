/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
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

#include <Safir/Utilities/Internal/VisibilityHelpers.h>

#ifdef dose_dll_EXPORTS
#  define DOSE_DLL_API SAFIR_HELPER_DLL_EXPORT
#else
#  define DOSE_DLL_API SAFIR_HELPER_DLL_IMPORT
#  define SAFIR_LIBRARY_NAME "dose_dll"
#  define SAFIR_NO_DEBUG_LIBRARY_SUFFIX
#  include <Safir/Utilities/Internal/AutoLink.h>
#endif
#define DOSE_DLL_LOCAL SAFIR_HELPER_DLL_LOCAL


#include <Safir/Dob/Typesystem/Internal/KernelDefs.h>

typedef DotsC_Int32 DoseC_RequestId;

//--------------------------------------------------------------------
// Constants used to define from which language a call comes from
//
// All in-parameters called 'lang' is used by dose_dll to distinguish
// which set of callbacks to use for a specific consumer. Make sure
// you send the correct value of lang when using this API. See list
// below.
//--------------------------------------------------------------------
const long DOSE_LANGUAGE_CPP    = 0;
const long DOSE_LANGUAGE_ADA    = 1;
const long DOSE_LANGUAGE_DOTNET = 2;
const long DOSE_LANGUAGE_JAVA   = 3;

// Define which languages use garbage collection
static const bool g_garbageCollected[] = {false, //C++
                                          false, //Ada
                                          true,  //Dotnet
                                          true}; //Java
//--------------------------------------------------------------------

#ifdef __cplusplus
extern "C"
{
#endif


    //---------------------------------------------------------------------------
    // Callback functions
    //---------------------------------------------------------------------------
    typedef void OnDispatchCb(void* const consumer,
                              bool& success);

    typedef void OnStopOrderCb(void* const consumer,
                               bool& success);

    typedef void OnNewEntityCb(const char* const currentBlob,
                               const char* const currentState,
                               void* const consumer,
                               const bool timestampDiff,
                               bool& success);

    typedef void OnUpdatedEntityCb(const char* const currentBlob,
                                   const char* const currentState,
                                   const char* const previousBlob,
                                   const char* const previousState,
                                   void* const consumer,
                                   const bool timestampDiff,
                                   bool& success);

    typedef void OnDeletedEntityCb(const char* const currentState,
                                   const char* const previousBlob,
                                   const char* const previousState,
                                   const bool explicitlyDeleted,
                                   void* const consumer,
                                   const bool timestampDiff,
                                   bool& success);

    typedef void OnCreateRequestCb(const char* const requestBlob,
                                   const char* const state,
                                   const long ctrl,
                                   const DotsC_Int32 responseId,
                                   void* const consumer,
                                   bool& success);

    typedef void OnUpdateRequestCb(const char* const requestBlob,
                                   const char* const state,
                                   const long ctrl,
                                   const DotsC_Int32 responseId,
                                   void* const consumer,
                                   bool& success);

    typedef void OnDeleteRequestCb(const char* const state,
                                   const long ctrl,
                                   const DotsC_Int32 responseId,
                                   void* const consumer,
                                   bool& success);

    typedef void OnServiceRequestCb(const char* const requestBlob,
                                    const char* const state,
                                    const long ctrl,
                                    const DotsC_Int32 responseId,
                                    void* const consumer,
                                    bool& success);

    typedef void OnResponseCb(const DoseC_RequestId requestId,
                              const char* const responseBlob,
                              const char* const responseState,
                              const char* const requestBlob,
                              const char* const requestState,
                              void* const consumer,
                              bool& success);

    typedef void OnMessageCb(const char* const message,
                             const char* const state,
                             void* const consumer,
                             bool& success);

    typedef void OnRegisteredCb(const DotsC_TypeId typeId,
                                const DotsC_Int64 handlerId,
                                const char* const handlerIdStr,
                                void* const consumer,
                                bool& success);

    typedef void OnUnregisteredCb(const DotsC_TypeId typeId,
                                  const DotsC_Int64 handlerId,
                                  const char* const handlerIdStr,
                                  void* const consumer,
                                  bool& success);

    typedef void OnRevokedRegistrationCb(const DotsC_TypeId typeId,
                                         const DotsC_Int64 handlerId,
                                         const char* const handlerIdStr,
                                         void* const consumer,
                                         bool& success);

    typedef void OnCompletedRegistrationCb(const DotsC_TypeId typeId,
                                           const DotsC_Int64 handlerId,
                                           const char* const handlerIdStr,
                                           void* const consumer,
                                           bool& success);

    typedef void OnInjectedNewEntityCb(const char* const injectionBlob,
                                       const char* const injectionState,
                                       void* const consumer,
                                       bool& success);

    typedef void OnInjectedUpdatedEntityCb(const char* const injectionBlob,
                                           const char* const injectionState,
                                           const char* const currentBlob,
                                           const char* const currentState,
                                           void* const consumer,
                                           bool& success);

    typedef void OnInjectedDeletedEntityCb(const char* const injectionState,
                                           const char* const currentBlob,
                                           const char* const currentState,
                                           void* const consumer,
                                           bool& success);

    typedef void OnInitialInjectionsDoneCb(const DotsC_TypeId typeId,
                                           const DotsC_Int64 handlerId,
                                           const char* const handlerIdStr,
                                           void* const consumer,
                                           bool& success);

    typedef void OnNotRequestOverflowCb(void* const consumer,
                                        bool& success);

    typedef void OnNotMessageOverflowCb(void* const consumer,
                                        bool& success);

    typedef void OnDropReferenceCb(void* const consumer,
                                   const long refCounter,
                                   bool& success);


    //----------------------------------------------
    // Startup and Initialization methods
    //----------------------------------------------
    DOSE_DLL_API void DoseC_Constructor(long& ctrl,
                                        bool& success);

    DOSE_DLL_API void DoseC_Destructor(const long ctrl);

    // Connect to local DOB
    DOSE_DLL_API void DoseC_IsConnected(const long ctrl,
                                        bool& isConnected,
                                        bool& success);

    DOSE_DLL_API void DoseC_Connect(const long ctrl,
                                    const char* connectionNameCommonPart,
                                    const char* connectionNameInstancePart,
                                    const DotsC_Int32 contextId,
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
                                    OnDropReferenceCb* onDropReferenceCb,
                                    bool& success);

    DOSE_DLL_API void DoseC_ConnectSecondary(const char* connectionNameCommonPart,
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
                                             long& newCtrlId,
                                             bool& success);

    DOSE_DLL_API void DoseC_Disconnect(const long ctrl,
                                       const bool checkThread,
                                       bool& success);


    DOSE_DLL_API void DoseC_GetConnectionName(long ctrl,
                                              const char*& name,
                                              bool& success);

    DOSE_DLL_API void DoseC_GetConnectionNameCommonPart(long ctrl,
                                                        const char*& name,
                                                        bool& success);

    DOSE_DLL_API void DoseC_GetConnectionNameInstancePart(long ctrl,
                                                          const char*& name,
                                                          bool& success);

    DOSE_DLL_API  void DoseC_RegisterServiceHandler(const long ctrl,
                                                    const DotsC_TypeId typeId,
                                                    const DotsC_Int64 handlerId,
                                                    const char* const handlerIdStr,
                                                    const bool overrideRegistration,
                                                    const long lang,
                                                    void* const consumer,
                                                    bool& success);

    DOSE_DLL_API  void DoseC_RegisterEntityHandler(const long ctrl,
                                                   const DotsC_TypeId typeId,
                                                   const DotsC_Int64 handlerId,
                                                   const char* const handlerIdStr,
                                                   const DotsC_EnumerationValue instanceIdPolicy,
                                                   const bool overrideRegistration,
                                                   const bool injectionHandler,
                                                   const long lang,
                                                   void* const consumer,
                                                   bool& success);

    DOSE_DLL_API void DoseC_UnregisterHandler(const long ctrl,
                                              const DotsC_TypeId typeId,
                                              const DotsC_Int64 handlerId,
                                              const char* const handlerIdStr,
                                              bool& success);

    //---------------------------------------------------------------------------------------
    // Subscription methods
    //---------------------------------------------------------------------------------------

    // Subscribe on specific class type
    DOSE_DLL_API void DoseC_SubscribeMessage(const long ctrl,
                                             const DotsC_TypeId typeId,
                                             const DotsC_Int64 channelId,
                                             const char* const channelIdStr,
                                             const bool includeSubclasses,
                                             const long lang,
                                             void* const consumer,
                                             bool& success);

    DOSE_DLL_API void DoseC_UnsubscribeMessage(const long ctrl,
                                               const DotsC_TypeId typeId,
                                               const DotsC_Int64 channelId,
                                               const char* const channelIdStr,
                                               const bool includeSubclasses,
                                               const long lang,
                                               void* const consumer,
                                               bool& success);

    DOSE_DLL_API void DoseC_SubscribeEntity(const long ctrl,
                                            const DotsC_TypeId typeId,
                                            const DotsC_Int64 instanceId,
                                            const char* const instanceIdStr,
                                            const bool allInstances,
                                            const bool includeUpdates,
                                            const bool includeSubclasses,
                                            const bool restartSubscription,
                                            const long lang,
                                            void* const consumer,
                                            bool& success);

    DOSE_DLL_API void DoseC_InjectorSubscribeEntity(const long ctrl,
                                                    const DotsC_TypeId typeId,
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

    DOSE_DLL_API void DoseC_UnsubscribeEntity(const long ctrl,
                                              const DotsC_TypeId typeId,
                                              const DotsC_Int64 instanceId,
                                              const char* const instanceIdStr,
                                              const bool allInstances,
                                              const bool includeSubclasses,
                                              const long lang,
                                              void* const consumer,
                                              bool& success);

    DOSE_DLL_API void DoseC_SubscribeRegistration(const long ctrl,
                                                  const DotsC_TypeId typeId,
                                                  const DotsC_Int64 handlerId,
                                                  const char* const handlerIdStr,
                                                  const bool includeSubclasses,
                                                  const bool restartSubscription,
                                                  const long lang,
                                                  void* const consumer,
                                                  bool& success);

    DOSE_DLL_API void DoseC_UnsubscribeRegistration(const long ctrl,
                                                    const DotsC_TypeId typeId,
                                                    const DotsC_Int64 handlerId,
                                                    const char* const handlerIdStr,
                                                    const bool includeSubclasses,
                                                    const long lang,
                                                    void* const consumer,
                                                    bool& success);



    //---------------------------
    // Dispatch method
    //---------------------------
    // Perform dispatch
    DOSE_DLL_API void DoseC_Dispatch(const long ctrl,
                                     bool& success);

    // Interrupts current call to dispatch
    DOSE_DLL_API void DoseC_ExitDispatch(const long ctrl,
                                         bool& success);

    // Get the current callback id. callbackId is the ordinal of Safir::Dob::CallbackId enum
    DOSE_DLL_API void DoseC_GetCurrentCallbackId(const long ctrl,
                                                 DotsC_Int32& callbackId,
                                                 bool& success);

    //---------------------------
    // Message method
    //---------------------------
    DOSE_DLL_API void DoseC_SendMessage(const long ctrl,
                                        const char* const message,
                                        const DotsC_Int64 channelId,
                                        const char* const channelIdStr,
                                        const long lang,
                                        void* const consumer,
                                        bool& success);

    //----------------------------
    // Request methods
    //----------------------------
    DOSE_DLL_API void DoseC_ServiceRequest(const long ctrl,
                                           const char* const request,
                                           const DotsC_Int64 handlerId,
                                           const char* const handlerIdStr,
                                           const long lang,
                                           void* const consumer,
                                           DoseC_RequestId& reqId,
                                           bool& success);

    DOSE_DLL_API void DoseC_CreateRequest(const long ctrl,
                                          char const* const request,
                                          const bool hasInstanceId,
                                          const DotsC_Int64 instanceId,
                                          const char* const instanceIdStr,
                                          const DotsC_Int64 handlerId,
                                          const char* const handlerIdStr,
                                          const long lang,
                                          void* const consumer,
                                          DoseC_RequestId& reqId,
                                          bool& success);

    DOSE_DLL_API void DoseC_UpdateRequest(const long ctrl,
                                          char const* const request,
                                          const DotsC_Int64 instanceId,
                                          const char* const instanceIdStr,
                                          const long lang,
                                          void* const consumer,
                                          DoseC_RequestId& reqId,
                                          bool& success);

    DOSE_DLL_API void DoseC_DeleteRequest(const long ctrl,
                                          const DotsC_TypeId typeId,
                                          const DotsC_Int64 instanceId,
                                          const char* const instanceIdStr,
                                          const long lang,
                                          void* const consumer,
                                          DoseC_RequestId& reqId,
                                          bool& success);

    //---------------------------
    // Response method
    //---------------------------
    // Send Response
    DOSE_DLL_API void DoseC_SendResponse(const long ctrl,
                                         const char * const blob,
                                         void * const consumer,
                                         const long lang,
                                         const DotsC_Int32 responseId,
                                         bool& success);

    //----------------------------
    // Entity methods
    //----------------------------
    //Change flags will be set to false on SetEntity
    DOSE_DLL_API void DoseC_SetEntity(const long ctrl,
                                      const char* const blob,
                                      const DotsC_Int64 instanceId,
                                      const char* const instanceIdStr,
                                      const DotsC_Int64 handlerId,
                                      const char* const handlerIdStr,
                                      const bool considerChangeFlags,
                                      const bool initialInjection,
                                      bool& success);

    DOSE_DLL_API void DoseC_DeleteEntity(const long ctrl,
                                         const DotsC_TypeId typeId,
                                         const DotsC_Int64 instanceId,
                                         const char* const instanceIdStr,
                                         const bool allInstances,
                                         const DotsC_Int64 handlerId,
                                         const char* const handlerIdStr,
                                         bool& success);

    DOSE_DLL_API void DoseC_InjectEntity(const long ctrl,
                                         const char* const blob,
                                         const DotsC_Int64 instanceId,
                                         const char* const instanceIdStr,
                                         const DotsC_Int64 handlerId,
                                         const char* const handlerIdStr,
                                         const DotsC_Int64 timestamp,
                                         bool& success);

    DOSE_DLL_API void DoseC_InjectDeletedEntity(const long ctrl,
                                                const DotsC_TypeId typeId,
                                                const DotsC_Int64 instanceId,
                                                const char* const instanceIdStr,
                                                const DotsC_Int64 handlerId,
                                                const char* const handlerIdStr,
                                                const DotsC_Int64 timestamp,
                                                bool& success);


    DOSE_DLL_API void DoseC_ReadEntity(const long ctrl,
                                       const DotsC_TypeId typeId,
                                       const DotsC_Int64 instanceId,
                                       const char* const instanceIdStr,
                                       const char*& currentBlob,
                                       const char*& currentState,
                                       bool& success);

    DOSE_DLL_API void DoseC_IsCreated(const long ctrl,
                                      const DotsC_TypeId typeId,
                                      const DotsC_Int64 instanceId,
                                      const char* const instanceIdStr,
                                      bool& isCreated,
                                      bool& success);

    DOSE_DLL_API void DoseC_GetNumberOfInstances(const long ctrl,
                                                 const DotsC_TypeId typeId,
                                                 const DotsC_Int64 handlerId,
                                                 const char* const handlerIdStr,
                                                 const bool includeSubclasses,
                                                 DotsC_Int64& numberOfInstances,
                                                 bool& success);

    DOSE_DLL_API void DoseC_GetInstanceIdPolicy(const long ctrl,
                                                const DotsC_TypeId typeId,
                                                const DotsC_Int64 handlerId,
                                                DotsC_EnumerationValue& instanceIdPolicy,
                                                bool& success);

    DOSE_DLL_API void DoseC_Postpone(const long ctrl,
                                     const bool redispatchCurrent,
                                     bool& success);

    DOSE_DLL_API void DoseC_ResumePostponed(const long ctrl,
                                            bool& success);

    DOSE_DLL_API void DoseC_IncompleteInjectionState(const long ctrl,
                                                     bool& success);

    DOSE_DLL_API void DoseC_GetChannelId(const char* const state,
                                         DotsC_Int64& channelId,
                                         bool& success);

    DOSE_DLL_API void DoseC_GetTypeId(const char* const state,
                                      DotsC_TypeId& typeId,
                                      bool& success);

    DOSE_DLL_API void DoseC_GetInstanceId(const char* const state,
                                          DotsC_Int64& instanceId,
                                          bool& success);

    DOSE_DLL_API void DoseC_GetHandlerId(const char* const state,
                                         DotsC_Int64& handlerId,
                                         bool& success);


    typedef void (* DoseC_BlobDeleter)(char * &);

    DOSE_DLL_API void DoseC_GetConnectionInfo(const char* const state,
                                              char*& blob,
                                              DoseC_BlobDeleter & deleter,
                                              bool& success);

    DOSE_DLL_API void DoseC_GetTopTimestamp(const char* const state,
                                            DotsC_Int64& timestamp,
                                            bool& success);

    DOSE_DLL_API void DoseC_GetMemberTimestamp(const char* const state,
                                               const DotsC_MemberIndex member,
                                               DotsC_Int64& timestamp,
                                               bool& success);

    DOSE_DLL_API void DoseC_GetQueueCapacity(const long ctrl,
                                             const DotsC_EnumerationValue queue,
                                             DotsC_Int32& queueCapacity,
                                             bool& success);

    DOSE_DLL_API void DoseC_GetQueueSize(const long ctrl,
                                         const DotsC_EnumerationValue queue,
                                         DotsC_Int32& queueSize,
                                         bool& success);

    /**
     * Compare two states and return a new blob with change flags set accordingly.
     *
     * @param wantsCurrent - if true it is the blob from currentState that will be returned
     *                       if false it is the blob from previousState that will be returned
     *                          (with the diff made in "reverse")
     * @param timestampDiff - Do a diff using the timestamps, not the actual data.
     */
    DOSE_DLL_API void DoseC_Diff(const char* const previousState,
                                 const char* const currentState,
                                 const bool wantCurrent,
                                 const bool timestampDiff,
                                 char* & diffBlob,
                                 DoseC_BlobDeleter & deleter,
                                 bool& success);

    DOSE_DLL_API void DoseC_AddReference(const char* const state);

    DOSE_DLL_API void DoseC_DropReference(const char* const state);


    //-------------------------------
    // EntityIterator functions
    //-------------------------------
    //destroy needs to be called even if end is true!
    DOSE_DLL_API void DoseC_EntityIteratorCreate(const long ctrl,
                                                 const DotsC_TypeId typeId,
                                                 const bool includeSubclasses,
                                                 DotsC_Int32& iteratorId,
                                                 bool& end,
                                                 bool& success);

    DOSE_DLL_API void DoseC_EntityIteratorDestroy(const long ctrl,
                                                  const DotsC_Int32 iteratorId);

    DOSE_DLL_API void DoseC_EntityIteratorCopy(const long ctrl,
                                               const DotsC_Int32 iteratorId,
                                               DotsC_Int32& iteratorIdCopy,
                                               bool& success);

    DOSE_DLL_API void DoseC_EntityIteratorIncrement(const long ctrl,
                                                    const DotsC_Int32 iteratorId,
                                                    bool& end,
                                                    bool& success);

    DOSE_DLL_API void DoseC_EntityIteratorDereference(const long ctrl,
                                                      const DotsC_Int32 iteratorId,
                                                      const char *& entityBlob,
                                                      const char *& entityState,
                                                      bool& success);

    DOSE_DLL_API void DoseC_EntityIteratorEqual(const long ctrl,
                                                const DotsC_Int32 first,
                                                const DotsC_Int32 second,
                                                bool& equal,
                                                bool& success);

    //-------------------------------
    // Debug and statistics
    //-------------------------------
    DOSE_DLL_API void DoseC_SimulateOverflows(const long ctrl,
                                              const bool inQueues,
                                              const bool outQueues,
                                              bool& success);

    //-----------
    // Context
    //-----------
    DOSE_DLL_API void DoseC_GetContext(const long ctrl,
                                       DotsC_Int32& context,
                                       bool& success);

#ifdef __cplusplus
}
#endif

#endif //_dose_h
