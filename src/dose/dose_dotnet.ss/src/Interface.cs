/* ****************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
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
using System;
using System.Runtime.InteropServices;

namespace Safir.Dob
{
    /// <summary>
    /// Interface function imports from dose_dll (see dose_dll Interface.h).
    /// </summary>
    internal class Interface
    {
        internal const System.Int32 DOSE_LANGUAGE_DOTNET = 2;

        internal const string DOSE_DLL_NAME = "dose_dll.dll";

        //---------------------------------------------------------------------
        // Delegates - using callingconvetion Cdecl instead of default stdcall
        //---------------------------------------------------------------------

        #region dose_dll callbacks

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        internal delegate void OnDispatchCb(System.IntPtr consumer,
                                            out byte success);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        internal delegate void OnStopOrderCb(System.IntPtr consumer,
                                             out byte success);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        internal delegate void OnNewEntityCb(System.IntPtr currentBlob,
                                             System.IntPtr currentState,
                                             System.IntPtr consumer,
                                             byte timestampDiff,
                                             out byte success);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        internal delegate void OnUpdatedEntityCb(System.IntPtr currentBlob,
                                                 System.IntPtr currentState,
                                                 System.IntPtr previousBlob,
                                                 System.IntPtr previousState,
                                                 System.IntPtr consumer,
                                                 byte timestampDiff,
                                                 out byte success);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        internal delegate void OnDeletedEntityCb(System.IntPtr currentState,
                                                 System.IntPtr previousBlob,
                                                 System.IntPtr previousState,
                                                 byte explicitlyDeleted,
                                                 System.IntPtr consumer,
                                                 byte timestampDiff,
                                                 out byte success);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        internal delegate void OnCreateRequestCb(System.IntPtr blob,
                                                 System.IntPtr state,
                                                 System.Int32 ctrl,
                                                 System.Int32 responseId,
                                                 System.IntPtr consumer,
                                                 out byte success);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        internal delegate void OnUpdateRequestCb(System.IntPtr blob,
                                                 System.IntPtr state,
                                                 System.Int32 ctrl,
                                                 System.Int32 responseId,
                                                 System.IntPtr consumer,
                                                 out byte success);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        internal delegate void OnDeleteRequestCb(System.IntPtr state,
                                                 System.Int32 ctrl,
                                                 System.Int32 responseId,
                                                 System.IntPtr consumer,
                                                 out byte success);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        internal delegate void OnServiceRequestCb(System.IntPtr blob,
                                                  System.IntPtr state,
                                                  System.Int32 ctrl,
                                                  System.Int32 responseId,
                                                  System.IntPtr consumer,
                                                  out byte success);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        internal delegate void OnResponseCb(System.Int32 requestId,
                                            System.IntPtr responseBlob,
                                            System.IntPtr responseState,
                                            System.IntPtr requestBlob,
                                            System.IntPtr requestState,
                                            System.IntPtr consumer,
                                            out byte success);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        internal delegate void OnMessageCb(System.IntPtr blob,
                                           System.IntPtr state,
                                           System.IntPtr consumer,
                                           out byte success);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        internal delegate void OnRegisteredCb(System.Int64 typeId,
                                              System.Int64 handlerId,
                                              System.IntPtr handlerIdStr,
                                              System.IntPtr consumer,
                                              out byte success);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        internal delegate void OnUnregisteredCb(System.Int64 typeId,
                                                System.Int64 handlerId,
                                                System.IntPtr handlerIdStr,
                                                System.IntPtr consumer,
                                                out byte success);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        internal delegate void OnRevokedRegistrationCb(System.Int64 typeId,
                                                     System.Int64 handlerId,
                                                     System.IntPtr handlerIdStr,
                                                     System.IntPtr consumer,
                                                     out byte success);


        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        internal delegate void OnCompletedRegistrationCb(System.Int64 typeId,
                                                         System.Int64 handlerId,
                                                         System.IntPtr handlerIdStr,
                                                         System.IntPtr consumer,
                                                         out byte success);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        internal delegate void OnInjectedNewEntityCb(System.IntPtr injectionBlob,
                                                     System.IntPtr injectionState,
                                                     System.IntPtr consumer,
                                                     out byte success);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        internal delegate void OnInjectedUpdatedEntityCb(System.IntPtr injectionBlob,
                                                         System.IntPtr injectionState,
                                                         System.IntPtr currentBlob,
                                                         System.IntPtr currentState,
                                                         System.IntPtr consumer,
                                                         out byte success);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        internal delegate void OnInjectedDeletedEntityCb(System.IntPtr injectionState,
                                                         System.IntPtr currentBlob,
                                                         System.IntPtr currentState,
                                                         System.IntPtr consumer,
                                                         out byte success);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        internal delegate void OnInitialInjectionsDoneCb(System.Int64 typeId,
                                                         System.Int64 handlerId,
                                                         System.IntPtr handlerIdStr,
                                                         System.IntPtr consumer,
                                                         out byte success);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        internal delegate void OnNotRequestOverflowCb(System.IntPtr consumer,
                                                      out byte success);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        internal delegate void OnNotMessageOverflowCb(System.IntPtr consumer,
                                                      out byte success);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        internal delegate void OnDropReferenceCb(System.IntPtr consumer,
                                                 System.Int32 refCounter,
                                                 out byte success);

        #endregion

        //--------------------------------------------------------------------
        // Exported interface from dose_internal.h (unmanaged code)
        //--------------------------------------------------------------------

        #region dose_dll functions
        //DoseC_Constructor
        [DllImport(DOSE_DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DoseC_Constructor(out System.Int32 ctrl,
                                                      out byte success);

        //DoseC_Destructor
        [DllImport(DOSE_DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DoseC_Destructor(System.Int32 ctrl);

        //DoseC_IsConnected

        [DllImport(DOSE_DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DoseC_IsConnected(System.Int32 ctrl,
                                                      out byte isConn,
                                                      out byte success);

        //DoseC_ConnectCb
        [DllImport(DOSE_DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DoseC_Connect
            (System.Int32 ctrl,
             System.IntPtr connectionNameCommonPart,
             System.IntPtr connectionNameInstancePart,
             System.Int32 context,
             System.Int32 lang,
             System.IntPtr connectionOwner,
             System.IntPtr dispatcher,
             OnDispatchCb onDispatchCb,
             OnStopOrderCb onStopOrderCb,
             OnNewEntityCb onNewEntityCb,
             OnUpdatedEntityCb onUpdatedEntityCb,
             OnDeletedEntityCb onDeletedEntityCb,
             OnCreateRequestCb onCreateRequestCb,
             OnUpdateRequestCb onUpdateRequestCb,
             OnDeleteRequestCb onDeleteRequestCb,
             OnServiceRequestCb onServiceRequestCb,
             OnResponseCb onResponseCb,
             OnMessageCb onMessageCb,
             OnRegisteredCb onRegisteredCb,
             OnUnregisteredCb onUnregisteredCb,
             OnRevokedRegistrationCb onRevokedRegistrationCb,
             OnCompletedRegistrationCb onCompletedRegistrationCb,
             OnInjectedNewEntityCb onInjectedNewEntityCb,
             OnInjectedUpdatedEntityCb onInjectedUpdatedEntityCb,
             OnInjectedDeletedEntityCb onInjectedDeletedEntityCb,
             OnInitialInjectionsDoneCb onInitialInjectionsDoneCb,
             OnNotRequestOverflowCb onNotRequestOverflowCb,
             OnNotMessageOverflowCb onNotMessageOverflowCb,
             OnDropReferenceCb onDropReferenceCb,
             out byte success);

        //DoseC_ConnectSecondary
        [DllImport(DOSE_DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DoseC_ConnectSecondary
            (System.IntPtr connectionNameCommonPart,
             System.IntPtr connectionNameInstancePart,
             System.Int32 lang,
             OnNewEntityCb onNewEntityCb,
             OnUpdatedEntityCb onUpdatedEntityCb,
             OnDeletedEntityCb onRemovedEntityCb,
             OnCreateRequestCb onCreateRequestCb,
             OnUpdateRequestCb onUpdateRequestCb,
             OnDeleteRequestCb onDeleteRequestCb,
             OnServiceRequestCb onServiceRequestCb,
             OnResponseCb onResponseCb,
             OnMessageCb onMessageCb,
             OnRegisteredCb onRegisteredCb,
             OnUnregisteredCb onUnregisteredCb,
             OnRevokedRegistrationCb onRevokedRegistrationCb,
             OnCompletedRegistrationCb onCompletedRegistrationCb,
             OnInjectedNewEntityCb onInjectedNewEntityCb,
             OnInjectedUpdatedEntityCb onInjectedUpdatedEntityCb,
             OnInjectedDeletedEntityCb onInjectedDeletedEntityCb,
             OnInitialInjectionsDoneCb onInitialInjectionsDoneCb,
             OnNotRequestOverflowCb onNotRequestOverflowCb,
             OnNotMessageOverflowCb onNotMessageOverflowCb,
             OnDropReferenceCb onDropReferenceCb,
             out System.Int32 newCtrlId,
             out byte success);

        //DoseC_Disconnect
        [DllImport(DOSE_DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DoseC_Disconnect(System.Int32 ctrl,
                                                     byte checkThread,
                                                     out byte success);

        //DoseC_GetConnectionName
        [DllImport(DOSE_DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DoseC_GetConnectionName(System.Int32 ctrl,
                                                            out System.IntPtr buf,
                                                            out byte success);

        //DoseC_GetConnectionNameCommonPart
        [DllImport(DOSE_DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DoseC_GetConnectionNameCommonPart(System.Int32 ctrl,
                                                                      out System.IntPtr buf,
                                                                      out byte success);

        //DoseC_GetConnectionNameInstancePart
        [DllImport(DOSE_DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DoseC_GetConnectionNameInstancePart(System.Int32 ctrl,
                                                                        out System.IntPtr buf,
                                                                        out byte success);

        //DoseC_RegisterServiceHandler
        [DllImport(DOSE_DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DoseC_RegisterServiceHandler(System.Int32 ctrl,
                                                                 System.Int64 typeId,
                                                                 System.Int64 handlerId,
                                                                 System.IntPtr handlerIdStr,
                                                                 byte overrideRegistration,
                                                                 System.Int32 lang,
                                                                 System.IntPtr consumer,
                                                                 out byte success);

        //DoseC_RegisterEntityHandler
        [DllImport(DOSE_DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DoseC_RegisterEntityHandler(System.Int32 ctrl,
                                                                System.Int64 typeId,
                                                                System.Int64 handlerId,
                                                                System.IntPtr handlerIdStr,
                                                                System.Int32 instanceIdPolicy,
                                                                byte overrideRegistration,
                                                                byte injectionHandler,
                                                                System.Int32 lang,
                                                                System.IntPtr consumer,
                                                                out byte success);

        //DoseC_UnregisterHandler
        [DllImport(DOSE_DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DoseC_UnregisterHandler(System.Int32 ctrl,
                                                            System.Int64 typeId,
                                                            System.Int64 handlerId,
                                                            System.IntPtr handlerIdStr,
                                                            out byte success);

        //DoseC_SubscribeMessage
        [DllImport(DOSE_DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DoseC_SubscribeMessage(System.Int32 ctrl,
                                                           System.Int64 typeId,
                                                           System.Int64 channelId,
                                                           System.IntPtr channelIdStr,
                                                           byte includeSubclasses,
                                                           System.Int32 lang,
                                                           System.IntPtr consumer,
                                                           out byte success);

        //DoseC_UnsubscribeMessage
        [DllImport(DOSE_DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DoseC_UnsubscribeMessage(System.Int32 ctrl,
                                                             System.Int64 typeId,
                                                             System.Int64 channelId,
                                                             System.IntPtr channelIdStr,
                                                             byte includeSubclasses,
                                                             System.Int32 lang,
                                                             System.IntPtr consumer,
                                                             out byte success);

        //DoseC_SubscribeEntity
        [DllImport(DOSE_DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DoseC_SubscribeEntity(System.Int32 ctrl,
                                                          System.Int64 typeId,
                                                          System.Int64 instanceId,
                                                          System.IntPtr instanceIdStr,
                                                          byte allInstances,
                                                          byte includeUpdates,
                                                          byte includeSubclasses,
                                                          byte restartSubscription,
                                                          System.Int32 lang,
                                                          System.IntPtr consumer,
                                                          out byte success);

        //DoseC_InjectorSubscribeEntity
        [DllImport(DOSE_DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DoseC_InjectorSubscribeEntity(System.Int32 ctrl,
                                                                  System.Int64 typeId,
                                                                  byte includeUpdates,
                                                                  byte includeSubclasses,
                                                                  byte restartSubscription,
                                                                  byte wantsGhostDelete,
                                                                  byte wantsLastState,
                                                                  byte doesntWantSourceIsPermanentStore,
                                                                  byte wantsAllStateChanges,
                                                                  byte timestampChangeInfo,
                                                                  System.Int32 lang,
                                                                  System.IntPtr consumer,
                                                                  out byte success);

        //DoseC_UnsubscribeEntity
        [DllImport(DOSE_DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DoseC_UnsubscribeEntity(System.Int32 ctrl,
                                                            System.Int64 typeId,
                                                            System.Int64 instanceId,
                                                            System.IntPtr instanceIdStr,
                                                            byte allInstances,
                                                            byte includeSubclasses,
                                                            System.Int32 lang,
                                                            System.IntPtr consumer,
                                                            out byte success);

        //DoseC_SubscribeRegistration
        [DllImport(DOSE_DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DoseC_SubscribeRegistration(System.Int32 ctrl,
                                                                System.Int64 typeId,
                                                                System.Int64 handlerId,
                                                                System.IntPtr handlerIdStr,
                                                                byte includeSubclasses,
                                                                byte restartSubscription,
                                                                System.Int32 lang,
                                                                System.IntPtr consumer,
                                                                out byte success);

        //DoseC_UnsubscribeRegistration
        [DllImport(DOSE_DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DoseC_UnsubscribeRegistration(System.Int32 ctrl,
                                                                  System.Int64 typeId,
                                                                  System.Int64 handlerId,
                                                                  System.IntPtr handlerIdStr,
                                                                  byte includeSubclasses,
                                                                  System.Int32 lang,
                                                                  System.IntPtr consumer,
                                                                  out byte success);

        //DoseC_Dispatch
        [DllImport(DOSE_DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DoseC_Dispatch(System.Int32 ctrl,
                                                   out byte success);

        //DoseC_ExitDispatch
        [DllImport(DOSE_DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DoseC_ExitDispatch(System.Int32 ctrl,
                                                       out byte success);

        //DoseC_GetCurrentCallbackId
        [DllImport(DOSE_DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DoseC_GetCurrentCallbackId(System.Int32 ctrl,
                                                               out System.Int32 callbackId,
                                                               out byte success);


        //DoseC_SendMessage
        [DllImport(DOSE_DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DoseC_SendMessage(System.Int32 ctrl,
                                                      System.IntPtr message,
                                                      System.Int64 channelId,
                                                      System.IntPtr channelIdStr,
                                                      System.Int32 lang,
                                                      System.IntPtr consumer,
                                                      out byte success);

        //DoseC_ServiceRequest
        [DllImport(DOSE_DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DoseC_ServiceRequest(System.Int32 ctrl,
                                                         System.IntPtr request,
                                                         System.Int64 handlerId,
                                                         System.IntPtr handlerIdStr,
                                                         System.Int32 lang,
                                                         System.IntPtr consumer,
                                                         out System.Int32 reqId,
                                                         out byte success);

        //DoseC_CreateRequest
        [DllImport(DOSE_DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DoseC_CreateRequest(System.Int32 ctrl,
                                                        System.IntPtr request,
                                                        byte hasInstanceId,
                                                        System.Int64 instanceId,
                                                        System.IntPtr instanceIdStr,
                                                        System.Int64 handlerId,
                                                        System.IntPtr handlerIdStr,
                                                        System.Int32 lang,
                                                        System.IntPtr consumer,
                                                        out System.Int32 reqId,
                                                        out byte success);

        //DoseC_UpdateRequest
        [DllImport(DOSE_DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DoseC_UpdateRequest(System.Int32 ctrl,
                                                        System.IntPtr request,
                                                        System.Int64 instanceId,
                                                        System.IntPtr instanceIdStr,
                                                        System.Int32 lang,
                                                        System.IntPtr consumer,
                                                        out System.Int32 reqId,
                                                        out byte success);

        //DoseC_DeleteRequest
        [DllImport(DOSE_DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DoseC_DeleteRequest(System.Int32 ctrl,
                                                        System.Int64 typeId,
                                                        System.Int64 instanceId,
                                                        System.IntPtr instanceIdStr,
                                                        System.Int32 lang,
                                                        System.IntPtr consumer,
                                                        out System.Int32 reqId,
                                                        out byte success);


        //DoseC_SendResponse
        [DllImport(DOSE_DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DoseC_SendResponse(System.Int32 ctrl,
                                                       System.IntPtr blob,
                                                       System.IntPtr consumer,
                                                       System.Int32 lang,
                                                       System.Int32 responseId,
                                                       out byte success);
        //DoseC_SetEntity
        [DllImport(DOSE_DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DoseC_SetEntity(System.Int32 ctrl,
                                                    System.IntPtr entity,
                                                    System.Int64 instanceId,
                                                    System.IntPtr instanceIdStr,
                                                    System.Int64 handlerId,
                                                    System.IntPtr handlerIdStr,
                                                    byte considerChangeFlags,
                                                    byte initialInjection,
                                                    out byte success);

        //DoseC_DeleteEntity
        [DllImport(DOSE_DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DoseC_DeleteEntity(System.Int32 ctrl,
                                                       System.Int64 typeId,
                                                       System.Int64 instanceId,
                                                       System.IntPtr instanceIdStr,
                                                       byte allInstances,
                                                       System.Int64 handlerId,
                                                       System.IntPtr handlerIdStr,
                                                       out byte success);

        //DoseC_InjectEntity
        [DllImport(DOSE_DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DoseC_InjectEntity(System.Int32 ctrl,
                                                       System.IntPtr entity,
                                                       System.Int64 instanceId,
                                                       System.IntPtr instanceIdStr,
                                                       System.Int64 handlerId,
                                                       System.IntPtr handlerIdStr,
                                                       System.Int64 timestamp,
                                                       out byte success);

        //DoseC_InjectDeletedEntity
        [DllImport(DOSE_DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DoseC_InjectDeletedEntity(System.Int32 ctrl,
                                                              System.Int64 typeId,
                                                              System.Int64 instanceId,
                                                              System.IntPtr instanceIdStr,
                                                              System.Int64 handlerId,
                                                              System.IntPtr handlerIdStr,
                                                              System.Int64 timestamp,
                                                              out byte success);

        //DoseC_ReadEntity
        [DllImport(DOSE_DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void DoseC_ReadEntity(System.Int32 ctrl,
                                                     System.Int64 typeId,
                                                     System.Int64 instanceId,
                                                     System.IntPtr instanceIdStr,
                                                     out System.IntPtr currentBlob,
                                                     out System.IntPtr currentState,
                                                     out byte success);

        //DoseC_IsCreated
        [DllImport(DOSE_DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DoseC_IsCreated(System.Int32 ctrl,
                                                    System.Int64 typeId,
                                                    System.Int64 instanceId,
                                                    System.IntPtr instanceIdStr,
                                                    out byte isCreated,
                                                    out byte success);

        //DoseC_GetNumberOfInstances
        [DllImport(DOSE_DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DoseC_GetNumberOfInstances(System.Int32 ctrl,
                                                               System.Int64 typeId,
                                                               System.Int64 handlerId,
                                                               System.IntPtr handlerIdStr,
                                                               bool includeSubclasses,
                                                               out System.Int64 numberOfInstances,
                                                               out byte success);

        //DoseC_GetInstanceIdPolicy
        [DllImport(DOSE_DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DoseC_GetInstanceIdPolicy(System.Int32 ctrl,
                                                               System.Int64 typeId,
                                                               System.Int64 handlerId,
                                                               out System.Int32 instanceIdPolicy,
                                                               out byte success);

        //DoseC_Postpone
        [DllImport(DOSE_DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DoseC_Postpone(System.Int32 ctrl,
                                                   byte redispatchCurrent,
                                                   out byte success);

        //DoseC_ResumePostponed
        [DllImport(DOSE_DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DoseC_ResumePostponed(System.Int32 ctrl,
                                                          out byte success);

        //DoseC_IncompleteInjectionState
        [DllImport(DOSE_DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DoseC_IncompleteInjectionState(System.Int32 ctrl,
                                                                   out byte success);

        //DoseC_GetChannelId
        [DllImport(DOSE_DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DoseC_GetChannelId(System.IntPtr state,
                                                       out System.Int64 channelId,
                                                       out byte success);
        //DoseC_GetTypeId
        [DllImport(DOSE_DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DoseC_GetTypeId(System.IntPtr state,
                                                    out System.Int64 typeId,
                                                    out byte success);

        //DoseC_GetInstanceId
        [DllImport(DOSE_DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DoseC_GetInstanceId(System.IntPtr state,
                                                        out System.Int64 instanceId,
                                                        out byte success);

        //DoseC_GetHandlerId
        [DllImport(DOSE_DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DoseC_GetHandlerId(System.IntPtr state,
                                                       out System.Int64 handlerId,
                                                       out byte success);
                //TODO: re-add this when MONO has fixed their bug. See
#if FUNC_PTR_WORKAROUND
        //DoseC_GetConnectionInfo
        [DllImport(DOSE_DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DoseC_GetConnectionInfo(System.IntPtr state,
                                                            out System.IntPtr blob,
                                                            out System.IntPtr dummy,
                                                            out byte success);
#else
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        internal delegate void DoseC_BlobDeleter(ref System.IntPtr ptr);

        //DoseC_GetConnectionInfo
        [DllImport(DOSE_DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DoseC_GetConnectionInfo(System.IntPtr state,
                                                            out System.IntPtr blob,
                                                            out DoseC_BlobDeleter deleter,
                                                            out byte success);
#endif

        //DoseC_GetTopTimestamp
        [DllImport(DOSE_DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DoseC_GetTopTimestamp(System.IntPtr state,
                                                          out System.Int64 timestamp,
                                                          out byte success);

        //DoseC_GetMemberTimestamp
        [DllImport(DOSE_DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DoseC_GetMemberTimestamp(System.IntPtr state,
                                                             System.Int32 member,
                                                             out System.Int64 timestamp,
                                                             out byte success);

        //DoseC_GetQueueCapacity
        [DllImport(DOSE_DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DoseC_GetQueueCapacity(System.Int32 ctrl,
                                                           System.Int32 queue,
                                                           out System.Int32 queueCapacity,
                                                           out byte success);
        //DoseC_GetQueueSize
        [DllImport(DOSE_DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DoseC_GetQueueSize(System.Int32 ctrl,
                                                       System.Int32 queue,
                                                       out System.Int32 queueSize,
                                                       out byte success);

#if FUNC_PTR_WORKAROUND
        //DoseC_Diff
        [DllImport(DOSE_DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DoseC_Diff(System.IntPtr previousState,
                                               System.IntPtr currentState,
                                               byte wantCurrent,
                                               byte timestampDiff,
                                               out System.IntPtr diffBlob,
                                               out System.IntPtr dummy,
                                               out byte success);
#else
        //DoseC_Diff
        [DllImport(DOSE_DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DoseC_Diff(System.IntPtr previousState,
                                               System.IntPtr currentState,
                                               byte wantCurrent,
                                               byte timestampDiff,
                                               out System.IntPtr diffBlob,
                                               out DoseC_BlobDeleter deleter,
                                               out byte success);
#endif


        //DoseC_AddReference
        [DllImport(DOSE_DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DoseC_AddReference(System.IntPtr state);


        //DoseC_DropReference
        [DllImport(DOSE_DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DoseC_DropReference(System.IntPtr state);


        //DoseC_EntityIteratorCreate
        [DllImport(DOSE_DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DoseC_EntityIteratorCreate(System.Int32 ctrl,
                                                               System.Int64 typeId,
                                                               byte includeSubclasses,
                                                               out System.Int32 iteratorId,
                                                               out byte end,
                                                               out byte success);

        //DoseC_EntityIteratorDestroy
        [DllImport(DOSE_DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DoseC_EntityIteratorDestroy(System.Int32 ctrl,
                                                                System.Int32 iteratorId);

        //DoseC_EntityIteratorIncrement
        [DllImport(DOSE_DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DoseC_EntityIteratorIncrement(System.Int32 ctrl,
                                                                  System.Int32 iteratorId,
                                                                  out byte end,
                                                                  out byte success);

        //DoseC_EntityIteratorDereference
        [DllImport(DOSE_DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DoseC_EntityIteratorDereference(System.Int32 ctrl,
                                                                    System.Int32 iteratorId,
                                                                    out System.IntPtr entityBlob,
                                                                    out System.IntPtr entityState,
                                                                    out byte success);

        //DoseC_GetContext
        [DllImport(DOSE_DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DoseC_GetContext(System.Int32 ctrl,
                                                     out System.Int32 context,
                                                     out byte success);

        //DoseC_SetAlwaysOverflowFlag
        [DllImport(DOSE_DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DoseC_SimulateOverflows(System.Int32 ctrl,
                                                            byte inQueues,
                                                            byte outQueues,
                                                            out byte success);


        #endregion

        #region Helpers
        internal static bool BoolOf(byte b)
        {
            return b != 0;
        }

        internal static byte ByteOf(bool b)
        {
            if (b)
                return 1;
            else
                return 0;
        }

        #endregion
    }

}
