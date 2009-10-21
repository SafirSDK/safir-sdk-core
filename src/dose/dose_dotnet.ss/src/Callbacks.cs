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

using System;

namespace Safir.Dob
{
    /// <summary>
    /// Summary description for Callbacks.
    /// </summary>
    internal class Callbacks
    {
        //Callback delegates, declared here to protect them from the GC.
        internal static Interface.OnDispatchCb onDispatchCb = new Interface.OnDispatchCb(Callbacks.OnDispatch);
        internal static Interface.OnStopOrderCb onStopOrderCb = new Interface.OnStopOrderCb(Callbacks.OnStopOrder);
        internal static Interface.OnNewEntityCb onNewEntityCb = new Interface.OnNewEntityCb(Callbacks.OnNewEntity);
        internal static Interface.OnUpdatedEntityCb onUpdatedEntityCb = new Interface.OnUpdatedEntityCb(Callbacks.OnUpdatedEntity);
        internal static Interface.OnDeletedEntityCb onDeletedEntityCb = new Interface.OnDeletedEntityCb(Callbacks.OnDeletedEntity);
        internal static Interface.OnCreateRequestCb onCreateRequestCb = new Interface.OnCreateRequestCb(Callbacks.OnCreateRequest);
        internal static Interface.OnUpdateRequestCb onUpdateRequestCb = new Interface.OnUpdateRequestCb(Callbacks.OnUpdateRequest);
        internal static Interface.OnDeleteRequestCb onDeleteRequestCb = new Interface.OnDeleteRequestCb(Callbacks.OnDeleteRequest);
        internal static Interface.OnServiceRequestCb onServiceRequestCb = new Interface.OnServiceRequestCb(Callbacks.OnServiceRequest);
        internal static Interface.OnResponseCb onResponseCb = new Interface.OnResponseCb(Callbacks.OnResponse);
        internal static Interface.OnMessageCb onMessageCb = new Interface.OnMessageCb(Callbacks.OnMessage);
        internal static Interface.OnRegisteredCb onRegisteredCb = new Interface.OnRegisteredCb(Callbacks.OnRegistered);
        internal static Interface.OnUnregisteredCb onUnregisteredCb = new Interface.OnUnregisteredCb(Callbacks.OnUnregistered);
        internal static Interface.OnRevokedRegistrationCb onRevokedRegistrationCb = new Interface.OnRevokedRegistrationCb(Callbacks.OnRevokedRegistration);
        internal static Interface.OnCompletedRegistrationCb onCompletedRegistrationCb = new Interface.OnCompletedRegistrationCb(Callbacks.OnCompletedRegistration);
        internal static Interface.OnInjectedNewEntityCb onInjectedNewEntityCb = new Interface.OnInjectedNewEntityCb(Callbacks.OnInjectedNewEntity);
        internal static Interface.OnInjectedUpdatedEntityCb onInjectedUpdatedEntityCb = new Interface.OnInjectedUpdatedEntityCb(Callbacks.OnInjectedUpdatedEntity);
        internal static Interface.OnInjectedDeletedEntityCb onInjectedDeletedEntityCb = new Interface.OnInjectedDeletedEntityCb(Callbacks.OnInjectedDeletedEntity);
        internal static Interface.OnInitialInjectionsDoneCb onInitialInjectionsDoneCb = new Interface.OnInitialInjectionsDoneCb(Callbacks.OnInitialInjectionsDone);
        internal static Interface.OnNotRequestOverflowCb onNotRequestOverflowCb = new Interface.OnNotRequestOverflowCb(Callbacks.OnNotRequestOverflow);
        internal static Interface.OnNotMessageOverflowCb onNotMessageOverflowCb = new Interface.OnNotMessageOverflowCb(Callbacks.OnNotMessageOverflow);
        internal static Interface.OnDropReferenceCb onDropReferenceCb = new Interface.OnDropReferenceCb(Callbacks.OnDropReference);


        //-------------------------------------------------------------------------------------
        // Callback from Dose_Dll
        //-------------------------------------------------------------------------------------
        private static void OnDispatch(System.IntPtr dispatcher, out byte success)
        {
            success = Interface.ByteOf(false);
            try
            {
                Dispatcher cons =
                    (Dispatcher)ConsumerHandler.ToConsumer(dispatcher);
                cons.OnDoDispatch();
                success = Interface.ByteOf(true);
            }
            catch (System.Exception exc)
            {
                Typesystem.LibraryExceptions.Instance.Set(exc);
            }
        }

        private static void OnStopOrder(System.IntPtr connectionOwner, out byte success)
        {
            success = Interface.ByteOf(false);
            try
            {
                StopHandler cons =
                    (StopHandler)ConsumerHandler.ToConsumer(connectionOwner);
                cons.OnStopOrder();
                success = Interface.ByteOf(true);
            }
            catch (System.Exception exc)
            {
                Typesystem.LibraryExceptions.Instance.Set(exc);
            }
        }
        
        private static void OnNewEntity(System.IntPtr currentBlob,
                                        System.IntPtr currentState,
                                        System.IntPtr consumer,
                                        byte timestampDiff,
                                        out byte success)
        {
            success = Interface.ByteOf(false);
            try
            {
                using (EntityProxy proxy = new EntityProxy(currentBlob,
                                                            currentState,
                                                            System.IntPtr.Zero,
                                                            System.IntPtr.Zero, 
                                                            true,
                                                            Interface.BoolOf(timestampDiff)))
                {
                    ((EntitySubscriber)ConsumerHandler.ToConsumer(consumer)).OnNewEntity(proxy);
                }

                success = Interface.ByteOf(true);
            }
            catch (System.Exception exc)
            {
                Typesystem.LibraryExceptions.Instance.Set(exc);
            }            
        }

        private static void OnUpdatedEntity(System.IntPtr currentBlob,
                                            System.IntPtr currentState,
                                            System.IntPtr previousBlob,
                                            System.IntPtr previousState,
                                            System.IntPtr consumer,
                                            byte timestampDiff,
                                            out byte success)
        {
            success = Interface.ByteOf(false);
            try
            {
                using (EntityProxy proxy = new EntityProxy(currentBlob,
                                                           currentState,
                                                           previousBlob,
                                                           previousState,
                                                           true,
                                                           Interface.BoolOf(timestampDiff)))
                {
                    ((EntitySubscriber)ConsumerHandler.ToConsumer(consumer)).OnUpdatedEntity(proxy);
                }

                success = Interface.ByteOf(true);
            }
            catch (System.Exception exc)
            {
                Typesystem.LibraryExceptions.Instance.Set(exc);
            }
        }

        private static void OnDeletedEntity(System.IntPtr currentState,
                                            System.IntPtr previousBlob,
                                            System.IntPtr previousState,
                                            byte explicitlyDeleted,
                                            System.IntPtr consumer,
                                            byte timestampDiff,
                                            out byte success)
        {

            success = Interface.ByteOf(false);
            try
            {
                using (EntityProxy proxy = new EntityProxy(System.IntPtr.Zero,
                                                           currentState,
                                                           previousBlob,
                                                           previousState,
                                                           true,
                                                           Interface.BoolOf(timestampDiff)))
                {
                    ((EntitySubscriber)ConsumerHandler.ToConsumer(consumer)).
                        OnDeletedEntity(proxy,Interface.BoolOf(explicitlyDeleted));
                }

                success = Interface.ByteOf(true);
            }
            catch (System.Exception exc)
            {
                Typesystem.LibraryExceptions.Instance.Set(exc);
            }
        }

        private static void OnCreateRequest(System.IntPtr blob,
                                            System.IntPtr state,
                                            System.Int32 ctrl,
                                            System.Int32 responseId,
                                            System.IntPtr consumer,
                                            out byte success)
        {
            success = Interface.ByteOf(false);
            try
            {
                ResponseSender rs = new ResponseSender(ctrl, consumer, responseId);
                using (EntityRequestProxy proxy = new EntityRequestProxy(blob, state))
                {
                    ((EntityRequestBase)ConsumerHandler.ToConsumer(consumer)).OnCreateRequest(proxy, rs);
                }
                success = Interface.ByteOf(true);
            }
            catch (System.Exception exc)
            {
                Typesystem.LibraryExceptions.Instance.Set(exc);
            } 
        }

        private static void OnUpdateRequest(System.IntPtr blob,
                                            System.IntPtr state,
                                            System.Int32 ctrl,
                                            System.Int32 responseId,
                                            System.IntPtr consumer,
                                            out byte success)
        {
            success = Interface.ByteOf(false);
            try
            {
                ResponseSender rs = new ResponseSender(ctrl, consumer, responseId);
                using (EntityRequestProxy proxy = new EntityRequestProxy(blob, state))
                {
                    ((EntityRequestBase)ConsumerHandler.ToConsumer(consumer)).OnUpdateRequest(proxy, rs);
                }
                success = Interface.ByteOf(true);
            }
            catch (System.Exception exc)
            {
                Typesystem.LibraryExceptions.Instance.Set(exc);
            }
        }

        private static void OnDeleteRequest(System.IntPtr state,
                                            System.Int32 ctrl,
                                            System.Int32 responseId,
                                            System.IntPtr consumer,
                                            out byte success)
        {
            success = Interface.ByteOf(false);
            try
            {
                ResponseSender rs = new ResponseSender(ctrl, consumer, responseId);
                using (EntityRequestProxy proxy = new EntityRequestProxy(System.IntPtr.Zero, state))
                {
                    ((EntityRequestBase)ConsumerHandler.ToConsumer(consumer)).OnDeleteRequest(proxy, rs);
                }
                success = Interface.ByteOf(true);
            }
            catch (System.Exception exc)
            {
                Typesystem.LibraryExceptions.Instance.Set(exc);
            } 
        }

        private static void OnServiceRequest(System.IntPtr blob,
                                             System.IntPtr state,
                                             System.Int32 ctrl,
                                             System.Int32 responseId,
                                             System.IntPtr consumer,
                                             out byte success)
        {
            success = Interface.ByteOf(false);
            try
            {
                ResponseSender rs = new ResponseSender(ctrl, consumer, responseId);
                using (ServiceRequestProxy proxy = new ServiceRequestProxy(blob, state))
                {
                    ((ServiceRequestBase)ConsumerHandler.ToConsumer(consumer)).OnServiceRequest(proxy, rs);
                }
                success = Interface.ByteOf(true);
            }
            catch (System.Exception exc)
            {
                Typesystem.LibraryExceptions.Instance.Set(exc);
            }
        }

        private static void OnResponse(System.Int32 requestId,
                                       System.IntPtr responseBlob,
                                       System.IntPtr responseState,
                                       System.IntPtr requestBlob,
                                       System.IntPtr requestState,
                                       System.IntPtr consumer,
                                       out byte success)
        {
            success = Interface.ByteOf(false);
            try
            {
                using (ResponseProxy proxy = new ResponseProxy(requestId,responseBlob,responseState,requestBlob,requestState))
                {
                    Requestor requestor = (Requestor)ConsumerHandler.ToConsumer(consumer);
                    ConsumerHandler.Instance.DropReference(requestor);
                    requestor.OnResponse(proxy);
                }
                success = Interface.ByteOf(true);
            }
            catch (System.Exception exc)
            {
                Typesystem.LibraryExceptions.Instance.Set(exc);
            }
        }

        private static void OnMessage(System.IntPtr message,
                                      System.IntPtr state,
                                      System.IntPtr consumer,
                                      out byte success)
        {
            success = Interface.ByteOf(false);
            try
            {
                using (MessageProxy proxy = new MessageProxy(message,state))
                {
                    ((MessageSubscriber)ConsumerHandler.ToConsumer(consumer)).OnMessage(proxy);
                }
                success = Interface.ByteOf(true);
            }
            catch (System.Exception exc)
            {
                Typesystem.LibraryExceptions.Instance.Set(exc);
            }
        }

        private static void OnRegistered(System.Int64 typeId,
                                         System.Int64 handlerId,
                                         System.IntPtr handlerIdStr,
                                         System.IntPtr consumer,
                                         out byte success)
        {
            success = Interface.ByteOf(false);
            try
            {
                ((RegistrationSubscriber)ConsumerHandler.ToConsumer(consumer))
                    .OnRegistered(typeId,
                    new Safir.Dob.Typesystem.HandlerId(handlerId, Typesystem.Internal.InternalOperations.StringOf(handlerIdStr)));
                success = Interface.ByteOf(true);
            }
            catch (System.Exception exc)
            {
                Typesystem.LibraryExceptions.Instance.Set(exc);
            }
        }

        private static void OnUnregistered(System.Int64 typeId,
                                           System.Int64 handlerId,
                                           System.IntPtr handlerIdStr,
                                           System.IntPtr consumer,
                                           out byte success)
        {
            success = Interface.ByteOf(false);
            try
            {
                ((RegistrationSubscriber)ConsumerHandler.ToConsumer(consumer))
                    .OnUnregistered(typeId, new Safir.Dob.Typesystem.HandlerId(handlerId, Typesystem.Internal.InternalOperations.StringOf(handlerIdStr)));
                success = Interface.ByteOf(true);
            }
            catch (System.Exception exc)
            {
                Typesystem.LibraryExceptions.Instance.Set(exc);
            }
        }

        private static void OnRevokedRegistration(System.Int64 typeId,
                                                     System.Int64 handlerId,
                                                     System.IntPtr handlerIdStr,
                                                     System.IntPtr consumer,
                                                     out byte success)
        {
            success = Interface.ByteOf(false);
            try
            {
                ((RevokedRegistrationBase)ConsumerHandler.ToConsumer(consumer))
                    .OnRevokedRegistration(typeId, new Safir.Dob.Typesystem.HandlerId(handlerId,
                    Typesystem.Internal.InternalOperations.StringOf(handlerIdStr)));
                success = Interface.ByteOf(true);
            }
            catch (System.Exception exc)
            {
                Typesystem.LibraryExceptions.Instance.Set(exc);
            }
        }


        private static void OnCompletedRegistration(System.Int64 typeId,
                                                    System.Int64 handlerId,
                                                    System.IntPtr handlerIdStr,
                                                    System.IntPtr consumer,
                                                    out byte success)
        {
            success = Interface.ByteOf(false);
            try
            {
                ((CompletedRegistrationBase)ConsumerHandler.ToConsumer(consumer))
                    .OnCompletedRegistration(typeId, new Safir.Dob.Typesystem.HandlerId(handlerId,
                    Typesystem.Internal.InternalOperations.StringOf(handlerIdStr)));
                success = Interface.ByteOf(true);
            }
            catch (System.Exception exc)
            {
                Typesystem.LibraryExceptions.Instance.Set(exc);
            }
        }

        private static void OnInjectedNewEntity(System.IntPtr injectionBlob,
                                                System.IntPtr injectionState,
                                                System.IntPtr consumer,
                                                out byte success)
        {
            success = Interface.ByteOf(false);
            try
            {
                using (InjectedEntityProxy proxy = new InjectedEntityProxy(injectionBlob,
                                                                           injectionState,
                                                                           System.IntPtr.Zero,
                                                                           System.IntPtr.Zero))
                {
                    ((EntityInjectionBase)ConsumerHandler.ToConsumer(consumer)).OnInjectedNewEntity(proxy);
                }

                success = Interface.ByteOf(true);
            }
            catch (System.Exception exc)
            {
                Typesystem.LibraryExceptions.Instance.Set(exc);
            }
        }

        private static void OnInjectedUpdatedEntity(System.IntPtr injectionBlob,
                                                    System.IntPtr injectionState,
                                                    System.IntPtr currentBlob,
                                                    System.IntPtr currentState,
                                                    System.IntPtr consumer,
                                                    out byte success)
        {
            success = Interface.ByteOf(false);
            try
            {
                using (InjectedEntityProxy proxy = new InjectedEntityProxy(injectionBlob,
                                                                           injectionState,
                                                                           currentBlob,
                                                                           currentState))
                {
                    ((EntityInjectionBase)ConsumerHandler.ToConsumer(consumer)).OnInjectedUpdatedEntity(proxy);
                }

                success = Interface.ByteOf(true);
            }
            catch (System.Exception exc)
            {
                Typesystem.LibraryExceptions.Instance.Set(exc);
            }
        }

        private static void OnInjectedDeletedEntity(System.IntPtr injectionState,
                                                    System.IntPtr currentBlob,
                                                    System.IntPtr currentState,
                                                    System.IntPtr consumer,
                                                    out byte success)
        {
            success = Interface.ByteOf(false);
            try
            {
                using (InjectedEntityProxy proxy = new InjectedEntityProxy(System.IntPtr.Zero,
                                                                           injectionState,
                                                                           currentBlob,
                                                                           currentState))
                {
                    ((EntityInjectionBase)ConsumerHandler.ToConsumer(consumer)).OnInjectedDeletedEntity(proxy);
                }

                success = Interface.ByteOf(true);
            }
            catch (System.Exception exc)
            {
                Typesystem.LibraryExceptions.Instance.Set(exc);
            }
        }

        private static void OnInitialInjectionsDone(System.Int64 typeId,
                                                    System.Int64 handlerId,
                                                    System.IntPtr handlerIdStr,
                                                    System.IntPtr consumer,
                                                    out byte success)
        {
            success = Interface.ByteOf(false);
            try
            {
                ((EntityInjectionBase)ConsumerHandler.ToConsumer(consumer)).
                    OnInitialInjectionsDone(typeId, new Safir.Dob.Typesystem.HandlerId
                        (handlerId, Typesystem.Internal.InternalOperations.StringOf(handlerIdStr)));

                success = Interface.ByteOf(true);
            }
            catch (System.Exception exc)
            {
                Typesystem.LibraryExceptions.Instance.Set(exc);
            }
        }

        private static void OnNotRequestOverflow(System.IntPtr requestor,
                                                 out byte success)
        {
            success = Interface.ByteOf(false);
            try
            {
                Requestor cons = (Requestor)ConsumerHandler.ToConsumer(requestor);
                ConsumerHandler.Instance.DropReference(cons);
                cons.OnNotRequestOverflow();
                success = Interface.ByteOf(true);
            }
            catch (System.Exception exc)
            {
                Typesystem.LibraryExceptions.Instance.Set(exc);
            }
        }

        private static void OnNotMessageOverflow(System.IntPtr messageSender,
                                                 out byte success)
        {
            success = Interface.ByteOf(false);
            try
            {
                MessageSender cons = (MessageSender)ConsumerHandler.ToConsumer(messageSender);
                ConsumerHandler.Instance.DropReference(cons);
                cons.OnNotMessageOverflow();
                success = Interface.ByteOf(true);
            }
            catch (System.Exception exc)
            {
                Typesystem.LibraryExceptions.Instance.Set(exc);
            }
        }

        private static void OnDropReference(System.IntPtr   consumer,
                                            System.Int32    refCounter,
                                            out byte        success)
        {
            success = Interface.ByteOf(false);
            try
            {
                Internal.ConsumerBase consumerBase = ConsumerHandler.ToConsumer(consumer);

                for (System.Int32 i = 0; i < refCounter; ++i)
                {
                    ConsumerHandler.Instance.DropReference(consumerBase);
                }
                success = Interface.ByteOf(true);
            }
            catch (System.Exception exc)
            {
                Typesystem.LibraryExceptions.Instance.Set(exc);
            }
        }
    }
}
