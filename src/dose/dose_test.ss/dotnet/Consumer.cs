/******************************************************************************
*
* Copyright Saab AB, 2006-2008 (http://www.safirsdk.com)
* 
* Created by: Henrik Sundberg / sthesu
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
using System.Collections.Generic;
using System.Text;

namespace dose_test_dotnet
{
    class Consumer :
        Safir.Dob.MessageSubscriber,
        Safir.Dob.MessageSender,
        Safir.Dob.RegistrationSubscriber,
        Safir.Dob.EntitySubscriber,
        Safir.Dob.EntityHandler,
        Safir.Dob.EntityHandlerInjection,
        Safir.Dob.EntityHandlerPending,
        Safir.Dob.ServiceHandler,
        Safir.Dob.ServiceHandlerPending,
        Safir.Dob.Requestor
    {
        private const string PREFIX = "Consumer ";

        public Consumer(int consumerNumber, 
                        string connectionName, 
                        string instance)
        {
            m_consumerNumber = consumerNumber;
            m_callbackActions = new Dictionary<Safir.Dob.CallbackId.Enumeration, List<DoseTest.Action>>();
            foreach (Safir.Dob.CallbackId.Enumeration cb in Enum.GetValues(typeof(Safir.Dob.CallbackId.Enumeration)))
            {
                m_callbackActions.Add(cb, new List<DoseTest.Action>());
            }
            m_connection.Attach(connectionName,instance);
        }

        public void AddCallbackAction(DoseTest.Action action)
        {
            m_callbackActions[action.ActionCallback.Val].Add(action);
        }


        public void ExecuteCallbackActions(Safir.Dob.CallbackId.Enumeration callback)
        {
            foreach (DoseTest.Action action in m_callbackActions[callback])
            {
                ExecuteAction(action);
            }
        }



        private System.Int64 GetTimestamp(DoseTest.Action action)
        {
            Safir.Dob.Typesystem.EntityId entityId = new Safir.Dob.Typesystem.EntityId
               (DoseTest.LastInjectionTimestamp.ClassTypeId,
                new Safir.Dob.Typesystem.InstanceId(DoseTest.LastInjectionTimestamp.ClassTypeId));

            System.Int64 delta = action.TimestampDelta.Val;
            Safir.Dob.EntityProxy ep = m_connection.Read(entityId);

            DoseTest.LastInjectionTimestamp ent = (DoseTest.LastInjectionTimestamp)ep.Entity;

            System.Int64 newVal = ent.Timestamp.Val + delta;

            ent.Timestamp.Val = newVal;

            m_connection.UpdateRequest(ent, entityId.InstanceId, m_timestampRequestor);

            return newVal;
        }

        public void ExecuteAction(DoseTest.Action action)
        {
            try
            {
                //only becomes true if RepeatUntilOverflow is true
                bool repeat = !action.RepeatUntilOverflow.IsNull() && action.RepeatUntilOverflow.Val;

                do //while repeat
                {
                    try
                    {
                        switch (action.ActionType.Val)
                        {
                            case DoseTest.ActionEnum.Enumeration.SendResponse:
                                {
                                    Safir.Dob.Response resp = action.Object.Obj as Safir.Dob.Response;
                                    m_responseSender.Send(resp);
                                }
                                break;
                            case DoseTest.ActionEnum.Enumeration.DiscardResponseSender:
                                {
                                    m_responseSender.Discard();
                                }
                                break;
                            case DoseTest.ActionEnum.Enumeration.RegisterEntityHandler:
                                {
                                    m_connection.RegisterEntityHandler
                                        (action.TypeId.Val,
                                         action.Handler.Val,
                                         action.InstanceIdPolicy.Val,
                                         this);

                                    //save the instance id policy
                                    m_instanceIdPolicyMap[new KeyValuePair<long, Safir.Dob.Typesystem.HandlerId>(action.TypeId.Val, action.Handler.Val)] =
                                        new Pair(action.InstanceIdPolicy.Val, action.Handler.Val.RawValue);

                                }
                                break;

                            case DoseTest.ActionEnum.Enumeration.RegisterEntityHandlerInjection:
                                {
                                    m_connection.RegisterEntityHandlerInjection
                                        (action.TypeId.Val,
                                         action.Handler.Val,
                                         action.InstanceIdPolicy.Val,
                                         this);

                                    //save the instance id policy
                                    m_instanceIdPolicyMap[new KeyValuePair<long, Safir.Dob.Typesystem.HandlerId>(action.TypeId.Val, action.Handler.Val)] =
                                        new Pair(action.InstanceIdPolicy.Val, action.Handler.Val.RawValue);

                                }
                                break;


                            case DoseTest.ActionEnum.Enumeration.RegisterEntityHandlerPending:
                                {
                                    m_connection.RegisterEntityHandlerPending
                                        (action.TypeId.Val,
                                         action.Handler.Val,
                                         action.InstanceIdPolicy.Val,
                                         this);

                                    //save the instance id policy
                                    m_instanceIdPolicyMap[new KeyValuePair<long, Safir.Dob.Typesystem.HandlerId>(action.TypeId.Val, action.Handler.Val)] =
                                        new Pair(action.InstanceIdPolicy.Val, action.Handler.Val.RawValue);

                                }
                                break;

                            case DoseTest.ActionEnum.Enumeration.RegisterServiceHandler:
                                {
                                    m_connection.RegisterServiceHandler(action.TypeId.Val,
                                                                        action.Handler.Val,
                                                                        this);

                                }
                                break;

                            case DoseTest.ActionEnum.Enumeration.RegisterServiceHandlerPending:
                                {
                                    m_connection.RegisterServiceHandlerPending(action.TypeId.Val,
                                                                               action.Handler.Val,
                                                                               this);

                                }
                                break;

                            case DoseTest.ActionEnum.Enumeration.UnregisterHandler:
                                {
                                    m_connection.UnregisterHandler(action.TypeId.Val,
                                                                   action.Handler.Val);

                                }
                                break;
                            case DoseTest.ActionEnum.Enumeration.SubscribeMessage:
                                {
                                    m_connection.SubscribeMessage(action.TypeId.Val,
                                                                  action.Channel.Val,
                                                                  action.IncludeSubclasses.Val,
                                                                  this);

                                }
                                break;
                            case DoseTest.ActionEnum.Enumeration.UnsubscribeMessage:
                                {
                                    m_connection.UnsubscribeMessage(action.TypeId.Val,
                                                                    action.Channel.Val,
                                                                    action.IncludeSubclasses.Val,
                                                                    this);
                                }
                                break;

                            case DoseTest.ActionEnum.Enumeration.SubscribeEntity:
                                {
                                    if (!action.TypeId.IsNull())
                                    {
                                        m_connection.SubscribeEntity(action.TypeId.Val,
                                                                     action.IncludeUpdates.Val,
                                                                     action.IncludeSubclasses.Val,
                                                                     action.RestartSubscription.Val,
                                                                     this);
                                    }
                                    else
                                    {
                                        m_connection.SubscribeEntity(action.EntityId.Val,
                                                                     action.IncludeUpdates.Val,
                                                                     action.RestartSubscription.Val,
                                                                     this);
                                    }
                                }
                                break;

                            case DoseTest.ActionEnum.Enumeration.InjectorSubscribeEntity:
                                {
                                    new Safir.Dob.ConnectionAspectInjector(m_connection).SubscribeEntity
                                        (action.TypeId.Val,
                                         action.IncludeUpdates.Val,
                                         action.IncludeSubclasses.Val,
                                         action.RestartSubscription.Val,
                                         action.WantsGhostDelete.Val,
                                         action.WantsLastState.Val,
                                         action.DoesntWantSourceIsPermanentStore.Val,
                                         action.WantsAllStateChanges.Val,
                                         action.TimestampChangeInfo.Val,
                                         this);
                                }
                                break;

                            case DoseTest.ActionEnum.Enumeration.UnsubscribeEntity:
                                {
                                    if (action.TypeId.IsNull())
                                    {
                                        m_connection.UnsubscribeEntity(action.EntityId.Val, this);
                                    }
                                    else
                                    {
                                        m_connection.UnsubscribeEntity(action.TypeId.Val, action.IncludeSubclasses.Val,this);
                                    }
                                }
                                break;

                            case DoseTest.ActionEnum.Enumeration.SubscribeRegistration:
                                {
                                    m_connection.SubscribeRegistration(action.TypeId.Val,
                                        action.Handler.Val,
                                        action.IncludeSubclasses.Val,
                                        action.RestartSubscription.Val,
                                        this);
                                }
                                break;
                            case DoseTest.ActionEnum.Enumeration.UnsubscribeRegistration:
                                {
                                    m_connection.UnsubscribeRegistration(action.TypeId.Val,
                                        action.Handler.Val,
                                        action.IncludeSubclasses.Val,
                                        this);
                                }
                                break;
                            case DoseTest.ActionEnum.Enumeration.SendMessage:
                                {
                                    m_connection.Send(action.Object.Obj as Safir.Dob.Message,
                                        action.Channel.Val,
                                        this);
                                }
                                break;

                            case DoseTest.ActionEnum.Enumeration.ServiceRequest:
                                {
                                    m_connection.ServiceRequest
                                        (action.Object.Obj as Safir.Dob.Service,
                                         action.Handler.Val,
                                         this);
                                }
                                break;
                            case DoseTest.ActionEnum.Enumeration.CreateRequest:
                                {
                                    if (!action.Instance.IsNull())
                                    {
                                        m_connection.CreateRequest
                                            (action.Object.Obj as Safir.Dob.Entity,
                                             action.Instance.Val,
                                             action.Handler.Val,
                                             this);
                                    }
                                    else
                                    {
                                        m_connection.CreateRequest
                                           (action.Object.Obj as Safir.Dob.Entity,
                                            action.Handler.Val,
                                            this);
                                    }
                                }
                                break;
                            case DoseTest.ActionEnum.Enumeration.UpdateRequest:
                                {
                                    m_connection.UpdateRequest
                                        (action.Object.Obj as Safir.Dob.Entity,
                                         action.Instance.Val,
                                         this);
                                }
                                break;
                            case DoseTest.ActionEnum.Enumeration.DeleteRequest:
                                {
                                    m_connection.DeleteRequest
                                        (action.EntityId.Val,
                                         this);
                                }
                                break;

                            case DoseTest.ActionEnum.Enumeration.SetAll:
                                {
                                    m_connection.SetAll(action.Object.Obj as Safir.Dob.Entity, action.Instance.Val, action.Handler.Val);
                                }
                                break;

                            case DoseTest.ActionEnum.Enumeration.InitialSet:
                                {
                                    new Safir.Dob.ConnectionAspectInjector(m_connection).InitialSet
                                        (action.Object.Obj as Safir.Dob.Entity, action.Instance.Val, action.Handler.Val);
                                }
                                break;

                            case DoseTest.ActionEnum.Enumeration.SetChanges:
                                {
                                    m_connection.SetChanges(action.Object.Obj as Safir.Dob.Entity,
                                        action.Instance.Val, action.Handler.Val);
                                }
                                break;

                            case DoseTest.ActionEnum.Enumeration.InjectChanges:
                                {
                                    new Safir.Dob.ConnectionAspectInjector(m_connection).InjectChanges
                                        (action.Object.Obj as Safir.Dob.Entity,
                                         action.Instance.Val, 
                                         GetTimestamp(action),
                                         action.Handler.Val);
                                }
                                break;

                            case DoseTest.ActionEnum.Enumeration.Delete:
                                {
                                    m_connection.Delete(action.EntityId.Val, action.Handler.Val);
                                }
                                break;

                            case DoseTest.ActionEnum.Enumeration.InjectDelete:
                                {
                                    new Safir.Dob.ConnectionAspectInjector(m_connection).
                                        InjectDelete(action.EntityId.Val,
                                                     GetTimestamp(action),
                                                     action.Handler.Val);
                                }
                                break;

                            case DoseTest.ActionEnum.Enumeration.Postpone:
                                {
                                    new Safir.Dob.ConnectionAspectPostpone(m_connection).
                                        Postpone(action.RedispatchCurrent.Val);
                                }
                                break;

                            case DoseTest.ActionEnum.Enumeration.ResumePostponed:
                                {
                                    new Safir.Dob.ConnectionAspectPostpone(m_connection).
                                        ResumePostponed();
                                }
                                break;

                            case DoseTest.ActionEnum.Enumeration.IncompleteInjectionState:
                                {
                                    new Safir.Dob.ConnectionAspectPostpone(m_connection).
                                        IncompleteInjectionState();
                                }
                                break;

                            case DoseTest.ActionEnum.Enumeration.DeleteAllInstances:
                                {
                                    m_connection.DeleteAllInstances(action.TypeId.Val,action.Handler.Val);
                                }
                                break;

                            case DoseTest.ActionEnum.Enumeration.GetEntityIterator:
                                {
                                    Logger.Instance.WriteLine(PREFIX + m_consumerNumber + ": "
                                        + "Iterating over entities of type "
                                        + Safir.Dob.Typesystem.Operations.GetName(action.TypeId.Val)
                                        + ":");
                                    foreach (Safir.Dob.EntityProxy entityProxy in m_connection.GetEntityEnumerator(action.TypeId.Val,action.IncludeSubclasses.Val))
                                    {
                                        Logger.Instance.WriteLine("  EntityId  = " + entityProxy.EntityId + ":\n"
                                             + "     Owner     = " + entityProxy.Owner + "\n"
                                             + "     OwnerConn = " + ConnInfoToXml(entityProxy.OwnerConnectionInfo) + "\n"
                                             + "     OwnerStr  = " + entityProxy.OwnerWithStringRepresentation + "\n"
                                             + "     Entity    = " + Safir.Dob.Typesystem.Serialization.ToXml(entityProxy.Blob));

                                    }
                                    Logger.Instance.WriteLine();

                                }
                                break;

                            case DoseTest.ActionEnum.Enumeration.Read:
                                {
                                    Logger.Instance.WriteLine(PREFIX + m_consumerNumber + ": "
                                        + "Read entity "
                                        + action.EntityId.Val
                                        + ":");

                                    using (Safir.Dob.EntityProxy entityProxy = m_connection.Read(action.EntityId.Val))
                                    {
                                        Logger.Instance.WriteLine("  EntityId  = " + entityProxy.EntityId + ":\n"
                                                 + "  Owner     = " + entityProxy.Owner + "\n"
                                                 + "  OwnerConn = " + ConnInfoToXml(entityProxy.OwnerConnectionInfo) + "\n"
                                                 + "  OwnerStr  = " + entityProxy.OwnerWithStringRepresentation + "\n"
                                                 + "  Entity    = " + Safir.Dob.Typesystem.Serialization.ToXml(entityProxy.Blob));
                                        Logger.Instance.WriteLine();
                                    }
                                }
                                break;

                            case DoseTest.ActionEnum.Enumeration.SimulateOverflows:
                                {
                                    Logger.Instance.WriteLine(PREFIX + m_consumerNumber + ": "
                                         + "SimulateOverflows("
                                         + action.InQueues.Val.ToString().ToLower()+ ", "
                                         + action.OutQueues.Val.ToString().ToLower() + ")");

                                    new Safir.Dob.ConnectionAspectMisc(m_connection).SimulateOverflows(action.InQueues.Val,
                                        action.OutQueues.Val);
                                }
                                break;

                            case DoseTest.ActionEnum.Enumeration.IsCreated:
                                {
                                    Logger.Instance.WriteLine(PREFIX + m_consumerNumber + ": "
                                        + "The instance "
                                        + action.EntityId.Val
                                        + " is "
                                        + (m_connection.IsCreated(action.EntityId.Val) ? "" : "not ")
                                        + "created.");
                                }
                                break;

                            case DoseTest.ActionEnum.Enumeration.GetNumberOfInstances:
                                {
                                    Logger.Instance.WriteLine(PREFIX + m_consumerNumber + ": "
                                        + "GetNumberOfInstances (type = "
                                        + Safir.Dob.Typesystem.Operations.GetName(action.TypeId.Val)
                                        + ", handler = " + action.Handler.Val
                                        + ", includeSubclasses = " + action.IncludeSubclasses.Val.ToString().ToLower()
                                        + "): "
                                        + m_connection.GetNumberOfInstances(action.TypeId.Val,
                                                                             action.Handler.Val,
                                                                             action.IncludeSubclasses.Val));
                                }
                                break;

                            case DoseTest.ActionEnum.Enumeration.GetQueueCapacity:
                                {
                                    Logger.Instance.WriteLine(PREFIX + m_consumerNumber + ": "
                                        + "The capacity of "
                                        + action.ConnectionQueueId.Val
                                        + " is "
                                        + new Safir.Dob.ConnectionAspectMisc(m_connection).GetQueueCapacity(action.ConnectionQueueId.Val));
                                }
                                break;

                            case DoseTest.ActionEnum.Enumeration.GetQueueSize:
                                {
                                    Logger.Instance.WriteLine(PREFIX + m_consumerNumber + ": "
                                        + "The size of "
                                        + action.ConnectionQueueId.Val
                                        + " is "
                                        + new Safir.Dob.ConnectionAspectMisc(m_connection).GetQueueSize(action.ConnectionQueueId.Val));
                                }
                                break;

                            case DoseTest.ActionEnum.Enumeration.ResetCallbackActions:
                                {
                                    Logger.Instance.WriteLine(PREFIX + m_consumerNumber + ": ResetCallbackActions");

                                    foreach (KeyValuePair<Safir.Dob.CallbackId.Enumeration, List<DoseTest.Action>> cbActions
                                        in m_callbackActions)
                                    {
                                        cbActions.Value.Clear();
                                    }
                                }
                                break;

                            default:
                                Logger.Instance.WriteLine(PREFIX + m_consumerNumber + ": "
                                    + "No handler defined for action "
                                    + action.ActionType.Val);
                                break;
                        }
                    }
                    catch (Safir.Dob.OverflowException)
                    {
                        Logger.Instance.WriteLine("Caught Overflow exception");
                        repeat = false;
                    }
                }
                while (repeat);
            }
            catch (Safir.Dob.Typesystem.Exception exc)
            {
                Logger.Instance.WriteLine("Caught Exception in ExecuteAction: " + exc.GetType().Namespace + "." + exc.GetType().Name);
                System.Console.WriteLine("Exception info: " + exc);
            }
            catch (Safir.Dob.Typesystem.FundamentalException exc)
            {
                Logger.Instance.WriteLine("Caught FundamentalException in ExecuteAction: " + exc.GetType().Namespace + "." + exc.GetType().Name);
                System.Console.WriteLine("Exception info: " + exc);
            }
        }

        #region MessageSubscriber Members

        void Safir.Dob.MessageSubscriber.OnMessage(Safir.Dob.MessageProxy messageProxy)
        {
            m_connection.ExitDispatch();
            ExecuteCallbackActions(Safir.Dob.CallbackId.Enumeration.OnMessage);

            Logger.Instance.WriteLine(PREFIX + m_consumerNumber + ": "
                       + "OnMessage:\n"
                       + "  Type       = " + Safir.Dob.Typesystem.Operations.GetName(messageProxy.TypeId) + "\n"
                       + "  ChannelId  = " + messageProxy.ChannelId + "\n"
                       + "  Sender     = " + ConnInfoToXml(messageProxy.SenderConnectionInfo) + "\n"
                       + "  ChannelId  = " + messageProxy.ChannelIdWithStringRepresentation + "\n"
                       + "  Message    = " + Safir.Dob.Typesystem.Serialization.ToXml(messageProxy.Blob) + "\n\n");
        }

        #endregion

        #region EntitySubscriber Members

        void Safir.Dob.EntitySubscriber.OnNewEntity(Safir.Dob.EntityProxy entityProxy)
        {
            m_connection.ExitDispatch();
            ExecuteCallbackActions(Safir.Dob.CallbackId.Enumeration.OnNewEntity);

            Logger.Instance.WriteLine(PREFIX + m_consumerNumber + ": "
                    + "OnNewEntity:\n"
                    + "  EntityId  = " + entityProxy.EntityId + "\n"
                    + "  Owner     = " + entityProxy.Owner + "\n"
                    + "  OwnerConn = " + ConnInfoToXml(entityProxy.OwnerConnectionInfo) + "\n"
                    + "  OwnerStr  = " + entityProxy.OwnerWithStringRepresentation + "\n"
                    + "  Entity    = " + Safir.Dob.Typesystem.Serialization.ToXml(entityProxy.Blob) + "\n"
                    + "  Changed top-level members: ");

            Safir.Dob.Entity entity = entityProxy.EntityWithChangeInfo;
            for (int i = 0;
                 i < Safir.Dob.Typesystem.Members.GetNumberOfMembers(entity.GetTypeId());
                 ++i)
            {
                if (entity.GetMember(i, 0).IsChanged())
                {
                    Logger.Instance.WriteLine("    "
                               + Safir.Dob.Typesystem.Members.GetName(entity.GetTypeId(), i));
                }
            }
            Logger.Instance.WriteLine();
        }
        void Safir.Dob.EntitySubscriber.OnUpdatedEntity(Safir.Dob.EntityProxy entityProxy)
        {
            m_connection.ExitDispatch();
            ExecuteCallbackActions(Safir.Dob.CallbackId.Enumeration.OnUpdatedEntity);

            Logger.Instance.WriteLine(PREFIX + m_consumerNumber + ": "
                + "OnUpdatedEntity:\n"
                + "  EntityId  = " + entityProxy.EntityId + "\n"
                + "  Owner     = " + entityProxy.Owner + "\n"
                + "  OwnerConn = " + ConnInfoToXml(entityProxy.OwnerConnectionInfo) + "\n"
                + "  OwnerStr  = " + entityProxy.OwnerWithStringRepresentation + "\n"
                + "  Entity    = " + Safir.Dob.Typesystem.Serialization.ToXml(entityProxy.Blob) + "\n"
                + "  Previous  = " + Safir.Dob.Typesystem.Serialization.ToXml(entityProxy.Previous.Blob) + "\n"
                + "  Changed top-level members: ");

            Safir.Dob.Entity entity = entityProxy.EntityWithChangeInfo;
            for (int i = 0;
                 i < Safir.Dob.Typesystem.Members.GetNumberOfMembers(entity.GetTypeId());
                 ++i)
            {
                if (entity.GetMember(i, 0).IsChanged())
                {
                    Logger.Instance.WriteLine("    "
                               + Safir.Dob.Typesystem.Members.GetName(entity.GetTypeId(), i));
                }
            }
            Logger.Instance.WriteLine();
        }

        void Safir.Dob.EntitySubscriber.OnDeletedEntity(Safir.Dob.EntityProxy entityProxy, bool deletedByOwner)
        {
            m_connection.ExitDispatch();
            ExecuteCallbackActions(Safir.Dob.CallbackId.Enumeration.OnDeletedEntity);

            Logger.Instance.WriteLine(PREFIX + m_consumerNumber + ": "
                            + "OnDeletedEntity:\n"
                            + "  EntityId       = " + entityProxy.EntityId + "\n"
                            + "  deletedByOwner = " + deletedByOwner.ToString().ToLower() + "\n"
                            + "  Owner          = " + entityProxy.Owner + "\n"
                            + "  OwnerConn = " + ConnInfoToXml(entityProxy.OwnerConnectionInfo) + "\n"
                            + "  OwnerStr  = " + entityProxy.OwnerWithStringRepresentation + "\n"
                            + "  Previous  = " + Safir.Dob.Typesystem.Serialization.ToXml(entityProxy.Previous.Blob));

            Logger.Instance.WriteLine();
        }

#endregion


        #region RegistrationSubscriber Members

        void Safir.Dob.RegistrationSubscriber.OnRegistered(long typeId, Safir.Dob.Typesystem.HandlerId handlerId)
        {
            m_connection.ExitDispatch();
            ExecuteCallbackActions(Safir.Dob.CallbackId.Enumeration.OnRegistered);

            Logger.Instance.WriteLine(PREFIX + m_consumerNumber + ": "
                       + "OnRegistered:\n"
                       + "  Type      = " + Safir.Dob.Typesystem.Operations.GetName(typeId) +"\n"
                       + "  HandlerId = " + handlerId);
            
            Logger.Instance.WriteLine();
        }

        void Safir.Dob.RegistrationSubscriber.OnUnregistered(long typeId, Safir.Dob.Typesystem.HandlerId handlerId)
        {
            m_connection.ExitDispatch();
            ExecuteCallbackActions(Safir.Dob.CallbackId.Enumeration.OnUnregistered);

            Logger.Instance.WriteLine(PREFIX + m_consumerNumber + ": "
                       + "OnUnregistered:\n"
                       + "  Type      = " + Safir.Dob.Typesystem.Operations.GetName(typeId) + "\n"
                       + "  HandlerId = " + handlerId);

            Logger.Instance.WriteLine();
        }
        #endregion

        #region MessageSender Members

        void Safir.Dob.MessageSender.OnNotMessageOverflow()
        {
            m_connection.ExitDispatch();
            ExecuteCallbackActions(Safir.Dob.CallbackId.Enumeration.OnNotMessageOverflow);

            Logger.Instance.WriteLine(PREFIX + m_consumerNumber + ": " + "OnNotMessageOverflow");
            Logger.Instance.WriteLine();
        }
        
        #endregion


        #region RevokedRegistrationBase Members

        void Safir.Dob.RevokedRegistrationBase.OnRevokedRegistration(long typeId, Safir.Dob.Typesystem.HandlerId handlerId)
        {
            m_connection.ExitDispatch();
            ExecuteCallbackActions(Safir.Dob.CallbackId.Enumeration.OnRevokedRegistration);

            Logger.Instance.WriteLine
                (PREFIX + m_consumerNumber + ": "
                + "OnRevokedRegistration:\n"
                + "  Type      = " + Safir.Dob.Typesystem.Operations.GetName(typeId) + "\n"
                + "  HandlerId = " + handlerId);
            Logger.Instance.WriteLine();
        }

        #endregion

        #region ServiceRequestBase Members

        void Safir.Dob.ServiceRequestBase.OnServiceRequest(Safir.Dob.ServiceRequestProxy serviceRequestProxy,
                                                           Safir.Dob.ResponseSender responseSender)
        {
            m_connection.ExitDispatch();
            m_responseSender = responseSender;
            ExecuteCallbackActions(Safir.Dob.CallbackId.Enumeration.OnServiceRequest);

            Logger.Instance.WriteLine(PREFIX + m_consumerNumber + ": "
                 + "OnServiceRequest: \n"
                 + "  Type       = " + Safir.Dob.Typesystem.Operations.GetName(serviceRequestProxy.TypeId) + "\n"
                 + "  Sender     = " + ConnInfoToXml(serviceRequestProxy.SenderConnectionInfo) + "\n"
                 + "  Handler    = " + serviceRequestProxy.ReceivingHandlerId + "\n"
                 + "  HandlerStr = " + serviceRequestProxy.ReceiverWithStringRepresentation + "\n"
                 + "  Request    = " + Safir.Dob.Typesystem.Serialization.ToXml(serviceRequestProxy.Blob));
            Logger.Instance.WriteLine();

            if (!responseSender.IsDone())
            {
                DoseTest.SuccessfulService resp = new DoseTest.SuccessfulService();
                resp.Info.Val = "AutoResponse";
                responseSender.Send(resp);
            }

        }

        #endregion

        #region CompletedRegistrationBase Members

        void Safir.Dob.CompletedRegistrationBase.OnCompletedRegistration(long typeId, Safir.Dob.Typesystem.HandlerId handlerId)
        {
            m_connection.ExitDispatch();
            ExecuteCallbackActions(Safir.Dob.CallbackId.Enumeration.OnCompletedRegistration);

            Logger.Instance.WriteLine
                (PREFIX + m_consumerNumber + ": "
                + "OnCompletedRegistration:\n"
                + "  Type      = " + Safir.Dob.Typesystem.Operations.GetName(typeId) + "\n"
                + "  HandlerId = " + handlerId);
            Logger.Instance.WriteLine();
        }

        #endregion

        #region EntityRequestBase Members

        public void OnCreateRequest(Safir.Dob.EntityRequestProxy entityRequestProxy, Safir.Dob.ResponseSender responseSender)
        {
            m_connection.ExitDispatch();
            m_responseSender = responseSender;
            ExecuteCallbackActions(Safir.Dob.CallbackId.Enumeration.OnCreateRequest);

            Pair value;
            KeyValuePair<System.Int64, Safir.Dob.Typesystem.HandlerId> key = 
                new KeyValuePair<long,Safir.Dob.Typesystem.HandlerId>
                (entityRequestProxy.TypeId, entityRequestProxy.ReceivingHandlerId);
            bool foundIt = m_instanceIdPolicyMap.TryGetValue(key,out value);
            if (!foundIt)
            {
                throw new Safir.Dob.Typesystem.SoftwareViolationException("Didn't find a corresponding item in m_instanceIdPolicyMap!");
            }
          
            if (value.First == Safir.Dob.InstanceIdPolicy.Enumeration.HandlerDecidesInstanceId)
            {
                Logger.Instance.WriteLine(PREFIX + m_consumerNumber + ": "
                     + "OnCreateRequest (Handler decides instance id): \n"
                     + "  Type       = " + entityRequestProxy.TypeId + "\n"
                     + "  Sender     = " + ConnInfoToXml(entityRequestProxy.SenderConnectionInfo) + "\n"
                     + "  Handler    = " + entityRequestProxy.ReceivingHandlerId + "\n"
                     + "  HandlerStr = " + entityRequestProxy.ReceiverWithStringRepresentation + "\n"
                     + "  Request    = " + Safir.Dob.Typesystem.Serialization.ToXml(entityRequestProxy.Blob));
                Logger.Instance.WriteLine();

                m_connection.SetAll(entityRequestProxy.Request,
                    new Safir.Dob.Typesystem.InstanceId(value.Second),
                                    entityRequestProxy.ReceivingHandlerId);

                Logger.Instance.WriteLine(PREFIX + m_consumerNumber + ": "
                     + "Handler created instance " + value.Second);


                Safir.Dob.EntityIdResponse resp = new Safir.Dob.EntityIdResponse();
                resp.Assigned.Val = new Safir.Dob.Typesystem.EntityId(entityRequestProxy.TypeId,
                                                                      new Safir.Dob.Typesystem.InstanceId(value.Second));
                responseSender.Send(resp);
                ++value.Second;
            }
            else
            {
                Logger.Instance.WriteLine(PREFIX + m_consumerNumber + ": "
                     + "OnCreateRequest (Requestor decides instance id): \n"
                     + "  Entity     = " + entityRequestProxy.EntityId + "\n"
                     + "  Sender     = " + ConnInfoToXml(entityRequestProxy.SenderConnectionInfo) + "\n"
                     + "  Handler    = " + entityRequestProxy.ReceivingHandlerId + "\n"
                     + "  HandlerStr = " + entityRequestProxy.ReceiverWithStringRepresentation + "\n"
                     + "  Request    = " + Safir.Dob.Typesystem.Serialization.ToXml(entityRequestProxy.Blob));
                Logger.Instance.WriteLine();

                m_connection.SetAll(entityRequestProxy.Request,
                                    entityRequestProxy.InstanceId,
                                    entityRequestProxy.ReceivingHandlerId);
            }



            if (!responseSender.IsDone())
            {
                DoseTest.SuccessfulCreate resp = new DoseTest.SuccessfulCreate();
                resp.Info.Val = "AutoResponse";
                responseSender.Send(resp);
            }

        }

        public void OnDeleteRequest(Safir.Dob.EntityRequestProxy entityRequestProxy, Safir.Dob.ResponseSender responseSender)
        {
            m_connection.ExitDispatch();
            m_responseSender = responseSender;
            ExecuteCallbackActions(Safir.Dob.CallbackId.Enumeration.OnDeleteRequest);

            Logger.Instance.WriteLine(PREFIX + m_consumerNumber + ": "
                 + "OnDeleteRequest: \n"
                 + "  Entity     = " + entityRequestProxy.EntityId + "\n"
                 + "  Sender     = " + ConnInfoToXml(entityRequestProxy.SenderConnectionInfo) + "\n"
                 + "  Handler    = " + entityRequestProxy.ReceivingHandlerId + "\n"
                 + "  HandlerStr = " + entityRequestProxy.ReceiverWithStringRepresentation);

            Logger.Instance.WriteLine();

            m_connection.Delete(entityRequestProxy.EntityId, entityRequestProxy.ReceivingHandlerId);

            if (!responseSender.IsDone())
            {
                DoseTest.SuccessfulDelete resp = new DoseTest.SuccessfulDelete();
                resp.Info.Val = "AutoResponse";
                responseSender.Send(resp);
            }

        }

        public void OnUpdateRequest(Safir.Dob.EntityRequestProxy entityRequestProxy, Safir.Dob.ResponseSender responseSender)
        {
            m_connection.ExitDispatch();
            m_responseSender = responseSender;
            ExecuteCallbackActions(Safir.Dob.CallbackId.Enumeration.OnUpdateRequest);

            Logger.Instance.WriteLine(PREFIX + m_consumerNumber + ": "
                 + "OnUpdateRequest: \n"
                 + "  Entity     = " + entityRequestProxy.EntityId + "\n"
                 + "  Sender     = " + ConnInfoToXml(entityRequestProxy.SenderConnectionInfo) + "\n"
                 + "  Handler    = " + entityRequestProxy.ReceivingHandlerId + "\n"
                 + "  HandlerStr = " + entityRequestProxy.ReceiverWithStringRepresentation + "\n"
                 + "  Request    = " + Safir.Dob.Typesystem.Serialization.ToXml(entityRequestProxy.Blob));
            Logger.Instance.WriteLine();

            m_connection.SetChanges(entityRequestProxy.Request, entityRequestProxy.InstanceId, entityRequestProxy.ReceivingHandlerId);

            if (!responseSender.IsDone())
            {
                DoseTest.SuccessfulUpdate resp = new DoseTest.SuccessfulUpdate();
                resp.Info.Val = "AutoResponse";
                responseSender.Send(resp);
            }

        }
        
        #endregion

        #region EntityInjectionBase Members

        void Safir.Dob.EntityInjectionBase.OnInitialInjectionsDone(long typeId, Safir.Dob.Typesystem.HandlerId handlerId)
        {
            m_connection.ExitDispatch();
            ExecuteCallbackActions(Safir.Dob.CallbackId.Enumeration.OnInitialInjectionsDone);

            Logger.Instance.WriteLine(PREFIX + m_consumerNumber + ": "
                + "OnInitialInjectionsDone:\n"
                + "  Type      = " + Safir.Dob.Typesystem.Operations.GetName(typeId) + "\n"
                + "  HandlerId = " + handlerId);
            Logger.Instance.WriteLine();
        }

        void Safir.Dob.EntityInjectionBase.OnInjectedDeletedEntity(Safir.Dob.InjectedEntityProxy entityProxy)
        {
            m_connection.ExitDispatch();
            ExecuteCallbackActions(Safir.Dob.CallbackId.Enumeration.OnInjectedDeletedEntity);

            Logger.Instance.WriteLine(PREFIX + m_consumerNumber + ": "
                 + "OnInjectedDeletedEntity:\n"
                 + "  EntityId       = " + entityProxy.EntityId + "\n"
                 + "  Current  = " + Safir.Dob.Typesystem.Serialization.ToXml(entityProxy.Current));
            Logger.Instance.WriteLine();

        }

        void Safir.Dob.EntityInjectionBase.OnInjectedNewEntity(Safir.Dob.InjectedEntityProxy entityProxy)
        {
            m_connection.ExitDispatch();
            ExecuteCallbackActions(Safir.Dob.CallbackId.Enumeration.OnInjectedNewEntity);

            Logger.Instance.WriteLine(PREFIX + m_consumerNumber + ": "
                 + "OnInjectedNewEntity:\n"
                 + "  EntityId  = " + entityProxy.EntityId + "\n"
                 + "  Injection = " + Safir.Dob.Typesystem.Serialization.ToXml(entityProxy.InjectionBlob) + "\n"
                 + "  Changed top-level members: ");

            Safir.Dob.Entity entity = entityProxy.Injection;
            for (int i = 0;
                 i < Safir.Dob.Typesystem.Members.GetNumberOfMembers(entity.GetTypeId());
                 ++i)
            {
                if (entity.GetMember(i, 0).IsChanged())
                {
                    Logger.Instance.WriteLine("    "
                               + Safir.Dob.Typesystem.Members.GetName(entity.GetTypeId(), i));
                }
            }

            Logger.Instance.WriteLine();
        }

        void Safir.Dob.EntityInjectionBase.OnInjectedUpdatedEntity(Safir.Dob.InjectedEntityProxy entityProxy)
        {
            m_connection.ExitDispatch();
            ExecuteCallbackActions(Safir.Dob.CallbackId.Enumeration.OnInjectedUpdatedEntity);

            Logger.Instance.WriteLine(PREFIX + m_consumerNumber + ": "
                 + "OnInjectedUpdatedEntity:\n"
                 + "  EntityId  = " + entityProxy.EntityId + "\n"
                 + "  Injection = " + Safir.Dob.Typesystem.Serialization.ToXml(entityProxy.InjectionBlob) + "\n"
                 + "  Current   = " + Safir.Dob.Typesystem.Serialization.ToXml(entityProxy.Current) + "\n"
                 + "  Changed top-level members: ");

            Safir.Dob.Entity entity = entityProxy.Injection;
            for (int i = 0;
                 i < Safir.Dob.Typesystem.Members.GetNumberOfMembers(entity.GetTypeId());
                 ++i)
            {
                if (entity.GetMember(i, 0).IsChanged())
                {
                    Logger.Instance.WriteLine("    "
                               + Safir.Dob.Typesystem.Members.GetName(entity.GetTypeId(), i));
                }
            }

            Logger.Instance.WriteLine();

        }

        #endregion

        #region Requestor Members

        void Safir.Dob.Requestor.OnNotRequestOverflow()
        {
            m_connection.ExitDispatch();
            ExecuteCallbackActions(Safir.Dob.CallbackId.Enumeration.OnNotRequestOverflow);
            Logger.Instance.WriteLine(PREFIX + m_consumerNumber + ": " + "OnNotRequestOverflow");
        }

        void Safir.Dob.Requestor.OnResponse(Safir.Dob.ResponseProxy responseProxy)
        {
            m_connection.ExitDispatch();
            ExecuteCallbackActions(Safir.Dob.CallbackId.Enumeration.OnResponse);

            Logger.Instance.Write(PREFIX + m_consumerNumber
                 + ": " + "OnResponse:\n"
                 + "  Type       = " + Safir.Dob.Typesystem.Operations.GetName(responseProxy.TypeId) + "\n"
                 + "  IsSuccess  = " + responseProxy.IsSuccess.ToString().ToLower() + "\n"
                 + "  Sender     = " + ConnInfoToXml(responseProxy.ResponseSenderConnectionInfo) + "\n"
                 + "  Response   = " + Safir.Dob.Typesystem.Serialization.ToXml(responseProxy.Blob) + "\n"
                 + "  Request    = ");
            try
            {
                Logger.Instance.WriteLine(Safir.Dob.Typesystem.Serialization.ToXml(responseProxy.Request));
            }
            catch (Safir.Dob.Typesystem.SoftwareViolationException)
            {
                Safir.Dob.Typesystem.EntityId eid = new Safir.Dob.Typesystem.EntityId(responseProxy.RequestTypeId, responseProxy.RequestInstanceId);
                Logger.Instance.WriteLine("DeleteRequest on " + eid);
            }

            Logger.Instance.WriteLine();

        }

        #endregion

        private string ConnInfoToXml(Safir.Dob.ConnectionInfo connInfo)
        {
            connInfo.ConnectionId.SetNull();
            if (!connInfo.ConnectionName.IsNull())
            {
                int index = connInfo.ConnectionName.Val.LastIndexOf("#");
                connInfo.ConnectionName.Val = connInfo.ConnectionName.Val.Substring(0, index);
            }
            return Safir.Dob.Typesystem.Serialization.ToXml(connInfo);
        }

        #region Private data members

        private Safir.Dob.SecondaryConnection m_connection = new Safir.Dob.SecondaryConnection();

        private readonly int m_consumerNumber;

        private Dictionary<Safir.Dob.CallbackId.Enumeration, List<DoseTest.Action>> m_callbackActions;
        
        private Safir.Dob.ResponseSender m_responseSender = null;
        
        public class Pair
        {
            public Pair()
            {
            }

            public Pair(Safir.Dob.InstanceIdPolicy.Enumeration first, System.Int64 second)
            {
                this.First = first;
                this.Second = second;
            }

            public Safir.Dob.InstanceIdPolicy.Enumeration First;
            public System.Int64 Second;
        };

        //the Object in the last pair is a boxed Int64
        private System.Collections.Generic.Dictionary<KeyValuePair<System.Int64, Safir.Dob.Typesystem.HandlerId>,
                                              Pair>
            m_instanceIdPolicyMap = new Dictionary<KeyValuePair<long, Safir.Dob.Typesystem.HandlerId>, Pair>();


        private class TimestampRequestor : Safir.Dob.Requestor
        {
            #region Requestor Members

            public void OnNotRequestOverflow() { }

            public void OnResponse(Safir.Dob.ResponseProxy responseProxy) { }

            #endregion
        };

        TimestampRequestor m_timestampRequestor = new TimestampRequestor();

        #endregion 
    
    
    
    }

}
