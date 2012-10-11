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
using System.Threading;
using System.Runtime.InteropServices;

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
        Safir.Dob.Requestor,
        Safir.Application.Backdoor
    {
        public static long instanceCount = 0;

        private const string PREFIX = "Consumer ";

        public Consumer(int consumerNumber,
                        string connectionName,
                        string instance)
        {
            Interlocked.Increment(ref instanceCount);

            m_consumerNumber = consumerNumber;
            m_connectionName = connectionName;
            m_connectionInstance = instance;
            m_callbackActions = new Dictionary<Safir.Dob.CallbackId.Enumeration, List<DoseTest.Action>>();
            foreach (Safir.Dob.CallbackId.Enumeration cb in Enum.GetValues(typeof(Safir.Dob.CallbackId.Enumeration)))
            {
                m_callbackActions.Add(cb, new List<DoseTest.Action>());
            }
            m_connection.Attach(m_connectionName,m_connectionInstance);
        }

        ~Consumer()
        {
            Interlocked.Decrement(ref instanceCount);
        }


        static bool NeedBinaryCheck(Safir.Dob.Typesystem.Object obj)
        {
            return
                obj.GetTypeId() == DoseTest.ComplexGlobalMessage.ClassTypeId ||
                obj.GetTypeId() == DoseTest.ComplexGlobalEntity.ClassTypeId ||
                obj.GetTypeId() == DoseTest.ComplexGlobalService.ClassTypeId;
        }

        //returns true if the blob needs to be modified.
        static bool CheckBinaryMemberInternal(Safir.Dob.Typesystem.BinaryContainer cont)
        {
            if (!cont.IsNull() && cont.Val.Length > 10000) //only check for large sizes
            {
                if (cont.Val.Length != 10 *1024 *1024)
                {
                    Logger.Instance.WriteLine("Binary is wrong size!");
                }
                else
                {
                    byte val = 0;
                    foreach (byte b in cont.Val)
                        //                    for (Safir.Dob.Typesystem.Binary.const_iterator it = cont.Val.begin();
                        //                         it != cont.GetVal().end(); ++it)
                    {
                        if (b != val)
                        {
                            Logger.Instance.WriteLine("Bad value in binary!");
                            break;
                        }
                        ++val;
                    }
                }
                //we do NOT want to print all this out to stdout, so we set it to null once we've checked it.
                cont.SetNull();
                return true;
            }
            else
            {
                return false;
            }
        }

        static String CheckBinaryMember(Safir.Dob.Typesystem.Object obj, System.IntPtr blob)
        {
            if (obj.GetTypeId() == DoseTest.ComplexGlobalMessage.ClassTypeId)
            {
                if (CheckBinaryMemberInternal(((DoseTest.ComplexGlobalMessage)obj).BinaryMember))
                {
                    System.Int32 blobSize = Safir.Dob.Typesystem.BlobOperations.GetSize(blob);
                    System.IntPtr b = Marshal.AllocHGlobal(blobSize);
                    //copy the blob
                    for (System.Int32 index = 0; index < blobSize; ++index)
                    {
                        Marshal.WriteByte(b,index,Marshal.ReadByte(blob,index));
                    }
                    Safir.Dob.Typesystem.BlobOperations.SetNull
                        (b,DoseTest.ComplexGlobalMessage.BinaryMemberMemberIndex,0);
                    String xml = Safir.Dob.Typesystem.Serialization.ToXml(b);
                    Marshal.FreeHGlobal(b);
                    return xml;
                }
            }
            else if (obj.GetTypeId() == DoseTest.ComplexGlobalEntity.ClassTypeId)
            {
                System.IntPtr b = System.IntPtr.Zero;
                if (CheckBinaryMemberInternal(((DoseTest.ComplexGlobalEntity)obj).BinaryMember))
                {
                    System.Int32 blobSize = Safir.Dob.Typesystem.BlobOperations.GetSize(blob);
                    b = Marshal.AllocHGlobal(blobSize);
                    //copy the blob
                    for (System.Int32 index = 0; index < blobSize; ++index)
                    {
                        Marshal.WriteByte(b,index,Marshal.ReadByte(blob,index));
                    }
                    Safir.Dob.Typesystem.BlobOperations.SetNull
                        (b,DoseTest.ComplexGlobalEntity.BinaryMemberMemberIndex,0);

                }

                //in the entity we use the binary array as well
                for (int i = 0; i < DoseTest.ComplexGlobalEntity.BinaryArrayMemberArraySize; ++i)
                {
                    if (CheckBinaryMemberInternal(((DoseTest.ComplexGlobalEntity)obj).BinaryArrayMember[i]))
                    {
                        if (b == System.IntPtr.Zero)
                        {
                            System.Int32 blobSize = Safir.Dob.Typesystem.BlobOperations.GetSize(blob);
                            b = Marshal.AllocHGlobal(blobSize);
                            //copy the blob
                            for (System.Int32 index = 0; index < blobSize; ++index)
                            {
                                Marshal.WriteByte(b,index,Marshal.ReadByte(blob,index));
                            }
                        }
                        Safir.Dob.Typesystem.BlobOperations.SetNull
                            (b,DoseTest.ComplexGlobalEntity.BinaryArrayMemberMemberIndex,i);
                    }
                }

                if (b != System.IntPtr.Zero)
                {
                    String xml = Safir.Dob.Typesystem.Serialization.ToXml(b);
                    Marshal.FreeHGlobal(b);
                    return xml;
                }


            }
            else if (obj.GetTypeId() == DoseTest.ComplexGlobalService.ClassTypeId)
            {
                if (CheckBinaryMemberInternal(((DoseTest.ComplexGlobalService)obj).BinaryMember))
                {
                    System.Int32 blobSize = Safir.Dob.Typesystem.BlobOperations.GetSize(blob);
                    System.IntPtr b = Marshal.AllocHGlobal(blobSize);
                    //copy the blob
                    for (System.Int32 index = 0; index < blobSize; ++index)
                    {
                        Marshal.WriteByte(b,index,Marshal.ReadByte(blob,index));
                    }
                    Safir.Dob.Typesystem.BlobOperations.SetNull
                        (b,DoseTest.ComplexGlobalService.BinaryMemberMemberIndex,0);
                    String xml = Safir.Dob.Typesystem.Serialization.ToXml(b);
                    Marshal.FreeHGlobal(b);
                    return xml;
                }
            }

            return Safir.Dob.Typesystem.Serialization.ToXml(blob);
        }


        String CallbackId()
        {
            Safir.Dob.CallbackId.Enumeration cb = new Safir.Dob.ConnectionAspectMisc(m_connection).GetCurrentCallbackId();
            return cb.ToString();
        }

        public void AddCallbackAction(DoseTest.Action action)
        {
            m_callbackActions[action.ActionCallback.Val].Add(action);
        }


        public void ExecuteCallbackActions(Safir.Dob.CallbackId.Enumeration callback)
        {
            foreach (DoseTest.Action action in m_callbackActions[callback])
            {
                DoseTest.ActionEnum.Enumeration actionKind = action.ActionKind.Val;

                ExecuteAction(action);

                if (actionKind == DoseTest.ActionEnum.Enumeration.ResetCallbackActions)
                {
                    return;
                }
            }
        }



        private System.Int64 GetTimestamp(DoseTest.Action action)
        {
            Safir.Dob.Typesystem.EntityId entityId = new Safir.Dob.Typesystem.EntityId
               (DoseTest.LastInjectionTimestamp.ClassTypeId,
                new Safir.Dob.Typesystem.InstanceId(DoseTest.LastInjectionTimestamp.ClassTypeId));

            System.Int64 delta = action.TimestampDelta.Val;
            using (Safir.Dob.EntityProxy ep = m_connection.Read(entityId))
            {
                DoseTest.LastInjectionTimestamp ent = (DoseTest.LastInjectionTimestamp)ep.Entity;

                System.Int64 newVal = ent.Timestamp.Val + delta;

                ent.Timestamp.Val = newVal;

                m_connection.UpdateRequest(ent, entityId.InstanceId, m_timestampRequestor);

                return newVal;
            }
        }

        public void ExecuteAction(DoseTest.Action action)
        {
            try
            {
                //only becomes true if RepeatUntilOverflow is true
                bool repeat = !action.RepeatUntilOverflow.IsNull() && action.RepeatUntilOverflow.Val;

                DateTime actionStartTime = DateTime.Now;
                long repeats = 0;

                do //while repeat
                {
                    try
                    {
                        switch (action.ActionKind.Val)
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
                                    m_responseSenderDiscarded = true;
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

                            case DoseTest.ActionEnum.Enumeration.GetInstanceIdPolicy:
                                {
                                    Logger.Instance.WriteLine(PREFIX + m_consumerNumber + ": "
                                        + "GetInstanceIdPolicy (type = "
                                        + Safir.Dob.Typesystem.Operations.GetName(action.TypeId.Val)
                                        + ", handler = " + action.Handler.Val
                                        + "): "
                                        + Safir.Dob.Typesystem.Operations.GetEnumerationValueName(Safir.Dob.InstanceIdPolicy.EnumerationId,
                                    (int)m_connection.GetInstanceIdPolicy(action.TypeId.Val, action.Handler.Val)));
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

                            case DoseTest.ActionEnum.Enumeration.GetContext:
                                {
                                    Logger.Instance.WriteLine(PREFIX + m_consumerNumber + ": "
                                        + "The test connection is opened in context "
                                        + new Safir.Dob.ConnectionAspectMisc(m_connection).GetContext());
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

                            case DoseTest.ActionEnum.Enumeration.StartBackdoor:
                                {
                                    Logger.Instance.WriteLine(PREFIX + m_consumerNumber + ": StartBackdoor");
                                    m_backdoorKeeper.Start(this, m_connectionName, m_connectionInstance);
                                }
                                break;

                            case DoseTest.ActionEnum.Enumeration.StopBackdoor:
                                {
                                    Logger.Instance.WriteLine(PREFIX + m_consumerNumber + ": StopBackdoor");
                                    m_backdoorKeeper.Stop();
                                }
                                break;

                            case DoseTest.ActionEnum.Enumeration.IsBackdoorStarted:
                                {
                                    Logger.Instance.WriteLine(PREFIX + m_consumerNumber + ": The backdoor is " +
                                                             (m_backdoorKeeper.IsStarted() ? "" : "not ") + "started");
                                }
                                break;

                            default:
                                Logger.Instance.WriteLine(PREFIX + m_consumerNumber + ": "
                                    + "No handler defined for action "
                                    + action.ActionKind.Val);
                                break;
                        }
                        ++repeats;

                        if (repeats % 1000 == 0)
                        {
                            Console.WriteLine("I've now done "+repeats+ " repeats without an overflow!");
                        }
                    }
                    catch (Safir.Dob.OverflowException)
                    {
                        Logger.Instance.WriteLine("Caught Overflow exception");
                        
                        if (repeat)
                        {
                            double secs = (DateTime.Now - actionStartTime).TotalSeconds;
                            Console.WriteLine("Time elapsed before I got an overflow was " + secs);
                            Console.WriteLine("I managed to send "+ repeats + " times");
 
                        }

                        //sleep a very short while, to let dose_main empty
                        //the message out queue. This hopefully reduces the tc 003
                        //output differences
                        Thread.Sleep(1);
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

            DoseTest.RootMessage msg = messageProxy.Message as DoseTest.RootMessage;
            String xml;

            if (NeedBinaryCheck(msg))
            {
                xml = CheckBinaryMember(msg,messageProxy.Blob);
            }
            else
            {
                xml = Safir.Dob.Typesystem.Serialization.ToXml(messageProxy.Blob);
            }

            Logger.Instance.WriteLine(PREFIX + m_consumerNumber + ": "
                       + CallbackId() + ":\n"
                       + "  Type       = " + Safir.Dob.Typesystem.Operations.GetName(messageProxy.TypeId) + "\n"
                       + "  ChannelId  = " + messageProxy.ChannelId + "\n"
                       + "  Sender     = " + ConnInfoToXml(messageProxy.SenderConnectionInfo) + "\n"
                       + "  ChannelId  = " + messageProxy.ChannelIdWithStringRepresentation + "\n"
                       + "  Message    = " + xml + "\n\n");
        }

        #endregion

        #region EntitySubscriber Members

        void Safir.Dob.EntitySubscriber.OnNewEntity(Safir.Dob.EntityProxy entityProxy)
        {
            m_connection.ExitDispatch();
            ExecuteCallbackActions(Safir.Dob.CallbackId.Enumeration.OnNewEntity);

            DoseTest.RootEntity entity = entityProxy.EntityWithChangeInfo as DoseTest.RootEntity;
            String xml;

            if (NeedBinaryCheck(entity))
            {
                xml = CheckBinaryMember(entity,entityProxy.Blob);
            }
            else
            {
                xml = Safir.Dob.Typesystem.Serialization.ToXml(entityProxy.Blob);
            }


            Logger.Instance.WriteLine(PREFIX + m_consumerNumber + ": "
                    + CallbackId() + ":\n"
                    + "  EntityId  = " + entityProxy.EntityId + "\n"
                    + "  Owner     = " + entityProxy.Owner + "\n"
                    + "  OwnerConn = " + ConnInfoToXml(entityProxy.OwnerConnectionInfo) + "\n"
                    + "  OwnerStr  = " + entityProxy.OwnerWithStringRepresentation + "\n"
                    + "  Entity    = " + xml + "\n"
                    + "  Changed top-level members: ");

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

            DoseTest.RootEntity entity = entityProxy.EntityWithChangeInfo as DoseTest.RootEntity;
            String xml;

            if (NeedBinaryCheck(entity))
            {
                xml = CheckBinaryMember(entity,entityProxy.Blob);
            }
            else
            {
                xml = Safir.Dob.Typesystem.Serialization.ToXml(entityProxy.Blob);
            }

            DoseTest.RootEntity prevEntity = entityProxy.Previous.Entity as DoseTest.RootEntity;
            String prevXml;

            if (NeedBinaryCheck(prevEntity))
            {
                prevXml = CheckBinaryMember(prevEntity,entityProxy.Previous.Blob);
            }
            else
            {
                prevXml = Safir.Dob.Typesystem.Serialization.ToXml(entityProxy.Previous.Blob);
            }

            Logger.Instance.WriteLine(PREFIX + m_consumerNumber + ": "
                + CallbackId() + ":\n"
                + "  EntityId  = " + entityProxy.EntityId + "\n"
                + "  Owner     = " + entityProxy.Owner + "\n"
                + "  OwnerConn = " + ConnInfoToXml(entityProxy.OwnerConnectionInfo) + "\n"
                + "  OwnerStr  = " + entityProxy.OwnerWithStringRepresentation + "\n"
                + "  Entity    = " + xml + "\n"
                + "  Previous  = " + prevXml + "\n"
                + "  Changed top-level members: ");

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

            DoseTest.RootEntity prevEntity = entityProxy.Previous.Entity as DoseTest.RootEntity;
            String prevXml;

            if (NeedBinaryCheck(prevEntity))
            {
                prevXml = CheckBinaryMember(prevEntity,entityProxy.Previous.Blob);
            }
            else
            {
                prevXml = Safir.Dob.Typesystem.Serialization.ToXml(entityProxy.Previous.Blob);
            }

            Logger.Instance.WriteLine(PREFIX + m_consumerNumber + ": "
                            + CallbackId() + ":\n"
                            + "  EntityId       = " + entityProxy.EntityId + "\n"
                            + "  deletedByOwner = " + deletedByOwner.ToString().ToLower() + "\n"
                            + "  Owner          = " + entityProxy.Owner + "\n"
                            + "  OwnerStr  = " + entityProxy.OwnerWithStringRepresentation + "\n"
                            + "  Previous  = " + prevXml);

            Logger.Instance.WriteLine();
        }

#endregion


        #region RegistrationSubscriber Members

        void Safir.Dob.RegistrationSubscriber.OnRegistered(long typeId, Safir.Dob.Typesystem.HandlerId handlerId)
        {
            m_connection.ExitDispatch();
            ExecuteCallbackActions(Safir.Dob.CallbackId.Enumeration.OnRegistered);

            Logger.Instance.WriteLine(PREFIX + m_consumerNumber + ": "
                       + CallbackId() + ":\n"
                       + "  Type      = " + Safir.Dob.Typesystem.Operations.GetName(typeId) +"\n"
                       + "  HandlerId = " + handlerId);

            Logger.Instance.WriteLine();
        }

        void Safir.Dob.RegistrationSubscriber.OnUnregistered(long typeId, Safir.Dob.Typesystem.HandlerId handlerId)
        {
            m_connection.ExitDispatch();
            ExecuteCallbackActions(Safir.Dob.CallbackId.Enumeration.OnUnregistered);

            Logger.Instance.WriteLine(PREFIX + m_consumerNumber + ": "
                       + CallbackId() + ":\n"
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

            Logger.Instance.WriteLine(PREFIX + m_consumerNumber + ": " + CallbackId());
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
                + CallbackId() + ":\n"
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

            DoseTest.RootService svc = serviceRequestProxy.Request as DoseTest.RootService;
            String xml;

            if (NeedBinaryCheck(svc))
            {
                xml = CheckBinaryMember(svc,serviceRequestProxy.Blob);
            }
            else
            {
                xml = Safir.Dob.Typesystem.Serialization.ToXml(serviceRequestProxy.Blob);
            }


            Logger.Instance.WriteLine(PREFIX + m_consumerNumber + ": "
                 + CallbackId() + ": \n"
                 + "  Type       = " + Safir.Dob.Typesystem.Operations.GetName(serviceRequestProxy.TypeId) + "\n"
                 + "  Sender     = " + ConnInfoToXml(serviceRequestProxy.SenderConnectionInfo) + "\n"
                 + "  Handler    = " + serviceRequestProxy.ReceivingHandlerId + "\n"
                 + "  HandlerStr = " + serviceRequestProxy.ReceiverWithStringRepresentation + "\n"
                 + "  Request    = " + xml);
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
                + CallbackId() + ":\n"
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
            m_responseSenderDiscarded = false;
            ExecuteCallbackActions(Safir.Dob.CallbackId.Enumeration.OnCreateRequest);

            DoseTest.RootEntity req = entityRequestProxy.Request as DoseTest.RootEntity;
            String xml;

            if (NeedBinaryCheck(req))
            {
                xml = CheckBinaryMember(req,entityRequestProxy.Blob);
            }
            else
            {
                xml = Safir.Dob.Typesystem.Serialization.ToXml(entityRequestProxy.Blob);
            }

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
                Logger.Instance.WriteLine
                    (PREFIX + m_consumerNumber + ": "
                     + CallbackId() + " (Handler decides instance id): \n"
                     + "  Type       = " + entityRequestProxy.TypeId + "\n"
                     + "  Sender     = " + ConnInfoToXml(entityRequestProxy.SenderConnectionInfo) + "\n"
                     + "  Handler    = " + entityRequestProxy.ReceivingHandlerId + "\n"
                     + "  HandlerStr = " + entityRequestProxy.ReceiverWithStringRepresentation + "\n"
                     + "  Request    = " + xml);
                Logger.Instance.WriteLine();

                if (!m_responseSenderDiscarded)
                {
                    m_connection.SetAll(req,
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
            }
            else
            {
                Logger.Instance.WriteLine
                    (PREFIX + m_consumerNumber + ": "
                     + CallbackId() + " (Requestor decides instance id): \n"
                     + "  Entity     = " + entityRequestProxy.EntityId + "\n"
                     + "  Sender     = " + ConnInfoToXml(entityRequestProxy.SenderConnectionInfo) + "\n"
                     + "  Handler    = " + entityRequestProxy.ReceivingHandlerId + "\n"
                     + "  HandlerStr = " + entityRequestProxy.ReceiverWithStringRepresentation + "\n"
                     + "  Request    = " + xml);
                Logger.Instance.WriteLine();

                if (!m_responseSenderDiscarded)
                {
                    m_connection.SetAll(req,
                                        entityRequestProxy.InstanceId,
                                        entityRequestProxy.ReceivingHandlerId);
                }
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
            m_responseSenderDiscarded = false;
            ExecuteCallbackActions(Safir.Dob.CallbackId.Enumeration.OnDeleteRequest);

            Logger.Instance.WriteLine(PREFIX + m_consumerNumber + ": "
                 + CallbackId() + ": \n"
                 + "  Entity     = " + entityRequestProxy.EntityId + "\n"
                 + "  Sender     = " + ConnInfoToXml(entityRequestProxy.SenderConnectionInfo) + "\n"
                 + "  Handler    = " + entityRequestProxy.ReceivingHandlerId + "\n"
                 + "  HandlerStr = " + entityRequestProxy.ReceiverWithStringRepresentation);

            Logger.Instance.WriteLine();

            if (!m_responseSenderDiscarded)
            {
                m_connection.Delete(entityRequestProxy.EntityId, entityRequestProxy.ReceivingHandlerId);
            }

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
            m_responseSenderDiscarded = false;
            ExecuteCallbackActions(Safir.Dob.CallbackId.Enumeration.OnUpdateRequest);

            DoseTest.RootEntity req = entityRequestProxy.Request as DoseTest.RootEntity;
            String xml;

            if (NeedBinaryCheck(req))
            {
                xml = CheckBinaryMember(req,entityRequestProxy.Blob);
            }
            else
            {
                xml = Safir.Dob.Typesystem.Serialization.ToXml(entityRequestProxy.Blob);
            }

            Logger.Instance.WriteLine
                (PREFIX + m_consumerNumber + ": "
                 + CallbackId() + ": \n"
                 + "  Entity     = " + entityRequestProxy.EntityId + "\n"
                 + "  Sender     = " + ConnInfoToXml(entityRequestProxy.SenderConnectionInfo) + "\n"
                 + "  Handler    = " + entityRequestProxy.ReceivingHandlerId + "\n"
                 + "  HandlerStr = " + entityRequestProxy.ReceiverWithStringRepresentation + "\n"
                 + "  Request    = " + xml);
            Logger.Instance.WriteLine();

            if (!m_responseSenderDiscarded)
            {
                m_connection.SetChanges(req,
                                        entityRequestProxy.InstanceId,
                                        entityRequestProxy.ReceivingHandlerId);
            }

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
                + CallbackId() + ":\n"
                + "  Type      = " + Safir.Dob.Typesystem.Operations.GetName(typeId) + "\n"
                + "  HandlerId = " + handlerId);
            Logger.Instance.WriteLine();
        }

        void Safir.Dob.EntityInjectionBase.OnInjectedDeletedEntity(Safir.Dob.InjectedEntityProxy entityProxy)
        {
            m_connection.ExitDispatch();
            ExecuteCallbackActions(Safir.Dob.CallbackId.Enumeration.OnInjectedDeletedEntity);

            Logger.Instance.WriteLine(PREFIX + m_consumerNumber + ": "
                 + CallbackId() + ":\n"
                 + "  EntityId       = " + entityProxy.EntityId + "\n"
                 + "  Current  = " + Safir.Dob.Typesystem.Serialization.ToXml(entityProxy.Current));
            Logger.Instance.WriteLine();

        }

        void Safir.Dob.EntityInjectionBase.OnInjectedNewEntity(Safir.Dob.InjectedEntityProxy entityProxy)
        {
            m_connection.ExitDispatch();
            ExecuteCallbackActions(Safir.Dob.CallbackId.Enumeration.OnInjectedNewEntity);

            Logger.Instance.WriteLine(PREFIX + m_consumerNumber + ": "
                 + CallbackId() + ":\n"
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
                 + CallbackId() + ":\n"
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
            Logger.Instance.WriteLine(PREFIX + m_consumerNumber + ": " + CallbackId());
        }

        void Safir.Dob.Requestor.OnResponse(Safir.Dob.ResponseProxy responseProxy)
        {
            m_connection.ExitDispatch();
            ExecuteCallbackActions(Safir.Dob.CallbackId.Enumeration.OnResponse);

            Logger.Instance.Write(PREFIX + m_consumerNumber
                 + ": " + CallbackId() + ":\n"
                 + "  Type       = " + Safir.Dob.Typesystem.Operations.GetName(responseProxy.TypeId) + "\n"
                 + "  IsSuccess  = " + responseProxy.IsSuccess.ToString().ToLower() + "\n"
                 + "  Sender     = " + ConnInfoToXml(responseProxy.ResponseSenderConnectionInfo) + "\n"
                 + "  Response   = " + Safir.Dob.Typesystem.Serialization.ToXml(responseProxy.Blob) + "\n"
                 + "  Request    = ");
            try
            {
                Safir.Dob.Typesystem.Object req = responseProxy.Request;

                if (NeedBinaryCheck(req))
                {
                    Logger.Instance.WriteLine(CheckBinaryMember(req,responseProxy.RequestBlob));
                }
                else
                {
                    Logger.Instance.WriteLine(Safir.Dob.Typesystem.Serialization.ToXml(responseProxy.RequestBlob));
                }
            }
            catch (Safir.Dob.Typesystem.SoftwareViolationException)
            {
                Safir.Dob.Typesystem.EntityId eid = new Safir.Dob.Typesystem.EntityId(responseProxy.RequestTypeId, responseProxy.RequestInstanceId);
                Logger.Instance.WriteLine("DeleteRequest on " + eid);
            }

            Logger.Instance.WriteLine();

        }

        #endregion

        #region Backdoor callbacks

        void Safir.Application.Backdoor.HandleCommand(string[] cmdTokens)
        {
            Logger.Instance.WriteLine(PREFIX + m_consumerNumber + ": Got a backdoor HandleCommand callback. Command tokens:");
            foreach (string cmd in cmdTokens)
            {
                Logger.Instance.Write(cmd + ' ');
            }
            Logger.Instance.WriteLine();
        }

        string Safir.Application.Backdoor.GetHelpText()
        {
            Logger.Instance.WriteLine(PREFIX + m_consumerNumber + ": Got a backdoor GetHelpText callback.");
            return "This is a help text";
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
        private readonly string m_connectionName;
        private readonly string m_connectionInstance;
        private Safir.Application.BackdoorKeeper m_backdoorKeeper = new Safir.Application.BackdoorKeeper();

        private Dictionary<Safir.Dob.CallbackId.Enumeration, List<DoseTest.Action>> m_callbackActions;

        private Safir.Dob.ResponseSender m_responseSender = null;

        private bool m_responseSenderDiscarded;

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
