/******************************************************************************
*
* Copyright Saab AB, 2005-2008 (http://www.safirsdk.com)
*
* Created by: Erik Adolfsson / sterad
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
using System.Threading;

namespace douf_time_test
{
    /// <summary>
    /// Summary description for Subscriber.
    /// </summary>
    public class Subscriber
    {
        private DobHandler dobHandler;
        public Consumer consumer;

        public Subscriber(DobHandler dh)
        {
            dobHandler = dh;
            consumer = new Consumer();
        }

        public void Start()
        {
            dobHandler.Dob.SubscribeEntity(Safir.Test.TimeConversion.ClassTypeId, true, true, true, consumer);
            Thread.Sleep(Timeout.Infinite);
        }
    }

    /// <summary>
    /// Summary description for Subscriber.
    /// </summary>
    public class Requestor
    {
        private DobHandler dobHandler;
        public Consumer consumer;

        public Requestor(DobHandler dh)
        {
            dobHandler = dh;
            consumer = new Consumer();
        }

        public void Start()
        {
            dobHandler.Dob.RegisterEntityHandler(Safir.Test.TimeConversion.ClassTypeId, new Safir.Dob.Typesystem.HandlerId(), Safir.Dob.InstanceIdPolicy.Enumeration.HandlerDecidesInstanceId, consumer);

            Safir.Test.TimeConversion timeConv = new Safir.Test.TimeConversion();

            do
            {
                // Get current time in NTP format
                Double time = Safir.Time.TimeProvider.GetUtcTime();
                System.DateTime dateTime = Safir.Time.TimeProvider.ToDateTime(time);
                Print.Time(dateTime);

                // Send to Dob
                timeConv.TimeStamp.Val = time;
                dobHandler.Dob.SetAll(timeConv, new Safir.Dob.Typesystem.InstanceId(0), new Safir.Dob.Typesystem.HandlerId());

                Thread.Sleep(5077);

            }while (true);
            

        }
    }

    /// <summary>
    /// Summary description for Consumer.
    /// </summary>
    public class Consumer : Safir.Dob.EntitySubscriber, Safir.Dob.EntityHandler
    {
        public Consumer()
        {
        }

        #region RevokedRegistrationBase Members

        public void OnRevokedRegistration(long typeId, Safir.Dob.Typesystem.HandlerId handlerId)
        {
        }

        #endregion

        #region EntityRequestBase Members

        public void OnCreateRequest(Safir.Dob.EntityRequestProxy entityRequestProxy, Safir.Dob.ResponseSender responseSender)
        {
            System.Console.WriteLine("- Received create entity: " + entityRequestProxy.EntityId.ToString());
        }

        public void OnDeleteRequest(Safir.Dob.EntityRequestProxy entityRequestProxy, Safir.Dob.ResponseSender responseSender)
        {
            System.Console.WriteLine("- Received removed entity: " + entityRequestProxy.EntityId.ToString());
        }

        public void OnUpdateRequest(Safir.Dob.EntityRequestProxy entityRequestProxy, Safir.Dob.ResponseSender responseSender)
        {
            System.Console.WriteLine("- Received updateded entity: " + entityRequestProxy.EntityId.ToString());
        }

        #endregion

        #region EntitySubscriber Members

        public void OnDeletedEntity(Safir.Dob.EntityProxy entityProxy, bool deletedByOwner)
        {
            throw new Exception("The method or operation is not implemented.");
        }

        public void OnNewEntity(Safir.Dob.EntityProxy entityProxy)
        {
            OnUpdatedEntity(entityProxy);
        }

        public void OnUpdatedEntity(Safir.Dob.EntityProxy entityProxy)
        {
            string name = Safir.Dob.Typesystem.Operations.GetName(entityProxy.TypeId);

            if (entityProxy.TypeId == Safir.Test.TimeConversion.ClassTypeId)
            {
                Safir.Test.TimeConversion timeConv = (Safir.Test.TimeConversion)entityProxy.Entity;

                if (!timeConv.TimeStamp.IsNull())
                {
                    Double time = timeConv.TimeStamp.Val;
                    System.DateTime dateTime = Safir.Time.TimeProvider.ToDateTime(time);
                    System.Console.WriteLine(" ");
                    Print.Time(dateTime);
                }
            }
        }

        #endregion
    }
}
