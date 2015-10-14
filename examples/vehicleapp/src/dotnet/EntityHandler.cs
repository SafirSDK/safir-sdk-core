/******************************************************************************
*
* Copyright Saab AB, 2008-2013 (http://safirsdkcore.com)
*
* Created by: Petter Lönnstedt / stpeln
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

namespace VehicleAppCs
{
    /// <summary>
    /// Defines a vehicle owner. This class handles the
    /// registration as a data owner and processes request
    /// on that data.
    /// </summary>

    class EntityHandler :
        Safir.Dob.EntityHandlerInjection
    {
        // This class uses this secondary connection for Dob calls.
        private Safir.Dob.SecondaryConnection m_connection;

        // Object counter.
        private int m_iNumberOfCreatedVehicles;

        /// <summary>
        /// Constructor.
        /// </summary>
        public EntityHandler()
        {
            m_connection = new Safir.Dob.SecondaryConnection();
            m_iNumberOfCreatedVehicles = 0;
        }

        /// <summary>
        /// Initiates this class. Creates a secondary DOB
        /// connection and registeres as handler.
        /// </summary>
        public void Init()
        {
            m_connection.Attach();
            
            // Register as vehicle entity handler.
            m_connection.RegisterEntityHandlerInjection(
                Capabilities.Vehicles.Vehicle.ClassTypeId,
                new Safir.Dob.Typesystem.HandlerId(),
                Safir.Dob.InstanceIdPolicy.Enumeration.HandlerDecidesInstanceId,
                this);
        }

        //
        // The following methods are derived from Safir.Dob.EntityHandlerInjection.
        //
        public void OnCreateRequest(
            Safir.Dob.EntityRequestProxy entityRequestProxy,
            Safir.Dob.ResponseSender responseSender)
        {
            bool bOk = false;
            Safir.Dob.Typesystem.InstanceId instanceId;
            Safir.Dob.Typesystem.EntityId entityId =
                new Safir.Dob.Typesystem.EntityId();

            // Cast to known type, the vehicle entity.
            Capabilities.Vehicles.Vehicle vehicle =
                (Capabilities.Vehicles.Vehicle)entityRequestProxy.Request;

            // Identification is a mandatory member.
            if (!vehicle.Identification.IsNull())
            {
                // Generate instance number from unique value.
                instanceId = 
                    new Safir.Dob.Typesystem.InstanceId(vehicle.Identification.Val);
                
                // Check if entity with given value already exist.
                entityId.TypeId = Capabilities.Vehicles.Vehicle.ClassTypeId;
                entityId.InstanceId = instanceId;
                
                if(!m_connection.IsCreated(entityId))
                {
                    // Store object in the Dob.
                    m_connection.SetAll(
                        vehicle, instanceId, new Safir.Dob.Typesystem.HandlerId());
                    bOk = true;
                    m_iNumberOfCreatedVehicles++;
                }
            }

            if (bOk)
            {
                // Inform requestor about the instance.
                Safir.Dob.EntityIdResponse entIdResponse =
                    new Safir.Dob.EntityIdResponse();
                entIdResponse.Assigned.Val = entityId;
                responseSender.Send(entIdResponse);
                
                // Send notification message when the number of created vehicles 
                // has reached the limit.
                if(m_iNumberOfCreatedVehicles == Capabilities.Vehicles.VehicleParameters.VehicleLimit)
                    MessageSender.Instance.SendMaxNofVehicleMsg();
            }
            else
            {
                Safir.Dob.ErrorResponse errorResponse =
                    new Safir.Dob.ErrorResponse();
                errorResponse.Code.Val = Safir.Dob.ResponseGeneralErrorCodes.SafirReqErr;
                responseSender.Send(errorResponse);
            }
        }

        public void OnUpdateRequest(
            Safir.Dob.EntityRequestProxy entityRequestProxy,
            Safir.Dob.ResponseSender responseSender)
        {
            bool bOk = false;

            // Cast to known type, the vehicle entity.
            Capabilities.Vehicles.Vehicle receivedVehicle =
                (Capabilities.Vehicles.Vehicle)entityRequestProxy.Request;
            
            if (m_connection.IsCreated(entityRequestProxy.EntityId))
            {
                // Don't allow the identification to be updated.
                if(!receivedVehicle.Identification.IsChanged())
                {
                    // Update the stored vehicle with the received one.
                    m_connection.SetChanges(
                        receivedVehicle,
                        entityRequestProxy.InstanceId,
                        new Safir.Dob.Typesystem.HandlerId());
                    bOk = true;
                }
            }
            
            if (bOk)
            {
                responseSender.Send(new Safir.Dob.SuccessResponse());
            }
            else
            {
                Safir.Dob.ErrorResponse errorResponse =
                    new Safir.Dob.ErrorResponse();
                errorResponse.Code.Val = Safir.Dob.ResponseGeneralErrorCodes.SafirReqErr;
                responseSender.Send(errorResponse);
            }
        }

        public void OnDeleteRequest(
            Safir.Dob.EntityRequestProxy entityRequestProxy,
            Safir.Dob.ResponseSender responseSender)
        {
            if(m_connection.IsCreated(entityRequestProxy.EntityId))
            {
                m_connection.Delete(
                    entityRequestProxy.EntityId,
                    new Safir.Dob.Typesystem.HandlerId());
                m_iNumberOfCreatedVehicles--;
            }
            responseSender.Send(new Safir.Dob.SuccessResponse());
        }

        public void OnRevokedRegistration(
            long typeId, Safir.Dob.Typesystem.HandlerId handlerId)
        {
            // No longer registered for given type.                                        
            Safir.Logging.SendSystemLog(Safir.Logging.Severity.Critical,
                                        "Unexpected revoked registration. " +
                                        handlerId.ToString() + " is no longer registered for type " +
                                        Safir.Dob.Typesystem.Operations.GetName(typeId));
        }

        public void OnInjectedNewEntity(
            Safir.Dob.InjectedEntityProxy injectedEntityProxy)
        {
            // The default implementation will automatically accept the object.
            m_iNumberOfCreatedVehicles++;
            // Send notification message when the number of created vehicles 
            // has reached the limit.
            if(m_iNumberOfCreatedVehicles == Capabilities.Vehicles.VehicleParameters.VehicleLimit)
                MessageSender.Instance.SendMaxNofVehicleMsg();
        }

        public void OnInjectedUpdatedEntity(
            Safir.Dob.InjectedEntityProxy injectedEntityProxy)
        {
            // Simply accept an update.
        }

        public void OnInjectedDeletedEntity(
            Safir.Dob.InjectedEntityProxy injectedEntityProxy)
        {
            // The default implementation will automatically accept the deletion.
            m_iNumberOfCreatedVehicles--;
        }

        public void OnInitialInjectionsDone(
            long typeId, Safir.Dob.Typesystem.HandlerId handlerId)
        {
            // This is just notification - no actions need to be taken.
        }
    }
}
