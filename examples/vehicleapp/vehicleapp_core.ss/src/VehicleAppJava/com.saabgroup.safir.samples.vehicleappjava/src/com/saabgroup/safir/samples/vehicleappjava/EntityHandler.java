/******************************************************************************
*
* Copyright Saab AB, 2011 (http://www.safirsdk.com)
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
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with Safir SDK Core.  If not, see <http://www.gnu.org/licenses/>.
*
******************************************************************************/
package com.saabgroup.safir.samples.vehicleappjava;

import com.saabgroup.safir.swreports.SwReport;

/**
 * Defines a vehicle owner. This class handles the
 * registration as a data owner and processes request
 * on that data.
 */
public class EntityHandler implements com.saabgroup.safir.dob.EntityHandlerInjection {

    // This class uses this secondary connection for Dob calls.
    private com.saabgroup.safir.dob.SecondaryConnection connection;

    // Object counter.
    private int numberOfCreatedVehicles;
    
    public EntityHandler() {
        connection = new com.saabgroup.safir.dob.SecondaryConnection();
        numberOfCreatedVehicles = 0;
    }
    
    /**
     * Initiates this class. Creates a secondary Dob connection and registers as handler.
     */
    public void init() {
        connection.attach();
        
        // Register as vehicle entity handler.
        connection.registerEntityHandlerInjection(
            capabilities.vehicles.Vehicle.ClassTypeId,
            new com.saabgroup.safir.dob.typesystem.HandlerId(),
            com.saabgroup.safir.dob.InstanceIdPolicy.HANDLER_DECIDES_INSTANCE_ID,
            this);
    }

    /*
     * (non-Javadoc)
     * @see com.saabgroup.safir.dob.RevokedRegistrationBase#onRevokedRegistration(long, com.saabgroup.safir.dob.typesystem.HandlerId)
     */
    @Override
    public void onRevokedRegistration(long typeId, com.saabgroup.safir.dob.typesystem.HandlerId handlerId) {
        // No longer registered for given type.
        SwReport.SendErrorReport(
            "Unexpected revoked registration",
            "EntityHandler.OnRevokedRegistration",
            "The handler " + handlerId.toString() + " is no longer registered for type " +
            com.saabgroup.safir.dob.typesystem.Operations.getName(typeId));
    }

    /*
     * (non-Javadoc)
     * @see com.saabgroup.safir.dob.EntityInjectionBase#onInitialInjectionsDone(long, com.saabgroup.safir.dob.typesystem.HandlerId)
     */
    @Override
    public void onInitialInjectionsDone(long arg0, com.saabgroup.safir.dob.typesystem.HandlerId handlerId) {
        // This is just notification - no actions need to be taken.
    }

    /*
     * (non-Javadoc)
     * @see com.saabgroup.safir.dob.EntityInjectionBase#onInjectedDeletedEntity(com.saabgroup.safir.dob.InjectedEntityProxy)
     */
    @Override
    public void onInjectedDeletedEntity(com.saabgroup.safir.dob.InjectedEntityProxy arg0) {
        // The default implementation will automatically accept the deletion.
        numberOfCreatedVehicles--;
    }

    /*
     * (non-Javadoc)
     * @see com.saabgroup.safir.dob.EntityInjectionBase#onInjectedNewEntity(com.saabgroup.safir.dob.InjectedEntityProxy)
     */
    @Override
    public void onInjectedNewEntity(com.saabgroup.safir.dob.InjectedEntityProxy arg0) {
        // The default implementation will automatically accept the object.
        numberOfCreatedVehicles++;
        // Send notification message when the number of created vehicles 
        // has reached the limit.
        if(numberOfCreatedVehicles == capabilities.vehicles.VehicleParameters.getVehicleLimit()) {
            VehicleMessageSender.getInstance().sendMaxNofVehicleMsg();
        }
    }

    /*
     * (non-Javadoc)
     * @see com.saabgroup.safir.dob.EntityInjectionBase#onInjectedUpdatedEntity(com.saabgroup.safir.dob.InjectedEntityProxy)
     */
    @Override
    public void onInjectedUpdatedEntity(com.saabgroup.safir.dob.InjectedEntityProxy arg0) {
        // Simply accept an update.
    }

    /*
     * (non-Javadoc)
     * @see com.saabgroup.safir.dob.EntityRequestBase#onCreateRequest(com.saabgroup.safir.dob.EntityRequestProxy, com.saabgroup.safir.dob.ResponseSender)
     */
    @Override
    public void onCreateRequest(com.saabgroup.safir.dob.EntityRequestProxy entityRequestProxy, 
            com.saabgroup.safir.dob.ResponseSender responseSender) {
        boolean bOk = false;
        com.saabgroup.safir.dob.typesystem.InstanceId instanceId;
        com.saabgroup.safir.dob.typesystem.EntityId entityId = new com.saabgroup.safir.dob.typesystem.EntityId();
        
        // Cast to known type, the vehicle entity.
        capabilities.vehicles.Vehicle vehicle =
            (capabilities.vehicles.Vehicle)entityRequestProxy.getRequest();
        
        // Identification is a mandatory member.
        if (!vehicle.identification().isNull())
        {
            // Generate instance number from unique value.
            instanceId = 
                new com.saabgroup.safir.dob.typesystem.InstanceId(vehicle.identification().getVal());
            
            // Check if entity with given value already exist.
            entityId.setTypeId(capabilities.vehicles.Vehicle.ClassTypeId);
            entityId.setInstanceId(instanceId);
            
            if(!connection.isCreated(entityId))
            {
                // Store object in the Dob.
                connection.setAll(vehicle, instanceId, new com.saabgroup.safir.dob.typesystem.HandlerId());
                bOk = true;
                numberOfCreatedVehicles++;
            }
        }
        
        if (bOk)
        {
            // Inform requestor about the instance.
            com.saabgroup.safir.dob.EntityIdResponse entIdResponse =
                new com.saabgroup.safir.dob.EntityIdResponse();
            entIdResponse.assigned().setVal(entityId);
            responseSender.send(entIdResponse);
            
            // Send notification message when the number of created vehicles 
            // has reached the limit.
            if(numberOfCreatedVehicles == capabilities.vehicles.VehicleParameters.getVehicleLimit())
                VehicleMessageSender.getInstance().sendMaxNofVehicleMsg();
        }
        else
        {
            com.saabgroup.safir.dob.ErrorResponse errorResponse = new com.saabgroup.safir.dob.ErrorResponse();
            errorResponse.code().setVal(com.saabgroup.safir.dob.ResponseGeneralErrorCodes.getSafirReqErr());
            responseSender.send(errorResponse);
        }
    }

    /*
     * (non-Javadoc)
     * @see com.saabgroup.safir.dob.EntityRequestBase#onDeleteRequest(com.saabgroup.safir.dob.EntityRequestProxy, com.saabgroup.safir.dob.ResponseSender)
     */
    @Override
    public void onDeleteRequest(com.saabgroup.safir.dob.EntityRequestProxy entityRequestProxy, 
            com.saabgroup.safir.dob.ResponseSender responseSender) {
        if(connection.isCreated(entityRequestProxy.getEntityId()))
        {
            connection.delete(
                entityRequestProxy.getEntityId(),
                new com.saabgroup.safir.dob.typesystem.HandlerId());
            numberOfCreatedVehicles--;
        }
        responseSender.send(new com.saabgroup.safir.dob.SuccessResponse());
    }

    /*
     * (non-Javadoc)
     * @see com.saabgroup.safir.dob.EntityRequestBase#onUpdateRequest(com.saabgroup.safir.dob.EntityRequestProxy, com.saabgroup.safir.dob.ResponseSender)
     */
    @Override
    public void onUpdateRequest(com.saabgroup.safir.dob.EntityRequestProxy entityRequestProxy, 
            com.saabgroup.safir.dob.ResponseSender responseSender) {
        boolean bOk = false;

        // Cast to known type, the vehicle entity.
        capabilities.vehicles.Vehicle receivedVehicle =
            (capabilities.vehicles.Vehicle)entityRequestProxy.getRequest();
        
        if (connection.isCreated(entityRequestProxy.getEntityId()))
        {
            // Don't allow the identification to be updated.
            if(!receivedVehicle.identification().isChanged())
            {
                // Update the stored vehicle with the received one.
                connection.setChanges(
                    receivedVehicle,
                    entityRequestProxy.getInstanceId(),
                    new com.saabgroup.safir.dob.typesystem.HandlerId());
                bOk = true;
            }
        }
        
        if (bOk)
        {
            responseSender.send(new com.saabgroup.safir.dob.SuccessResponse());
        }
        else
        {
            com.saabgroup.safir.dob.ErrorResponse errorResponse =
                new com.saabgroup.safir.dob.ErrorResponse();
            errorResponse.code().setVal(com.saabgroup.safir.dob.ResponseGeneralErrorCodes.getSafirReqErr());
            responseSender.send(errorResponse);
        }
    }
}
