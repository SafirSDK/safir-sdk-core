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
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with Safir SDK Core.  If not, see <http://www.gnu.org/licenses/>.
*
******************************************************************************/
package com.saabgroup.safir.samples.vehicleappjava;

import com.saabgroup.safir.dob.ErrorListResponse;
import com.saabgroup.safir.dob.ErrorResponse;
import com.saabgroup.safir.dob.ResponseErrorInfo;
import com.saabgroup.safir.dob.ResponseGeneralErrorCodes;
import com.saabgroup.safir.dob.ResponseSender;
import com.saabgroup.safir.dob.SecondaryConnection;
import com.saabgroup.safir.dob.ServiceRequestProxy;
import com.saabgroup.safir.dob.typesystem.HandlerId;
import com.saabgroup.safir.Logging;

/**
 * Defines a service. This class handles the registration
 * of the CalculateSpeedDifference service and processes requests.
 * This is just an example to show how the service mechanism works,
 * you would of course not use it in real project to calculate the
 * difference between to speed values.
 */
public class VehicleServiceHandler implements com.saabgroup.safir.dob.ServiceHandler {

    // This class uses this secondary connection for Dob calls.
    private SecondaryConnection connection;

    /**
     * Constructor.
     */
    public VehicleServiceHandler()
    {
        connection = new SecondaryConnection();
    }
    
    /**
     * Initiates this class. Creates a secondary DOB connection and registers as handler.
     */
    public void init() {
        connection.attach();
        // Register as service handler.
        connection.registerServiceHandler(
            capabilities.CalculateSpeedDifference.ClassTypeId,
            new com.saabgroup.safir.dob.typesystem.HandlerId(),
            this);
    }
    
    @Override
    public void onRevokedRegistration(long typeId, HandlerId handlerId) {
        // No longer registered for given type.
        com.saabgroup.safir.Logging.sendSystemLog
            (com.saabgroup.safir.Logging.Severity.CRITICAL,
             "Unexpected revoked registration " +
             handlerId.toString() + " is no longer registered for type " +
             com.saabgroup.safir.dob.typesystem.Operations.getName(typeId));
    }

    @Override
    public void onServiceRequest(ServiceRequestProxy serviceRequestProxy, ResponseSender responseSender) {
        float speed, speedProperty, speedDiff;
        boolean bOk = false, bPropertyOk = true;
        com.saabgroup.safir.dob.Entity ent;
        capabilities.CalculateSpeedDifferenceResponse serviceResponse =
            new capabilities.CalculateSpeedDifferenceResponse();

        // Cast to known type, the CalculateSpeedDiff service.
        capabilities.CalculateSpeedDifference receivedService =
            (capabilities.CalculateSpeedDifference)serviceRequestProxy.getRequest();

        if(!receivedService.objectWithSpeed().isNull() ||
            !receivedService.speed().isNull())
        {
            // Retrieve the values.
            speed = receivedService.speed().getVal();
            ent = receivedService.objectWithSpeed().getObj();

            // Use the property mechanism to obtain the value from the
            // ObjectWithSpeed member. Be sure to check the mapping first.
            // Note, that this is just an example of how to use properties
            // and it has nothing to do with the service mechanism.
            if(capabilities.SpeedObjectProperty.hasProperty(ent))
            {
                // Check Speed property.
                if(!capabilities.SpeedObjectProperty.isNullSpeedMember(ent))
                {
                    // Retrieve the value from the entity by using the property.
                    speedProperty = capabilities.SpeedObjectProperty.getSpeedMember(ent);
                    speedDiff = speedProperty - speed;
                    serviceResponse.speedDifference().setVal(speedDiff);
                    bOk = true;
                }
                else
                {
                    bPropertyOk = false;
                }
            }
            else
            {
                bPropertyOk = false;
            }
        }

        // Send response.
        if (bOk) {
            responseSender.send(serviceResponse);
        }
        else if(!bPropertyOk) {
            // Respond with an error list that points out the member ObjectWithSpeed
            // as erroneous. This is to exemplify usage of an error list.
            ErrorListResponse errorListResponse = new ErrorListResponse();

            // Insert only one error in the list.
            errorListResponse.numberOfErrors().setVal(1);

            // The error to be inserted in the error list.
            ResponseErrorInfo error = new ResponseErrorInfo();

            error.member().setVal(capabilities.CalculateSpeedDifference.getObjectWithSpeedMemberIndex());
            error.code().setVal(ResponseGeneralErrorCodes.getSafirReqErr());

            // Insert error in list.
            errorListResponse.error().get(0).setObj(error);

            responseSender.send(errorListResponse);
        }
        else {
            // Respond with a general error.
            ErrorResponse errorResponse = new ErrorResponse();
            errorResponse.code().setVal(ResponseGeneralErrorCodes.getSafirReqErr());
            responseSender.send(errorResponse);
        }
    }
}
