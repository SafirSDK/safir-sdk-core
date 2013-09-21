/******************************************************************************
*
* Copyright Saab AB, 2008-2009 (http://www.safirsdk.com)
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
    /// Defines a service. This class handles the registration
    /// of the CalculateSpeedDifference service and processes requests.
    /// This is just an example to show how the service mechanism works,
    /// you would of course not use it in real project to calculate the
    /// difference between to speed values.
    /// </summary>

    class ServiceHandler :
        Safir.Dob.ServiceHandler
    {
        // This class uses this secondary connection for Dob calls.
        private Safir.Dob.SecondaryConnection m_connection;

        /// <summary>
        /// Constructor.
        /// </summary>
        public ServiceHandler()
        {
            m_connection = new Safir.Dob.SecondaryConnection();
        }

        /// <summary>
        /// Initiates this class. Creates a secondary DOB
        /// connection and registeres as handler.
        /// </summary>
        public void Init()
        {
            m_connection.Attach();
            // Register as service handler.
            m_connection.RegisterServiceHandler(
                Capabilities.CalculateSpeedDifference.ClassTypeId,
                new Safir.Dob.Typesystem.HandlerId(),
                this);
        }

        //
        // The following methods are derived from Safir.Dob.ServiceHandler.
        //
        public void OnRevokedRegistration(long typeId, Safir.Dob.Typesystem.HandlerId handlerId)
        {
            // No longer registered for given type.
            Safir.Logging.SendSystemLog(Safir.Logging.Severity.Critical,
                                        "Unexpected revoked registration. " +
                                        handlerId.ToString() + " is no longer registered for type " +
                                        Safir.Dob.Typesystem.Operations.GetName(typeId));
        }

        public void OnServiceRequest(
            Safir.Dob.ServiceRequestProxy serviceRequestProxy,
            Safir.Dob.ResponseSender responseSender)
        {   
            float speed, speedProperty, speedDiff;
            bool bOk = false, bPropertyOk = true;
            Safir.Dob.Entity ent;
            Capabilities.CalculateSpeedDifferenceResponse serviceResponse =
                new Capabilities.CalculateSpeedDifferenceResponse();

            // Cast to known type, the CalculateSpeedDiff service.
            Capabilities.CalculateSpeedDifference receivedService =
                (Capabilities.CalculateSpeedDifference)serviceRequestProxy.Request;

            if(!receivedService.ObjectWithSpeed.IsNull() ||
                !receivedService.Speed.IsNull())
            {
                // Retrieve the values.
                speed = receivedService.Speed.Val;
                ent = receivedService.ObjectWithSpeed.Obj;

                // Use the property mechanism to obtain the value from the
                // ObjectWithSpeed member. Be sure to check the mapping first.
                // Note, that this is just an example of how to use properties
                // and it has nothing to do with the service mechanism.
                if(Capabilities.SpeedObjectProperty.HasProperty(ent))
                {
                    // Check Speed property.
                    if(!Capabilities.SpeedObjectProperty.IsNullSpeedMember(ent))
                    {
                        // Retrieve the value from the entity by using the property.
                        speedProperty = Capabilities.SpeedObjectProperty.GetSpeedMember(ent);
                        speedDiff = speedProperty - speed;
                        serviceResponse.SpeedDifference.Val = speedDiff;
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
            if (bOk)
            {
                responseSender.Send(serviceResponse);
            }
            else if(!bPropertyOk)
            {
                // Respond with an error list that points out the member ObjectWithSpeed
                // as erroneous. This is to examplify usage of an error list.
                Safir.Dob.ErrorListResponse errorListResponse =
                    new Safir.Dob.ErrorListResponse();

                // Insert only one error in the list.
                errorListResponse.NumberOfErrors.Val = 1;

                // The error to be inserted in the error list.
                Safir.Dob.ResponseErrorInfo error =
                    new Safir.Dob.ResponseErrorInfo();

                error.Member.Val =
                    Capabilities.CalculateSpeedDifference.ObjectWithSpeedMemberIndex;
                error.Code.Val = Safir.Dob.ResponseGeneralErrorCodes.SafirReqErr;

                // Insert error in list.
                errorListResponse.Error[0].Obj = error;

                responseSender.Send(errorListResponse);
            }
            else
            {
                // Respond with a general error.
                Safir.Dob.ErrorResponse errorResponse =
                    new Safir.Dob.ErrorResponse();
                errorResponse.Code.Val = Safir.Dob.ResponseGeneralErrorCodes.SafirReqErr;
                responseSender.Send(errorResponse);
            }
        }
    }
}
