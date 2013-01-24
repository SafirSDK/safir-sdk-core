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

#include "ServiceHandler.h"
//StartRemoveInExercise
#include <Capabilities/CalculateSpeedDifference.h>
#include <Capabilities/CalculateSpeedDifferenceResponse.h>
//StopRemoveInExercise
#include <Capabilities/SpeedObjectProperty.h>
#include <Safir/Dob/ErrorListResponse.h>
#include <Safir/Dob/ResponseErrorInfo.h>
#include <Safir/Dob/ResponseGeneralErrorCodes.h>
#include <Safir/SwReports/SwReport.h>

namespace VehicleAppCpp
{
    ServiceHandler::ServiceHandler()
    {
    }

    void ServiceHandler::Init()
    {
        m_connection.Attach();

        // Register as service handler.

        //StartRemoveInExercise
        m_connection.RegisterServiceHandler(
            Capabilities::CalculateSpeedDifference::ClassTypeId,
            Safir::Dob::Typesystem::HandlerId(),
            this);
        //StopRemoveInExercise
    }

    void ServiceHandler::OnRevokedRegistration(
        const Safir::Dob::Typesystem::TypeId typeId,
        const Safir::Dob::Typesystem::HandlerId& handlerId)
    {
        // No longer registered for given type.
       Safir::SwReports::SendErrorReport(
           L"Unexpected revoked registration", L"ServiceHandler::OnRevokedRegistration",
           L"The handler " + handlerId.ToString() +  L" is no longer registered for type " +
           Safir::Dob::Typesystem::Operations::GetName(typeId));
    }

    void ServiceHandler::OnServiceRequest(
        const Safir::Dob::ServiceRequestProxy serviceRequestProxy,
        Safir::Dob::ResponseSenderPtr responseSender )
    {
        Safir::Dob::Typesystem::Si32::MeterPerSecond speed, speedProperty, speedDiff;
        bool bOk = false, bPropertyOk = true;
        Safir::Dob::EntityPtr ent;

        //StartRemoveInSolution
        (void)speed;         // fix 'unused' warning
        (void)speedProperty; // fix 'unused' warning
        (void)speedDiff;     // fix 'unused' warning
        (void)bOk;           // fix 'unused' warning
        (void)bPropertyOk;   // fix 'unused' warning
        (void)ent;           // fix 'unused' warning
        //StopRemoveInSolution

        // Cast to known type, the CalculateSpeedDiff service. Since we expect one class type only,
        // we can perform a static cast (cheaper than a dynamic cast).

        //StartRemoveInSolution
        //const Capabilities::CalculateSpeedDifferencePtr receivedService =
        //    boost::static_pointer_cast<Capabilities::CalculateSpeedDifference>(serviceRequestProxy.GetRequest());
        //StopRemoveInSolution

        //StartRemoveInExercise
        Capabilities::CalculateSpeedDifferenceResponsePtr serviceResponse =
                        Capabilities::CalculateSpeedDifferenceResponse::Create();

        const Capabilities::CalculateSpeedDifferencePtr receivedService =
            boost::static_pointer_cast<Capabilities::CalculateSpeedDifference>(serviceRequestProxy.GetRequest());

        if(!receivedService -> ObjectWithSpeed().IsNull() || !receivedService -> Speed().IsNull())
        {
            // Retrieve the values.
            speed = receivedService -> Speed().GetVal();
            ent = receivedService -> ObjectWithSpeed().GetPtr();

            // Use the property mechanism to obtain the value from the
            // ObjectWithSpeed member. Be sure to check the mapping first.
            // Note, that this is just an example of how to use properties
            // and it has nothing to do with the service mechanism.
            if(Capabilities::SpeedObjectProperty::HasProperty(ent))
            {
                // Check Speed property.
                if(!Capabilities::SpeedObjectProperty::IsNullSpeedMember(ent))
                {
                    // Retrieve the value from the entity by using the property.
                    speedProperty = Capabilities::SpeedObjectProperty::GetSpeedMember(ent);
                    speedDiff = speedProperty - speed;
                    serviceResponse -> SpeedDifference().SetVal(speedDiff);
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
            responseSender -> Send(serviceResponse);
        }
        else if(!bPropertyOk)
        {
            // Respond with an error list that points out the member ObjectWithSpeed
            // as erroneous. This is to examplify usage of an error list.
            Safir::Dob::ErrorListResponsePtr errorListResponse =
                Safir::Dob::ErrorListResponse::Create();

            // Insert only one error in the list.
            errorListResponse -> NumberOfErrors().SetVal(1);

            // The error to be inserted in the error list.
            Safir::Dob::ResponseErrorInfoPtr error =
                Safir::Dob::ResponseErrorInfo::Create();

            error -> Member().SetVal(Capabilities::CalculateSpeedDifference::ObjectWithSpeedMemberIndex());
            error -> Code().SetVal(Safir::Dob::ResponseGeneralErrorCodes::SafirReqErr());

            // Insert error in list.
            errorListResponse -> Error()[0].SetPtr(error);

            responseSender -> Send(errorListResponse);
        }
        else
        {
            // Respond with a general error.
            Safir::Dob::ErrorResponsePtr errorResponse = Safir::Dob::ErrorResponse::Create();
            errorResponse -> Code().SetVal(Safir::Dob::ResponseGeneralErrorCodes::SafirReqErr());
            responseSender -> Send(errorResponse);
        }
        //StopRemoveInExercise
    }
};
