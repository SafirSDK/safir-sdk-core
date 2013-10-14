/******************************************************************************
*
* Copyright Saab AB, 2008-2013 (http://safir.sourceforge.net)
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

#include "App.h"
#include "EntityHandler.h"
#include "MessageSender.h"
//StartRemoveInExercise
#include <Capabilities/Vehicles/Vehicle.h>
//StopRemoveInExercise
#include <Capabilities/Vehicles/VehicleParameters.h>
#include <Safir/Logging/Log.h>
#include <Safir/Dob/SuccessResponse.h>
#include <Safir/Dob/ErrorResponse.h>
#include <Safir/Dob/EntityIdResponse.h>
#include <Safir/Dob/NotFoundException.h>
#include <Safir/Dob/ResponseGeneralErrorCodes.h>



namespace VehicleAppCpp
{
    EntityHandler::EntityHandler()
    {
    }

    void EntityHandler::Init()
    {
        m_connection.Attach();

        // Register as vehicle entity handler.
        //StartRemoveInExercise
        m_connection.RegisterEntityHandlerInjection(
            Capabilities::Vehicles::Vehicle::ClassTypeId,
            Safir::Dob::Typesystem::HandlerId(),
            Safir::Dob::InstanceIdPolicy::HandlerDecidesInstanceId,
            this);
        //StopRemoveInExercise
    }

    void EntityHandler::OnRevokedRegistration(
        const Safir::Dob::Typesystem::TypeId typeId,
        const Safir::Dob::Typesystem::HandlerId& handlerId)
    {
        // No longer registered for given type.
        Safir::Logging::SendSystemLog(Safir::Logging::Critical,
                                      L"Unexpected revoked registration" +
                                      handlerId.ToString() +  L" is no longer registered for type " +
                                      Safir::Dob::Typesystem::Operations::GetName(typeId));
    }

    void EntityHandler::OnInjectedNewEntity(const Safir::Dob::InjectedEntityProxy injectedEntityProxy)
    {
        // The default implementation will automatically accept the object.

        // Send notification message when the number of created vehicles
        // has reached the limit.
        Safir::Dob::Typesystem::Int64 iNumOfCreatedInstances = 1;
            // This can by incommented as soon as the vehicle dou file is created!
            //m_connection.GetNumberOfInstances(
            //  Capabilities::Vehicles::Vehicle::ClassTypeId,
            //  Safir::Dob::Typesystem::HandlerId(),
            //  false);

        if(iNumOfCreatedInstances == Capabilities::Vehicles::VehicleParameters::VehicleLimit())
            MessageSender::Instance().SendMaxNofVehicleMsg();
    }

    void EntityHandler::OnInjectedDeletedEntity(const Safir::Dob::InjectedEntityProxy injectedEntityProxy)
    {
        // The default implementation will automatically accept the deletion.
    }

    void EntityHandler::OnInitialInjectionsDone(
        const Safir::Dob::Typesystem::TypeId typeId,
        const Safir::Dob::Typesystem::HandlerId& handlerId)
    {
        // This is just notification - no actions need to be taken.
        (void)typeId;    // fix 'unused' warning
        (void)handlerId; // fix 'unused' warning
    }

    void EntityHandler::OnCreateRequest(
        const Safir::Dob::EntityRequestProxy entityRequestProxy,
        Safir::Dob::ResponseSenderPtr responseSender)
    {
        //StartRemoveInExercise
        bool bOk = false;
        Safir::Dob::Typesystem::InstanceId instanceId;
        Safir::Dob::Typesystem::EntityId entityId;
        //StopRemoveInExercise

        // Cast to known type, the vehicle entity. Since we expect one class type only,
        // we can perform a static cast (cheaper than a dynamic cast).

        //StartRemoveInSolution
        //const Capabilities::Vehicles::VehiclePtr vehicle =
        //    boost::static_pointer_cast<Capabilities::Vehicles::Vehicle>(entityRequestProxy.GetRequest());
        //StopRemoveInSolution

        //StartRemoveInExercise
        const Capabilities::Vehicles::VehiclePtr vehicle =
            boost::static_pointer_cast<Capabilities::Vehicles::Vehicle>(entityRequestProxy.GetRequest());

        // Identification is a mandatory member.
        if (!vehicle -> Identification().IsNull())
        {
            // Generate instance number from unique value.
            instanceId = Safir::Dob::Typesystem::InstanceId(vehicle -> Identification().GetVal());

            // Check if entity with given value already exist.
            entityId = Safir::Dob::Typesystem::EntityId(
                Capabilities::Vehicles::Vehicle::ClassTypeId,
                instanceId);

            if(!m_connection.IsCreated(entityId))
            {
                // Store object in the Dob.
                m_connection.SetAll(vehicle, instanceId, Safir::Dob::Typesystem::HandlerId());
                bOk = true;
            }
        }

        if (bOk)
        {
            // Inform requestor about the instance.
            Safir::Dob::EntityIdResponsePtr entIdResponse = Safir::Dob::EntityIdResponse::Create();
            entIdResponse ->Assigned().SetVal(entityId);
            responseSender -> Send(entIdResponse);

            // Send notification message when the number of created vehicles
            // has reached the limit.
            Safir::Dob::Typesystem::Int64 iNumOfCreatedInstances = m_connection.GetNumberOfInstances(
                Capabilities::Vehicles::Vehicle::ClassTypeId,
                Safir::Dob::Typesystem::HandlerId(),
                false);

            if(iNumOfCreatedInstances == Capabilities::Vehicles::VehicleParameters::VehicleLimit())
                MessageSender::Instance().SendMaxNofVehicleMsg();
        }
        else
        {
            Safir::Dob::ErrorResponsePtr errorResponse = Safir::Dob::ErrorResponse::Create();
            errorResponse -> Code().SetVal(Safir::Dob::ResponseGeneralErrorCodes::SafirReqErr());
            responseSender -> Send(errorResponse);
        }
        //StopRemoveInExercise
    }

    void EntityHandler::OnUpdateRequest(
        const Safir::Dob::EntityRequestProxy entityRequestProxy,
        Safir::Dob::ResponseSenderPtr responseSender)
    {
        //StartRemoveInExercise
        bool bOk = false;
        //StopRemoveInExercise

        //StartRemoveInSolution
        //const Capabilities::Vehicles::VehiclePtr receivedVehicle =
        //    boost::static_pointer_cast<Capabilities::Vehicles::Vehicle>(entityRequestProxy.GetRequest());
        //StopRemoveInSolution

        //StartRemoveInExercise
        const Capabilities::Vehicles::VehiclePtr receivedVehicle =
            boost::static_pointer_cast<Capabilities::Vehicles::Vehicle>(entityRequestProxy.GetRequest());

        if (m_connection.IsCreated(entityRequestProxy.GetEntityId()))
        {

            // Don't allow the identification to be updated.
            if(!receivedVehicle -> Identification().IsChanged())
            {
                // Update the stored vehicle with the received one.
                m_connection.SetChanges(
                    receivedVehicle,
                    entityRequestProxy.GetInstanceId(),
                    Safir::Dob::Typesystem::HandlerId());
                bOk = true;
            }
        }

        if (bOk)
        {
            responseSender -> Send(Safir::Dob::SuccessResponse::Create());
        }
        else
        {
            Safir::Dob::ErrorResponsePtr errorResponse = Safir::Dob::ErrorResponse::Create();
            errorResponse -> Code().SetVal(Safir::Dob::ResponseGeneralErrorCodes::SafirReqErr());
            responseSender -> Send(errorResponse);
        }

        //StopRemoveInExercise
    }

    void EntityHandler::OnDeleteRequest(
        const Safir::Dob::EntityRequestProxy entityRequestProxy,
        Safir::Dob::ResponseSenderPtr responseSender)
    {
        //StartRemoveInSolution
        (void)entityRequestProxy; // fix 'unused' warning
        //StopRemoveInSolution

        //StartRemoveInExercise
        if (m_connection.IsCreated(entityRequestProxy.GetEntityId()))
        {
            m_connection.Delete(entityRequestProxy.GetEntityId(),
                Safir::Dob::Typesystem::HandlerId());
        }
        responseSender -> Send(Safir::Dob::SuccessResponse::Create());
        //StopRemoveInExercise
    }
 };
