/******************************************************************************
*
* Copyright Saab AB, 2008 (http://www.safirsdk.com)
* 
* Created by: Saab AB
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
#include <Capabilities/Vehicles/Vehicle.h>
#include <Capabilities/Animals/Animal.h>
#include <Safir/Dob/SuccessResponse.h>
#include <Safir/Dob/ErrorResponse.h>
#include <Safir/Dob/EntityIdResponse.h>
#include <Safir/Dob/NotFoundException.h>
#include <Safir/Dob/ResponseGeneralErrorCodes.h>
#include <Safir/SwReports/SwReport.h>
#include <Safir/Time/TimeProvider.h>

#include <ace/OS_NS_unistd.h>
#include <boost/date_time/posix_time/posix_time_duration.hpp>

namespace VehicleAppCpp
{
    EntityHandler::EntityHandler() : m_debug(L"ForEachTest")
    {
        m_debug.Enable(true);
    }

    void EntityHandler::Init()
    {
        m_connection.Attach();

        // Register as vehicle entity handler.

        m_connection.RegisterEntityHandler(
            Capabilities::Vehicles::Vehicle::ClassTypeId, 
            Safir::Dob::Typesystem::HandlerId(),
            Safir::Dob::InstanceIdPolicy::RequestorDecidesInstanceId,
            this);

        m_connection.RegisterEntityHandler(
            Capabilities::Animals::Animal::ClassTypeId, 
            Safir::Dob::Typesystem::HandlerId(),
            Safir::Dob::InstanceIdPolicy::RequestorDecidesInstanceId,
            this);

         m_debug << "Registered entity handlers" << std::endl;
    }

    void EntityHandler::OnRevokedRegistration(
        const Safir::Dob::Typesystem::TypeId typeId,
        const Safir::Dob::Typesystem::HandlerId& handlerId)
    {
        // No longer registered for given type.
        Safir::SwReports::SendErrorReport(
            L"Unexpected revoked registration",
            L"EntityHandler::OnRevokedRegistration",
            L"The handler " + handlerId.ToString() +  L" is no longer registered for type " + 
            Safir::Dob::Typesystem::Operations::GetName(typeId));
    }

    void EntityHandler::OnCreateRequest(
        const Safir::Dob::EntityRequestProxy entityRequestProxy,
        Safir::Dob::ResponseSenderPtr responseSender)
    {
        bool bOk = false;
        Safir::Dob::Typesystem::InstanceId instanceId;
        Safir::Dob::Typesystem::EntityId entityId;

        const Capabilities::Vehicles::VehiclePtr vehicle =
            boost::dynamic_pointer_cast<Capabilities::Vehicles::Vehicle>(entityRequestProxy.GetRequest());

        if (vehicle != NULL)
        {

            //instanceId = Safir::Dob::Typesystem::InstanceId(Safir::Dob::Typesystem::InstanceId::GenerateRandom());

            // Check if entity with given value already exist.
            entityId = Safir::Dob::Typesystem::EntityId(
                Capabilities::Vehicles::Vehicle::ClassTypeId,
                entityRequestProxy.GetInstanceId());

            if(!m_connection.IsCreated(entityId))
            {
                // Store object in the Dob.
                m_connection.SetAll(vehicle, entityRequestProxy.GetInstanceId(), Safir::Dob::Typesystem::HandlerId());
                bOk = true;
            }

            if (bOk)
            {
                // Inform requestor about the instance.
               /* Safir::Dob::EntityIdResponsePtr entIdResponse = Safir::Dob::EntityIdResponse::Create();
                entIdResponse ->Assigned().SetVal(entityId);
                responseSender -> Send(entIdResponse);*/
                responseSender -> Send(Safir::Dob::SuccessResponse::Create());
            }
            else
            {
                Safir::Dob::ErrorResponsePtr errorResponse = Safir::Dob::ErrorResponse::Create();
                errorResponse -> Code().SetVal(Safir::Dob::ResponseGeneralErrorCodes::SafirReqErr());
                responseSender -> Send(errorResponse);
            }

        }

        const Capabilities::Animals::AnimalPtr animal =
            boost::dynamic_pointer_cast<Capabilities::Animals::Animal>(entityRequestProxy.GetRequest());

        if (animal != NULL)
        {
            //instanceId = Safir::Dob::Typesystem::InstanceId(Safir::Dob::Typesystem::InstanceId::GenerateRandom());

            // Check if entity with given value already exist.
            entityId = Safir::Dob::Typesystem::EntityId(
                Capabilities::Animals::Animal::ClassTypeId,
                entityRequestProxy.GetInstanceId());

            if(!m_connection.IsCreated(entityId))
            {
                // Store object in the Dob.
                m_connection.SetAll(animal, entityRequestProxy.GetInstanceId(), Safir::Dob::Typesystem::HandlerId());
                bOk = true;
            }


            if (bOk)
            {
                // Inform requestor about the instance.
                /*Safir::Dob::EntityIdResponsePtr entIdResponse = Safir::Dob::EntityIdResponse::Create();
                entIdResponse ->Assigned().SetVal(entityId);
                responseSender -> Send(entIdResponse);*/
                responseSender -> Send(Safir::Dob::SuccessResponse::Create());
            }
            else
            {
                Safir::Dob::ErrorResponsePtr errorResponse = Safir::Dob::ErrorResponse::Create();
                errorResponse -> Code().SetVal(Safir::Dob::ResponseGeneralErrorCodes::SafirReqErr());
                responseSender -> Send(errorResponse);
            }
        }
    }

    void EntityHandler::OnUpdateRequest(
        const Safir::Dob::EntityRequestProxy entityRequestProxy,
        Safir::Dob::ResponseSenderPtr responseSender)
    {
        bool bOk = false;
        
        m_debug << "OnUpdateRequest" << std::endl;
        boost::posix_time::ptime pt = Safir::Time::TimeProvider::ToLocalTime(Safir::Time::TimeProvider::GetUtcTime());
        m_debug << boost::posix_time::to_simple_wstring(pt) << std::endl;


        const Capabilities::Vehicles::VehiclePtr vehicle =
            boost::dynamic_pointer_cast<Capabilities::Vehicles::Vehicle>(entityRequestProxy.GetRequest());

        if (vehicle != NULL)
        {

            if (m_connection.IsCreated(entityRequestProxy.GetEntityId()))
            {
                // Update the stored vehicle with the received one.
                m_debug << "Sleeping for 1 seconds" << std::endl;
                ACE_OS::sleep(1);
                m_debug << "Done sleeping" << std::endl;
                // Update the stored vehicle with the received one.
                m_connection.SetChanges(
                    vehicle,
                    entityRequestProxy.GetInstanceId(),
                    Safir::Dob::Typesystem::HandlerId());
                bOk = true;
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
        }


        const Capabilities::Animals::AnimalPtr animal =
            boost::dynamic_pointer_cast<Capabilities::Animals::Animal>(entityRequestProxy.GetRequest());

        if (animal != NULL)
        {

            if(m_connection.IsCreated(entityRequestProxy.GetEntityId()))
            {
                // Update the stored vehicle with the received one.
                m_debug << "Sleeping for 5 seconds" << std::endl;
                ACE_OS::sleep(5);
                m_debug << "Done sleeping" << std::endl;
                // Update the stored vehicle with the received one.
                m_connection.SetChanges(
                    animal,
                    entityRequestProxy.GetInstanceId(),
                    Safir::Dob::Typesystem::HandlerId());
                bOk = true;
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
        }
    }


    void EntityHandler::OnDeleteRequest(
        const Safir::Dob::EntityRequestProxy entityRequestProxy,
        Safir::Dob::ResponseSenderPtr responseSender)
    {

        m_debug << "OnDeleteRequest" << std::endl;
        boost::posix_time::ptime pt = Safir::Time::TimeProvider::ToLocalTime(Safir::Time::TimeProvider::GetUtcTime());
        m_debug << boost::posix_time::to_simple_wstring(pt) << std::endl;

        if (m_connection.IsCreated(entityRequestProxy.GetEntityId()))
        {
            if (entityRequestProxy.GetTypeId() == Capabilities::Vehicles::Vehicle::ClassTypeId)
            {
                m_debug << "Sleeping for 1 seconds" << std::endl;
                ACE_OS::sleep(1);
            }
            else
            {
                m_debug << "Sleeping for 5 seconds" << std::endl;
                ACE_OS::sleep(5);
            }
            m_debug << "Done sleeping" << std::endl;

            m_connection.Delete(entityRequestProxy.GetEntityId(), 
                Safir::Dob::Typesystem::HandlerId());
        }
        responseSender -> Send(Safir::Dob::SuccessResponse::Create());       
    }
};
