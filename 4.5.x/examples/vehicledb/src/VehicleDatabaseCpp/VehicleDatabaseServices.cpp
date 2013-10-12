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

#include "VehicleDatabaseServices.h"
#include "DatabaseInteraction.h"
#include <Capabilities/Vehicles/DeleteVehicleCategoryService.h>
#include <Capabilities/Vehicles/GetVehicleCategoryService.h>
#include <Capabilities/Vehicles/SetVehicleCategoryService.h>
#include <Capabilities/Vehicles/VehicleCategoryInfo.h>

#include <Safir/Dob/ErrorListResponse.h>
#include <Safir/Dob/Response.h>
#include <Safir/Dob/SuccessResponse.h>
#include <Safir/Logging/Log.h>

namespace VehicleDatabaseCpp
{
    //--------------------------------------------------------------------------
    VehicleDatabaseServices& VehicleDatabaseServices::Instance()
    {
        static VehicleDatabaseServices  instance;
        return instance;
    }

    //--------------------------------------------------------------------------
    void  VehicleDatabaseServices::Init()
    {
        m_connection.Attach();

        //StartRemoveInExercise
        // Register as service provider.
        m_connection.RegisterServiceHandler(
            Capabilities::Vehicles::DeleteVehicleCategoryService::ClassTypeId,
            Safir::Dob::Typesystem::HandlerId(),
            this);

        m_connection.RegisterServiceHandler(
            Capabilities::Vehicles::GetVehicleCategoryService::ClassTypeId,
            Safir::Dob::Typesystem::HandlerId(),
            this);

        m_connection.RegisterServiceHandler(
            Capabilities::Vehicles::SetVehicleCategoryService::ClassTypeId,
            Safir::Dob::Typesystem::HandlerId(),
            this);
        //StopRemoveInExercise
    }

    //--------------------------------------------------------------------------
    void VehicleDatabaseServices::OnRevokedRegistration(
        const Safir::Dob::Typesystem::TypeId typeId,
        const Safir::Dob::Typesystem::HandlerId& handlerId)
    {
        // No longer registered for given type.
        Safir::Logging::SendSystemLog(Safir::Logging::Error,
                                      L"Unexpected revoked registration" +
                                      handlerId.ToString() +  L" is no longer registered for type " +
                                      Safir::Dob::Typesystem::Operations::GetName(typeId));
    }

    //--------------------------------------------------------------------------
    void  VehicleDatabaseServices::OnServiceRequest(
        const Safir::Dob::ServiceRequestProxy serviceRequestProxy,
        Safir::Dob::ResponseSenderPtr responseSender)
    {
        Safir::Dob::ResponsePtr response;

        // "dynamic_pointer_cast" returns NULL if the service isn't of the correct class, or a subclass of it.

        //StartRemoveInExercise
        // Test if a "Get" request:
        Capabilities::Vehicles::GetVehicleCategoryServicePtr getRequest =
            boost::dynamic_pointer_cast<Capabilities::Vehicles::GetVehicleCategoryService>(
            serviceRequestProxy.GetRequest());
        if (getRequest != NULL)
        {
            response = DatabaseInteraction::Instance().GetVehicleCategory(getRequest);
            responseSender -> Send(response);
            return;
        }
        //StopRemoveInExercise

        // Test if a "Set" request:
        Capabilities::Vehicles::SetVehicleCategoryServicePtr setRequest
            = boost::dynamic_pointer_cast<Capabilities::Vehicles::SetVehicleCategoryService>(
            serviceRequestProxy.GetRequest());
        if (setRequest != NULL)
        {
            response = DatabaseInteraction::Instance().SetVehicleCategory(setRequest);
            responseSender -> Send(response);
            return;
        }

        // Test if a "Delete" request:
        Capabilities::Vehicles::DeleteVehicleCategoryServicePtr  deleteRequest
            = boost::dynamic_pointer_cast<Capabilities::Vehicles::DeleteVehicleCategoryService>(
            serviceRequestProxy.GetRequest());
        if (deleteRequest != NULL)
        {
            response = DatabaseInteraction::Instance().DeleteVehicleCategory(deleteRequest);
            responseSender -> Send(response);
            return;
        }

        // Unexpected request:
        responseSender -> Send(Safir::Dob::ErrorResponse::Create());
    }

}
