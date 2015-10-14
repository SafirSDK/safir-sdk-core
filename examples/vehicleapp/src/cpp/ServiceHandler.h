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

#ifndef __SERVICE_HANDLER_H
#define __SERVICE_HANDLER_H

#include <Safir/Dob/Connection.h>

namespace VehicleAppCpp
{
    /** 
     * Defines a service. This class handles the registration
     * of the CalculateSpeedDifference service and processes requests.
     * This is just an example to show how the service mechanism works,
     * you would of course not use it in real project to calculate the
     * difference between to speed values.
     */
    class ServiceHandler :
        // Allows this class to register as a service provider.
        public Safir::Dob::ServiceHandler
    {
    public:

        ServiceHandler();

        /** 
         * Initiates this class. Creates a secondary DOB
         * connection and registeres the service.
         */
        void Init();

        /** 
         * Methods derived from Safir::Dob::ServiceHandler.
         */
        void OnServiceRequest(
            const Safir::Dob::ServiceRequestProxy serviceRequestProxy,
            Safir::Dob::ResponseSenderPtr responseSender);

        void OnRevokedRegistration(
            const Safir::Dob::Typesystem::TypeId typeId,
            const Safir::Dob::Typesystem::HandlerId& handlerId);

    private:
        // This class uses this secondary connection for Dob calls.
        Safir::Dob::SecondaryConnection m_connection;
    };
};
#endif
