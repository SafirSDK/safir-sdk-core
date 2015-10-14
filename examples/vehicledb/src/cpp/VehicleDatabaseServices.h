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

#ifndef __GET_VEHICLE_DATABASE_SERVICES_H
#define __GET_VEHICLE_DATABASE_SERVICES_H

#include <boost/noncopyable.hpp>
#include <Safir/Dob/Connection.h>

namespace VehicleDatabaseCpp
{
    /** 
     * Defines services. This class handles the registration of the
     * Get, Set- and DeleteVehicleCategoryService services and processes.
     */
    class VehicleDatabaseServices :
        // Allows this class to register as a service provider.
        public Safir::Dob::ServiceHandler,
        private boost::noncopyable
    {
    public:

        /** 
         * Returns the one and only instance to this singleton class.
         *
         * @return The instance.
         */
        static VehicleDatabaseServices  & Instance();

        /** 
         * Initiates this class. Creates a secondary DOB
         * connection and registeres the service.
         */
        void Init();

    private:

        /** 
         * Methods derived from Safir::Dob::ServiceHandler.
         */
        void OnServiceRequest(
            const Safir::Dob::ServiceRequestProxy serviceRequestProxy,
            Safir::Dob::ResponseSenderPtr responseSender);

        void OnRevokedRegistration(
            const Safir::Dob::Typesystem::TypeId typeId,
            const Safir::Dob::Typesystem::HandlerId& handlerId);

        // This class uses this secondary connection for Dob calls.
        Safir::Dob::SecondaryConnection  m_connection;
    };
};
#endif
