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

#ifndef __DATABASE_INTERACTION_H
#define __DATABASE_INTERACTION_H

#include <Capabilities/Vehicles/DeleteVehicleCategoryService.h>
#include <Capabilities/Vehicles/GetVehicleCategoryService.h>
#include <Capabilities/Vehicles/SetVehicleCategoryService.h>

#include <boost/noncopyable.hpp>
#include <Safir/Databases/Odbc/Columns.h>
#include <Safir/Databases/Odbc/Connection.h>
#include <Safir/Databases/Odbc/Environment.h>
#include <Safir/Databases/Odbc/InputParameter.h>
#include <Safir/Databases/Odbc/Statement.h>
#include <Safir/Dob/Response.h>

namespace VehicleDatabaseCpp
{

    class DatabaseInteraction :
        private boost::noncopyable
    {
    public:
        static DatabaseInteraction  & Instance();

        void  Init();

        // The provided database operations.
        // This class hides the data representation of the database interface.
        // DOB objects is selected as the suitable data representation here for
        // this example application.
        Safir::Dob::ResponsePtr  GetVehicleCategory
                    (Capabilities::Vehicles::GetVehicleCategoryServicePtr  request);
        Safir::Dob::ResponsePtr  SetVehicleCategory
                    (Capabilities::Vehicles::SetVehicleCategoryServicePtr  request);
        Safir::Dob::ResponsePtr  DeleteVehicleCategory
                    (Capabilities::Vehicles::DeleteVehicleCategoryServicePtr  request);

    private:
        DatabaseInteraction(void);

        // The database connection.
        Safir::Databases::Odbc::Environment  m_environment;
        Safir::Databases::Odbc::Connection   m_connection;

        void  ConnectToDatabase();

        // The storded SQL procedures.
        Safir::Databases::Odbc::Statement  m_GetEntryStmt;
        Safir::Databases::Odbc::Statement  m_SetEntryStmt;
        Safir::Databases::Odbc::Statement  m_DeleteEntryStmt;

        // The parameters of the stored procedures.
        Safir::Databases::Odbc::Int32Parameter       m_paramVehicleCategory;
        Safir::Databases::Odbc::Float32Parameter     m_paramMaxSpeed;
        Safir::Databases::Odbc::BooleanParameter     m_paramIsDrivingLicenceRequired;
        Safir::Databases::Odbc::WideStringParameter  m_paramRemark;

        // The entry columns returned from the stored procedures.
        Safir::Databases::Odbc::Int32Column          m_columnVehicleCategory;
        Safir::Databases::Odbc::Float32Column        m_columnMaxSpeed;
        Safir::Databases::Odbc::BooleanColumn        m_columnIsDrivingLicenceRequired;
        Safir::Databases::Odbc::WideStringColumn     m_columnRemark;

        void  ConnectToStoredProcedures();

    };

}

#endif
