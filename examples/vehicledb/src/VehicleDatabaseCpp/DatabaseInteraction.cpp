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

#include "DatabaseInteraction.h"
#include <Capabilities/Vehicles/GetVehicleCategoryResponse.h>
#include <Capabilities/Vehicles/DatabaseParameters.h>

#include <Safir/Databases/Odbc/IntegrityConstraintException.h>
#include <Safir/Databases/Odbc/ReconnectException.h>
#include <Safir/Databases/Odbc/RetryException.h>
#include <Safir/Databases/Odbc/TimeoutException.h>
#include <Safir/Dob/ErrorResponse.h>
#include <Safir/Logging/Log.h>


namespace VehicleDatabaseCpp
{
    //--------------------------------------------------------------------------
    DatabaseInteraction  & DatabaseInteraction::Instance()
    {
        static DatabaseInteraction  instance;
        return instance;
    }

    //--------------------------------------------------------------------------
    DatabaseInteraction::DatabaseInteraction(void)
    : m_paramRemark (Capabilities::Vehicles::VehicleCategoryInfo::RemarkMaxStringLength()),
      m_columnRemark(Capabilities::Vehicles::VehicleCategoryInfo::RemarkMaxStringLength())
    {
    }

    //--------------------------------------------------------------------------
    void  DatabaseInteraction::Init()
    {
        ConnectToDatabase();
    }

    //--------------------------------------------------------------------------
    void  DatabaseInteraction::ConnectToDatabase()
    {
        try
        {
            //StartRemoveInExercise
            // Allocate an environment for databases. Made once per application.
            m_environment.Alloc();

            // Connect to the database.
            m_connection.Alloc(m_environment);
            m_connection.Connect(Capabilities::Vehicles::DatabaseParameters::ConnectionString());
            m_connection.UseManualTransactions();
            m_connection.SetConnectAttr(SQL_ATTR_CONNECTION_TIMEOUT, 5L);
            //StopRemoveInExercise

            ConnectToStoredProcedures();
        }
        catch (const Safir::Databases::Odbc::ReconnectException & ex)
        {
            // The operation cannot succeed without reconnecting to the RDBMS.
            Safir::Logging::SendSystemLog(Safir::Logging::Error,
                                          L"DatabaseError: " + ex.GetExceptionInfo());
        }
    }

    //--------------------------------------------------------------------------
    void  DatabaseInteraction::ConnectToStoredProcedures()
    {
        try
        {

            //StartRemoveInExercise
            //------------------------------------
            // Connect to stored procedure: GET
            //------------------------------------
            // Allocate a new statement in the connection.
            m_GetEntryStmt.Alloc( m_connection );

            // Connect the statement to the stored procedure.
            m_GetEntryStmt.Prepare(L"{call spGetVehicleCategory(?)}");

            // Bind the parameters of the stored procedure.
            m_GetEntryStmt.BindParameter(1, m_paramVehicleCategory);

            // Bind the entry columns returned from the stored procedure.
            m_GetEntryStmt.BindColumn(1, m_columnMaxSpeed);
            m_GetEntryStmt.BindColumn(2, m_columnIsDrivingLicenceRequired);
            m_GetEntryStmt.BindColumn(3, m_columnRemark);

            // Set the statement attributes.
            m_GetEntryStmt.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, 5L);  // Query timeout is 5 secs
            //StopRemoveInExercise
        
            //------------------------------------
            // Connect to stored procedure: SET
            //------------------------------------
            // Allocate a new statement in the connection.
            m_SetEntryStmt.Alloc( m_connection );

            // Connect the statement to the stored procedure.
            m_SetEntryStmt.Prepare(L"{call spSetVehicleCategory(?,?,?,?)}");

            // Bind the parameters of the stored procedure.
            m_SetEntryStmt.BindParameter(1, m_paramVehicleCategory);
            m_SetEntryStmt.BindParameter(2, m_paramMaxSpeed);
            m_SetEntryStmt.BindParameter(3, m_paramIsDrivingLicenceRequired);
            m_SetEntryStmt.BindParameter(4, m_paramRemark);

            // Set the statement attributes.
            m_SetEntryStmt.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, 5L);  // Query timeout is 5 secs

            //------------------------------------
            // Connect to stored procedure: DELETE
            //------------------------------------
            // Allocate a new statement in the connection.
            m_DeleteEntryStmt.Alloc( m_connection );

            // Connect the statement to the stored procedure.
            m_DeleteEntryStmt.Prepare(L"{call spDeleteVehicleCategory(?)}");
                
            // Bind the parameters of the stored procedure.
            m_DeleteEntryStmt.BindParameter(1, m_paramVehicleCategory);

            // Set the statement attributes.
            m_DeleteEntryStmt.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, 5L);  // Query timeout is 5 secs
            
        }
        catch (const Safir::Databases::Odbc::ReconnectException & ex)
        {
            // The operation cannot succeed without reconnecting to the database.
            Safir::Logging::SendSystemLog(Safir::Logging::Error,
                                          L"DatabaseError: " + ex.GetExceptionInfo());
        }
        catch (const Safir::Databases::Odbc::RetryException & ex)
        {
            // Retry the last operation.
            Safir::Logging::SendSystemLog(Safir::Logging::Error,
                                          L"DatabaseError: " + ex.GetExceptionInfo());
        }
    }

//the example has some unreachable code (the very last return statement)
//since it is also meant to be able to become an exercise.
#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4702)
#endif

    //--------------------------------------------------------------------------
    Safir::Dob::ResponsePtr  DatabaseInteraction::GetVehicleCategory
                    (Capabilities::Vehicles::GetVehicleCategoryServicePtr  request)
    {
        // Check the service request:
        if (request == NULL
            || request->VehicleCategory().IsNull())
        {
            Safir::Logging::SendSystemLog(Safir::Logging::Error,
                                          L"RequestError: VehicleCategory is missing.");

            return Safir::Dob::ErrorResponse::Create();
        }

        //StartRemoveInExercise

        // Enter the input parameters of the stored procedure.
        m_paramVehicleCategory.SetValue(request->VehicleCategory().GetVal());

        // YES, two exception handlers!
        // The outer also handles exceptions in the inner exception handler (at Rollback).
        try
        {
            try
            {
                // Execute the stored procedure.
                m_GetEntryStmt.Execute();

                // Get the entry returned from the stored procedure.
                // NOTE: If the stored procedure can return several entries,
                // loop until "Fetch" returns "false".
                if (m_GetEntryStmt.Fetch())
                {
                    Capabilities::Vehicles::GetVehicleCategoryResponsePtr  response
                        = Capabilities::Vehicles::GetVehicleCategoryResponse::Create();
                    response->VehicleCategoryInfo().SetPtr
                        (Capabilities::Vehicles::VehicleCategoryInfo::Create());

                    // Copy the search key into the service response.
                    response->VehicleCategoryInfo()->VehicleCategory().SetVal
                        (request->VehicleCategory().GetVal());

                    // Copy the column data into the service response.
                    if (!m_columnMaxSpeed.IsNull())
                    {
                        response->VehicleCategoryInfo()->MaxSpeed().SetVal
                            (m_columnMaxSpeed.GetValue());
                    }
                    if (!m_columnIsDrivingLicenceRequired.IsNull())
                    {
                        response->VehicleCategoryInfo()->IsDrivingLicenceRequired().SetVal
                            (m_columnIsDrivingLicenceRequired.GetValue());
                    }
                    if (!m_columnRemark.IsNull())
                    {
                        response->VehicleCategoryInfo()->Remark().SetVal
                            (m_columnRemark.GetValue());
                    }

                    m_connection.Commit();  // End the read transaction
                    return response;
                }

                // No result:
                m_connection.Commit();  // End the read transaction
                return Safir::Dob::ErrorResponse::Create();

            }
            catch (const Safir::Dob::Typesystem::Exception & ex)
            {
                // Safir::Databases::Odbc::IntegrityConstraintException
                // Safir::Databases::Odbc::TimeoutException
                // Safir::Databases::Odbc::RetryException
                //
                // On any of these exceptions: Retry the last operation with a timer
                // or abort the database transaction with "Rollback".
                Safir::Logging::SendSystemLog(Safir::Logging::Error,
                                              L"DatabaseError: " + ex.GetExceptionInfo());
                m_connection.Rollback();  // Abort the database transaction.
                return Safir::Dob::ErrorResponse::Create();
            }
        }
        catch (const Safir::Databases::Odbc::ReconnectException & ex)
        {
            // The operation cannot succeed without reconnecting to the database.
            Safir::Logging::SendSystemLog(Safir::Logging::Error,
                                          L"DatabaseError: " + ex.GetExceptionInfo());
            return Safir::Dob::ErrorResponse::Create();
        }

        //StopRemoveInExercise

        //StartRemoveInSolution
        // Remove this lines
        return Safir::Dob::SuccessResponse::Create();
        //StopRemoveInSolution
    }

#ifdef _MSC_VER
#pragma warning (pop)
#endif

    //--------------------------------------------------------------------------
    Safir::Dob::ResponsePtr  DatabaseInteraction::SetVehicleCategory
                    (Capabilities::Vehicles::SetVehicleCategoryServicePtr  request)
    {
        // Check the service request:
        if (request == NULL
            || request->VehicleCategoryInfo().IsNull()
            || request->VehicleCategoryInfo()->VehicleCategory().IsNull())
        {
            Safir::Logging::SendSystemLog(Safir::Logging::Error,
                                          L"RequestError: VehicleCategory is missing.");
            return Safir::Dob::ErrorResponse::Create();
        }

        // Enter the input parameters of the stored procedure.
        m_paramVehicleCategory.SetValue(request->VehicleCategoryInfo()->VehicleCategory().GetVal());
        
        if (request->VehicleCategoryInfo()->MaxSpeed().IsNull())
            m_paramMaxSpeed.SetNull();
        else
            m_paramMaxSpeed.SetValue(request->VehicleCategoryInfo()->MaxSpeed().GetVal());

        if (request->VehicleCategoryInfo()->IsDrivingLicenceRequired().IsNull())
            m_paramIsDrivingLicenceRequired.SetNull();
        else
            m_paramIsDrivingLicenceRequired.SetValue(request->VehicleCategoryInfo()->IsDrivingLicenceRequired().GetVal());
        
        if (request->VehicleCategoryInfo()->Remark().IsNull())
            m_paramRemark.SetNull();
        else
            m_paramRemark.SetValue(request->VehicleCategoryInfo()->Remark().GetVal());
        
        // YES, two exception handlers!
        // The outer also handles exceptions in the inner exception handler (at Rollback).
        try
        {
            try
            {
                // Execute the stored procedure.
                m_SetEntryStmt.Execute();

                // End the update transaction
                m_connection.Commit();
                return Safir::Dob::SuccessResponse::Create();

            }
            catch (const Safir::Dob::Typesystem::Exception & ex)
            {
                // Safir::Databases::Odbc::IntegrityConstraintException
                // Safir::Databases::Odbc::TimeoutException
                // Safir::Databases::Odbc::RetryException
                //
                // On any of these exceptions: Retry the last operation with a timer
                // or abort the database transaction with "Rollback".
                Safir::Logging::SendSystemLog(Safir::Logging::Error,
                                              L"DatabaseError: " + ex.GetExceptionInfo());
                m_connection.Rollback();  // Abort the database transaction.
                return Safir::Dob::ErrorResponse::Create();
            }
        }
        catch (const Safir::Databases::Odbc::ReconnectException & ex)
        {
            // The operation cannot succeed without reconnecting to the database.
            Safir::Logging::SendSystemLog(Safir::Logging::Error,
                                          L"DatabaseError: " + ex.GetExceptionInfo());
            return Safir::Dob::ErrorResponse::Create();
        }
    }

    //--------------------------------------------------------------------------
    Safir::Dob::ResponsePtr  DatabaseInteraction::DeleteVehicleCategory
                    (Capabilities::Vehicles::DeleteVehicleCategoryServicePtr  request)
    {
        // Check the service request:
        if (request == NULL
            || request->VehicleCategory().IsNull())
        {
            Safir::Logging::SendSystemLog(Safir::Logging::Error,
                                          L"RequestError: VehicleCategory is missing.");
            return Safir::Dob::ErrorResponse::Create();
        }

        // Enter the input parameters of the stored procedure.
        m_paramVehicleCategory.SetValue(request->VehicleCategory().GetVal());
        
        // YES, two exception handlers!
        // The outer also handles exceptions in the inner exception handler (at Rollback).
        try
        {
            try
            {
                // Execute the stored procedure.
                m_DeleteEntryStmt.Execute();

                // End the update transaction
                m_connection.Commit();
                return Safir::Dob::SuccessResponse::Create();

            }
            catch (const Safir::Dob::Typesystem::Exception & ex)
            {
                // Safir::Databases::Odbc::IntegrityConstraintException
                // Safir::Databases::Odbc::TimeoutException
                // Safir::Databases::Odbc::RetryException
                //
                // On any of these exceptions: Retry the last operation with a timer
                // or abort the database transaction with "Rollback".

                Safir::Logging::SendSystemLog(Safir::Logging::Error,
                                              L"DatabaseError: " + ex.GetExceptionInfo());
                m_connection.Rollback();  // Abort the database transaction.
                return Safir::Dob::ErrorResponse::Create();
            }
        }
        catch (const Safir::Databases::Odbc::ReconnectException & ex)
        {
            // The operation cannot succeed without reconnecting to the database.
            Safir::Logging::SendSystemLog(Safir::Logging::Error,
                                          L"DatabaseError: " + ex.GetExceptionInfo());
            return Safir::Dob::ErrorResponse::Create();
        }
    }

}
