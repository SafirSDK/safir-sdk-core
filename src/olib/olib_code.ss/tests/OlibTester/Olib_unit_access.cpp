/******************************************************************************
*
* Copyright Saab AB, 2005-2008 (http://www.safirsdk.com)
*
* Created by: Jörgen Johansson / stjrjo
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
#include "Olib_unit_access.h"
#include <Safir/Dob/Typesystem/Exceptions.h>
#include <iostream>
#include <Safir/Databases/Odbc/ReconnectException.h>
#include <Safir/Databases/Odbc/RetryException.h>
#include <Safir/Databases/Odbc/defs.h>

DbUnitAccess::DbUnitAccess() : 
    m_outParamCallsign(6), m_outParamUnitSizeId(50), m_outParamUnitIdentityId(100),  
    m_outParamCombatReadinessDescription(100), m_paramCallsign(6), m_paramUnitSizeId(50), 
    m_paramUnitIdentityId(50), m_paramCombatReadinessDescription(100), m_paramUnitTypeId(50), 
    m_paramData(2000), m_columnCallsign(6), m_columnUnitSizeId(50), m_columnUnitIdentityId(50), 
    m_columnCombatReadinessDescription(100), m_columnUnitTypeId(50), m_paramNClob(100), 
    m_columnNClob(1000), m_paramBinary(6000), m_columnBinary( 6000 ), m_paramBlob(100), 
    m_columnBlob(100), m_inoutParamCallsign(6), m_inoutParamUnitSizeId(50), m_inoutParamUnitIdentityId(50), 
    m_inoutParamCombatReadinessDescription(100), m_bInputOutputIsPrepared( false ), 
    m_bOutputIsPrepared( false ), m_bCreateIsPrepared( false ), m_bUpdateIsPrepared( false ), 
    m_bDeleteIsPrepared(false), m_bGetAllUnitsIsPrepared( false ), m_bPerfTestIsPrepared( false ), 
    m_bUseDbIsPrepared( false ), m_bBinaryReadIsPrepared( false ), m_bBinaryWriteIsPrepared( false ), 
    m_bWriteNClobIsPrepared(false), m_bReadNClobIsPrepared(false), m_bWriteBlobIsPrepared(false),
    m_bReadBlobIsPrepared(false), bDobIsConnected(false)   
{
}

DbUnitAccess::~DbUnitAccess(void)
{
}

void DbUnitAccess::OnDoDispatch()
{
    std::wcout << "Dispatch" << std::endl;
}

void DbUnitAccess::OnStopOrder()
{
    std::wcout << "StopOrder" << std::endl;
}

void DbUnitAccess::Connect()
{   
    std::string strTimestamp("2007-08-05 13:34");
    std::string strMsgSeqNo;
    std::string strRefMsgSeqNo;

    try
    {
        if (!bDobIsConnected)
        {
            m_dob.Open(L"OLIBTEST", L"1",0,this, this);
            bDobIsConnected = true;
        }

        if (!m_environment.IsValid())
            m_environment.Alloc();

        if (!m_connection.IsValid())
            m_connection.Alloc(m_environment);

        if (!m_connection.IsConnected())
        {   
            //m_connection.Connect(L"DSN=ExpressTest;PWD=scenario;UID=ScenarioLogin;" ); // SQLExpress
            m_connection.Connect(L"DSN=SafirDb;PWD=olibtesteruser;UID=olibtesteruser;" ); // MIMER
            //m_connection.Connect(L"DSN=MySqlNy;PWD=safir;UID=jorgen;" );
            //m_connection.Connect(L"DRIVER=SQL Server;Server=srv-sql;Database=Förbandshantering;Trusted_Connection=yes;" );
            m_connection.UseManualTransactions();
            m_connection.SetConnectAttr(SQL_ATTR_CONNECTION_TIMEOUT, 5L);
        }
    }    
    catch(const Safir::Databases::Odbc::ReconnectException & ex)
    {
        std::wcout 
            << "DbUnitAccess::Connect " 
            << ex.GetExceptionInfo()
            << std::endl;

        m_nNoOfErrors++;
    }
}

void DbUnitAccess::Disconnect()
{
    if (m_connection.IsConnected())
        m_connection.Disconnect();

    if (m_connection.IsValid()) 
        m_connection.Free();

    if (m_environment.IsValid())
        m_environment.Free();

    m_bInputOutputIsPrepared = false;
    m_bOutputIsPrepared = false;
    m_bCreateIsPrepared = false;
    m_bUpdateIsPrepared = false;
    m_bDeleteIsPrepared = false;
    m_bGetAllUnitsIsPrepared = false;
    m_bPerfTestIsPrepared = false;
    m_bUseDbIsPrepared = false;
    m_bBinaryReadIsPrepared = false;
    m_bBinaryWriteIsPrepared = false;
    m_bWriteNClobIsPrepared = false;
    m_bReadNClobIsPrepared = false;
    m_bWriteBlobIsPrepared = false;
    m_bReadBlobIsPrepared = false;
}

void DbUnitAccess::AllocStmt()
{   
    if (!m_OutputStmt.IsValid())
        m_OutputStmt.Alloc( m_connection );
    if (!m_InputOutputStmt.IsValid())
        m_InputOutputStmt.Alloc( m_connection );
    if (!m_CreateStmt.IsValid())
        m_CreateStmt.Alloc( m_connection );
    if (!m_UpdateStmt.IsValid())
        m_UpdateStmt.Alloc( m_connection );
    if (!m_DeleteStmt.IsValid())
        m_DeleteStmt.Alloc( m_connection );
    if (!m_GetAllUnitsStmt.IsValid())
        m_GetAllUnitsStmt.Alloc( m_connection );
    if (!m_BinaryReadStmt.IsValid())
        m_BinaryReadStmt.Alloc( m_connection );
    if (!m_BinaryWriteStmt.IsValid())
        m_BinaryWriteStmt.Alloc( m_connection );
    if (!m_WriteNClobStmt.IsValid())
        m_WriteNClobStmt.Alloc( m_connection );
    if (!m_ReadNClobStmt.IsValid())
        m_ReadNClobStmt.Alloc( m_connection );
    if (!m_WriteBlobStmt.IsValid())
        m_WriteBlobStmt.Alloc( m_connection );
    if (!m_ReadBlobStmt.IsValid())
        m_ReadBlobStmt.Alloc( m_connection );

    //if (!m_PerfTestStmt.IsValid())
    //  m_PerfTestStmt.Alloc( m_connection );

  //  if (!m_bPerfTestIsPrepared && m_PerfTestStmt.IsValid())
  //  {
  //      m_PerfTestStmt.Prepare(L"Insert into tblPerfTest values(?, ?, ?)");
  //      
        //m_PerfTestStmt.BindParameter(1, m_paramTypeId);
        //m_PerfTestStmt.BindParameter(2, m_paramInstanceNo);
        //m_PerfTestStmt.BindParameter(3, m_paramData);

        //m_PerfTestStmt.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, 5L);   // Query timeout is 5 secs

  //      m_bPerfTestIsPrepared = true;
  //  }

    if (!m_bOutputIsPrepared && m_OutputStmt.IsValid())
    {
        m_OutputStmt.Prepare(L"{call spOutputOlibTest (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)}");

        m_OutputStmt.BindParameter(1, m_outParamCallsign);
        m_OutputStmt.BindParameter(2, m_outParamUnitSizeId);
        m_OutputStmt.BindParameter(3, m_outParamUnitIdentityId);
        m_OutputStmt.BindParameter(4, m_outParamCombatReadiness);
        m_OutputStmt.BindParameter(5, m_outParamCombatReadinessDescription);
        m_OutputStmt.BindParameter(6, m_outParamLatitude);
        m_OutputStmt.BindParameter(7, m_outParamLongitude);
        m_OutputStmt.BindParameter(8, m_outParamSpeed);
        m_OutputStmt.BindParameter(9, m_outParamCourse);
        m_OutputStmt.BindParameter(10, m_outParamMeasurementTime);
        m_OutputStmt.BindParameter(11, m_outParamIsAlive);
        m_OutputStmt.BindParameter(12, m_outParamAlargeinteger);

        m_OutputStmt.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, 5L);   // Query timeout is 5 secs

        m_bOutputIsPrepared = true;
    }

    if (!m_bInputOutputIsPrepared && m_InputOutputStmt.IsValid())
    {
        m_InputOutputStmt.Prepare(L"{call spInputOutputOlibTest(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)}");

        m_InputOutputStmt.BindParameter(1, m_inoutParamCallsign);
        m_InputOutputStmt.BindParameter(2, m_inoutParamUnitSizeId);
        m_InputOutputStmt.BindParameter(3, m_inoutParamUnitIdentityId);
        m_InputOutputStmt.BindParameter(4, m_inoutParamCombatReadiness);
        m_InputOutputStmt.BindParameter(5, m_inoutParamCombatReadinessDescription);
        m_InputOutputStmt.BindParameter(6, m_inoutParamLatitude);
        m_InputOutputStmt.BindParameter(7, m_inoutParamLongitude);
        m_InputOutputStmt.BindParameter(8, m_inoutParamSpeed);
        m_InputOutputStmt.BindParameter(9, m_inoutParamCourse);
        m_InputOutputStmt.BindParameter(10, m_inoutParamMeasurementTime);
        m_InputOutputStmt.BindParameter(11, m_inoutParamIsAlive);
        m_InputOutputStmt.BindParameter(12, m_inoutParamAlargeinteger);

        m_InputOutputStmt.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, 5L);  // Query timeout is 5 secs

        m_bInputOutputIsPrepared = true;
    }

    if (!m_bCreateIsPrepared && m_CreateStmt.IsValid())
    {
        m_CreateStmt.Prepare(L"{call spCreateOlibTest(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)}");

        m_CreateStmt.BindParameter(1, m_paramCallsign);
        m_CreateStmt.BindParameter(2, m_paramUnitSizeId);
        m_CreateStmt.BindParameter(3, m_paramUnitIdentityId);
        m_CreateStmt.BindParameter(4, m_paramCombatReadiness);
        m_CreateStmt.BindParameter(5, m_paramCombatReadinessDescription);
        m_CreateStmt.BindParameter(6, m_paramLatitude);
        m_CreateStmt.BindParameter(7, m_paramLongitude);
        m_CreateStmt.BindParameter(8, m_paramSpeed);
        m_CreateStmt.BindParameter(9, m_paramCourse);
        m_CreateStmt.BindParameter(10, m_paramMeasurementTime);
        m_CreateStmt.BindParameter(11, m_paramIsAlive);
        m_CreateStmt.BindParameter(12, m_paramAlargeinteger);

        m_CreateStmt.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, 5L);   // Query timeout is 5 secs

        m_bCreateIsPrepared = true;
    }

    if (!m_bUpdateIsPrepared && m_UpdateStmt.IsValid())
    {
        m_UpdateStmt.Prepare(L"{call spUpdateOlibTest(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)}");

        m_UpdateStmt.BindParameter(1, m_paramUnitId);
        m_UpdateStmt.BindParameter(2, m_paramCallsign);
        m_UpdateStmt.BindParameter(3, m_paramUnitSizeId);
        m_UpdateStmt.BindParameter(4, m_paramUnitIdentityId);
        m_UpdateStmt.BindParameter(5, m_paramCombatReadiness);
        m_UpdateStmt.BindParameter(6, m_paramCombatReadinessDescription);
        m_UpdateStmt.BindParameter(7, m_paramLatitude);
        m_UpdateStmt.BindParameter(8, m_paramLongitude);
        m_UpdateStmt.BindParameter(9, m_paramSpeed);
        m_UpdateStmt.BindParameter(10, m_paramCourse);
        m_UpdateStmt.BindParameter(11, m_paramMeasurementTime);
        m_UpdateStmt.BindParameter(12, m_paramIsAlive);
        m_UpdateStmt.BindParameter(13, m_paramAlargeinteger);

        m_UpdateStmt.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, 5L);   // Query timeout is 5 secs

        m_bUpdateIsPrepared = true;
    }

    if (!m_bDeleteIsPrepared && m_DeleteStmt.IsValid())
    {
        m_DeleteStmt.Prepare(L"{call spDeleteOlibTest(?)}");

        m_DeleteStmt.BindParameter(1, m_paramUnitId);

        m_DeleteStmt.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, 5L);   // Query timeout is 5 secs

        m_bDeleteIsPrepared = true;
    }

    if (!m_bBinaryReadIsPrepared && m_BinaryReadStmt.IsValid())
    {
        m_BinaryReadStmt.Prepare(L"select id, data from tblOlibTestBinary;");

        m_BinaryReadStmt.BindColumn(1, m_columnUnitId);
        m_BinaryReadStmt.BindColumn(2, m_columnBinary);

        m_BinaryReadStmt.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, 5L);   // Query timeout is 5 secs

        m_bBinaryReadIsPrepared = true;
    }

     if (!m_bBinaryWriteIsPrepared && m_BinaryWriteStmt.IsValid())
    {
        m_BinaryWriteStmt.Prepare(L"insert into tblOlibTestBinary (id, data) values (?, ?);");

        m_BinaryWriteStmt.BindParameter(1, m_paramUnitId);
        m_BinaryWriteStmt.BindParameter(2, m_paramBinary);

        m_BinaryWriteStmt.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, 5L);  // Query timeout is 5 secs

        m_bBinaryWriteIsPrepared = true;
    }

    if (!m_bWriteNClobIsPrepared && m_WriteNClobStmt.IsValid())
    {
        m_WriteNClobStmt.Prepare(L"insert into tblOlibTestNClob (id, data) values (?, ?);");

        m_WriteNClobStmt.BindParameter(1, m_paramUnitId);
        //m_WriteNClobStmt.BindParameter(2, m_paramNClob); // This line causes software violation
        m_WriteNClobStmt.BindLongParameter(2, m_paramNClob);

        m_WriteNClobStmt.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, 5L);   // Query timeout is 5 secs

        m_bWriteNClobIsPrepared = true;
    }

    if (!m_bReadNClobIsPrepared && m_ReadNClobStmt.IsValid())
    {
        m_ReadNClobStmt.Prepare(L"select data from tblOlibTestNClob where id = ?;");

        m_ReadNClobStmt.BindParameter(1, m_paramUnitId);
        //m_ReadNClobStmt.BindColumn(1, m_columnNClob);

        m_ReadNClobStmt.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, 5L);    // Query timeout is 5 secs

        m_bReadNClobIsPrepared  = true;
    }

    if (!m_bWriteBlobIsPrepared && m_WriteBlobStmt.IsValid())
    {
        m_WriteBlobStmt.Prepare(L"insert into tblOlibTestBlob (id, data) values (?, ?);");

        m_WriteBlobStmt.BindParameter(1, m_paramUnitId);
        //m_WriteBlobStmt.BindParameter(2, m_paramBlob); // This line causes software violation
        m_WriteBlobStmt.BindLongParameter(2, m_paramBlob);

        m_WriteBlobStmt.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, 5L);    // Query timeout is 5 secs

        m_bWriteBlobIsPrepared = true;
    }

    if (!m_bReadBlobIsPrepared && m_ReadBlobStmt.IsValid())
    {
        m_ReadBlobStmt.Prepare(L"select data from tblOlibTestBlob where id = ?;");

        m_ReadBlobStmt.BindParameter(1, m_paramUnitId);
        //m_ReadBlobStmt.BindColumn(1, m_paramBlob);

        m_ReadBlobStmt.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, 5L); // Query timeout is 5 secs

        m_bReadBlobIsPrepared  = true;
    }

    if (!m_bGetAllUnitsIsPrepared && m_GetAllUnitsStmt.IsValid())
    {
        //m_GetAllUnitsStmt.Prepare(    L"select UnitId, CallSign, UnitSizeId, UnitIdentityId, CombatReadiness, "
        //                          L"CombatReadinessDescription, Latitude, Longitude, Speed, Course, "
        //                          L"MeasurementTime, IsAlive, ALargeInt from tblUnit;");
        m_GetAllUnitsStmt.Prepare(L"{call spGetAllOlibTests}");

        m_GetAllUnitsStmt.BindColumn(1, m_columnUnitId);
        m_GetAllUnitsStmt.BindColumn(2, m_columnCallsign);
        m_GetAllUnitsStmt.BindColumn(3, m_columnUnitSizeId);
        m_GetAllUnitsStmt.BindColumn(4, m_columnUnitIdentityId);
        m_GetAllUnitsStmt.BindColumn(5, m_columnCombatReadiness);
        m_GetAllUnitsStmt.BindColumn(6, m_columnCombatReadinessDescription);
        m_GetAllUnitsStmt.BindColumn(7, m_columnLatitude);
        m_GetAllUnitsStmt.BindColumn(8, m_columnLongitude);
        m_GetAllUnitsStmt.BindColumn(9, m_columnSpeed);
        m_GetAllUnitsStmt.BindColumn(10, m_columnCourse);
        m_GetAllUnitsStmt.BindColumn(11, m_columnMeasurementTime);
        m_GetAllUnitsStmt.BindColumn(12, m_columnIsAlive);
        m_GetAllUnitsStmt.BindColumn(13, m_columnAlargeinteger);

        m_GetAllUnitsStmt.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, 5L);  // Query timeout is 5 secs

        m_bGetAllUnitsIsPrepared = true;
    }
}

void DbUnitAccess::UseOlibTest()
{
    try
    {
        if (!m_UseDbStmt.IsValid())
            m_UseDbStmt.Alloc( m_connection );

        if (!m_bUseDbIsPrepared && m_UseDbStmt.IsValid())
        {
            m_UseDbStmt.Prepare(L"Use olib;");
            
            m_UseDbStmt.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, 5L);    // Query timeout is 5 secs

            m_bUseDbIsPrepared = true;
        }

        m_UseDbStmt.Execute();
    }    
    catch(const Safir::Databases::Odbc::ReconnectException & ex)
    {
        std::wcout 
            << "DbUnitAccess UseOlibTest " 
            << ex.GetExceptionInfo() 
            << std::endl;
        m_nNoOfErrors++;
    }
}

void DbUnitAccess::SetReadAllTimeout()
{
    std::wstring aString;
    long lTimeout = 1;

    std::wcout << "Timeout:";
    std::getline( std::wcin, aString );
    lTimeout = boost::lexical_cast<long>( aString.c_str() );
    m_CreateStmt.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, lTimeout);
}

long DbUnitAccess::GetReadAllTimeout()
{
    long lTimeout;
    m_CreateStmt.GetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, lTimeout);
    return lTimeout;
}

void DbUnitAccess::SetConnectionTimeout()
{
    std::wstring aString;
    long lTimeout = 1;

    std::wcout << "Timeout:";
    std::getline( std::wcin, aString );
    lTimeout = boost::lexical_cast<long>( aString.c_str() );
    m_connection.SetConnectAttr(SQL_ATTR_CONNECTION_TIMEOUT, lTimeout);
}

long DbUnitAccess::GetConnectionTimeout()
{
    long lTimeout;
    m_connection.GetConnectAttr(SQL_ATTR_CONNECTION_TIMEOUT, lTimeout);
    return lTimeout;
}

void DbUnitAccess::SetConnectionPooling()
{
    m_environment.SetEnvAttr(SQL_ATTR_CONNECTION_POOLING, SQL_CP_ONE_PER_HENV);
}

void DbUnitAccess::GetConnectionPooling()
{
    long lValue;
    m_environment.GetEnvAttr(SQL_ATTR_CONNECTION_POOLING, lValue);
    std::wcout << "SQL_ATTR_CONNECTION_POOLING: ";
    switch(lValue)
    {
    case SQL_CP_OFF :
        std::wcout << "SQL_CP_OFF" << std::endl;
        break;

    case SQL_CP_ONE_PER_DRIVER:
        std::wcout << "SQL_CP_ONE_PER_DRIVER" << std::endl;
        break;

    case SQL_CP_ONE_PER_HENV:
        std::wcout << "SQL_CP_ONE_PER_HENV" << std::endl;
        break;

    default:
        std::wcout << "Undefined value" << std::endl;
        break;
    }
}
void DbUnitAccess::CloseStmt(void)
{
    if (m_InputOutputStmt.IsValid())
    {
        m_bInputOutputIsPrepared = false;
        m_InputOutputStmt.Free();
    }

    if (m_OutputStmt.IsValid())
    {
        m_bOutputIsPrepared = false;
        m_OutputStmt.Free();
    }

    if (m_CreateStmt.IsValid())
    {
        m_bCreateIsPrepared = false;
        m_CreateStmt.Free();
    }

    if (m_UpdateStmt.IsValid())
    {
        m_bUpdateIsPrepared = false;
        m_UpdateStmt.Free();
    }

    if (m_DeleteStmt.IsValid())
    {
        m_bDeleteIsPrepared = false;
        m_DeleteStmt.Free();
    }

    if (m_GetAllUnitsStmt.IsValid())
    {
        m_bGetAllUnitsIsPrepared = false;
        m_GetAllUnitsStmt.Free();
    }

    if (m_PerfTestStmt.IsValid())
    {
        m_bPerfTestIsPrepared = false;
        m_PerfTestStmt.Free();
    }
}


void DbUnitAccess::PerfTest()
{
    if (!m_connection.IsConnected())
    {
        std::wcout << "Not Connected" << std::endl;
        return;
    }

    try
    {
        m_paramTypeId.SetValue( 1 );
        std::wstring aString(
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
            L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"  
            L"123456789012345678901234567890123456789012456789012345678901234567890123456789012345678901234567890" );
        m_paramData.SetValue( aString );
        for (int i = 0; i < 100000; ++i)
        {
            m_paramInstanceNo.SetValue( i );
            m_PerfTestStmt.Execute();
            m_connection.Commit();

        }
        ClearNoOfErrors();
    }    
    catch(const Safir::Databases::Odbc::ReconnectException & ex)
    {
        std::wcout 
            << "DbUnitAccess PerfTest " 
            << ex.GetExceptionInfo()  
            << std::endl;
        m_nNoOfErrors++;
    }

}

void DbUnitAccess::TestOutputParameters()
{
    if (!m_connection.IsConnected())
    {
        std::wcout << "Not Connected" << std::endl;
        return;
    }

    try
    {
        TIMESTAMP_STRUCT tsTime;

        m_outParamUnitId.SetValue(1);
        m_outParamCallsign.SetValue(L"None");
        m_outParamUnitSizeId.SetValue(L"None");
        m_outParamUnitIdentityId.SetValue(L"None");
        m_outParamCombatReadiness.SetValue(1);
        m_outParamCombatReadinessDescription.SetValue(L"None");
        m_outParamLatitude.SetValue(1.0);
        m_outParamLongitude.SetValue(1.0);
        m_outParamCourse.SetValue(1.0);
        m_outParamSpeed.SetValue(1.0);
        m_outParamMeasurementTime.SetNull();
        m_outParamIsAlive.SetValue(true);
        m_outParamAlargeinteger.SetValue(1);

        std::wstring aString(L"JA");
        m_paramCallsign.SetValue(aString);

        m_OutputStmt.Execute();
        if (m_outParamUnitId.IsNull())
            std::wcout << " Unit: NULL" << std::endl;
        else
            std::wcout  << "Unit:" << m_outParamUnitId.GetValue() << std::endl;
        if (m_outParamCallsign.IsNull())
            std::wcout << " CallSign: NULL" << std::endl;
        else
            std::wcout  << " CallSign:" << m_outParamCallsign.GetValue() << std::endl;
        if (m_outParamUnitSizeId.IsNull())
            std::wcout << " Size: NULL" << std::endl;
        else
            std::wcout << " Size: " << m_outParamUnitSizeId.GetValue() << std::endl;
        if (m_outParamUnitIdentityId.IsNull())
            std::wcout << " Identity: NULL" << std::endl;
        else
            std::wcout << " Identity: " << m_outParamUnitIdentityId.GetValue() << std::endl;
        if (m_outParamCombatReadiness.IsNull())
            std::wcout << " CombatReadiness: NULL" << std::endl;
        else
            std::wcout << " CombatReadiness: " << m_outParamCombatReadiness.GetValue() << std::endl;
        if (m_outParamLatitude.IsNull())
            std::wcout << " Latitude: NULL" << std::endl;
        else
            std::wcout << " Latitude: " << m_outParamLatitude.GetValue() << std::endl;
        if (m_outParamLongitude.IsNull())
            std::wcout << " Longitude: NULL" << std::endl;
        else
            std::wcout << " Longitude: " << m_outParamLongitude.GetValue() << std::endl;
        if (m_outParamSpeed.IsNull())
            std::wcout << " Speed: null" << std::endl;
        else
            std::wcout << " Speed: " << m_outParamSpeed.GetValue() << std::endl;
        if (m_outParamCourse.IsNull())
            std::wcout << " Course: NULL" << std::endl;
        else
            std::wcout << " Course: " << m_outParamCourse.GetValue() << std::endl;
        if (m_outParamIsAlive.IsNull())
            std::wcout << " IsAlive: NULL" << std::endl;
        else
            std::wcout << " IsAlive: " << m_outParamIsAlive.GetValue() << std::endl;
        if (m_outParamMeasurementTime.IsNull())
            std::wcout << " Measurementtime: NULL" << std::endl;
        else
        {
            tsTime = m_outParamMeasurementTime.GetTimeStamp();
            std::wcout  << " Measurementtime: " << static_cast<int>(tsTime.year)
                        << "-" << static_cast<int>(tsTime.month)
                        << "-" << static_cast<int>(tsTime.day)
                        << " " << static_cast<int>(tsTime.hour)
                        << ":" << static_cast<int>(tsTime.minute)
                        << ":" << static_cast<int>(tsTime.second)
                        << std::endl;
        }
        if (m_outParamAlargeinteger.IsNull())
            std::wcout << " AlargeInteger: NULL" << std::endl;
        else
            std::wcout << " AlargeInteger: " << m_outParamAlargeinteger.GetValue() << std::endl;
        std::wcout.flush();

        m_connection.Commit();
        ClearNoOfErrors();
    }
    catch(const Safir::Databases::Odbc::ReconnectException & ex)
    {
        std::wcout 
            << "DbUnitAccess CreateUnit " 
            << ex.GetExceptionInfo()  
            << std::endl;
        m_nNoOfErrors++;
    }
}

void DbUnitAccess::ReadAllUnits()
{
    if (!m_connection.IsConnected())
    {
        std::wcout << "Not Connected" << std::endl;
        return;
    }

    try
    {
        std::wstring wsCallSign = L"None";
        TIMESTAMP_STRUCT tsTime;
        Safir::Dob::Typesystem::Si64::Second time;

        m_GetAllUnitsStmt.Execute();
        while (m_GetAllUnitsStmt.Fetch())
        {
            std::wcout  << "Unit:" << m_columnUnitId.GetValue() << std::endl;
            if (m_columnCallsign.IsNull())
                std::wcout << " CallSign: NULL" << std::endl;
            else
                std::wcout  << " CallSign: '" << m_columnCallsign.GetValue() << "'" << std::endl;
            if (m_columnUnitSizeId.IsNull())
                std::wcout << " Size: NULL" << std::endl;
            else
                std::wcout << " Size: '" << m_columnUnitSizeId.GetValue() << "'" << std::endl;
            if (m_columnUnitIdentityId.IsNull())
                std::wcout << " Identity: NULL" << std::endl;
            else
                std::wcout << " Identity: '" << m_columnUnitIdentityId.GetValue() << "'" << std::endl;
            if (m_columnCombatReadiness.IsNull())
                std::wcout << " CombatReadiness: NULL" << std::endl;
            else
                std::wcout << " CombatReadiness: " << m_columnCombatReadiness.GetValue() << std::endl;
            if (m_columnLatitude.IsNull())
                std::wcout << " Latitude: NULL" << std::endl;
            else
                std::wcout << " Latitude: " << m_columnLatitude.GetValue() << std::endl;
            if (m_columnLongitude.IsNull())
                std::wcout << " Longitude: NULL" << std::endl;
            else
                std::wcout << " Longitude: " << m_columnLongitude.GetValue() << std::endl;
            if (m_columnSpeed.IsNull())
                std::wcout << " Speed: null" << std::endl;
            else
                std::wcout << " Speed: " << m_columnSpeed.GetValue() << std::endl;
            if (m_columnCourse.IsNull())
                std::wcout << " Course: NULL" << std::endl;
            else
                std::wcout << " Course: " << m_columnCourse.GetValue() << std::endl;
            if (m_columnIsAlive.IsNull())
                std::wcout << " IsAlive: NULL" << std::endl;
            else
                std::wcout << " IsAlive: " << m_columnIsAlive.GetValue() << std::endl;
            if (m_columnMeasurementTime.IsNull())
                std::wcout << " Measurementtime: NULL" << std::endl;
            else
            {
                tsTime = m_columnMeasurementTime.GetTimeStamp();
                std::wcout  << " Measurementtime: " << static_cast<int>(tsTime.year)
                            << "-" << static_cast<int>(tsTime.month)
                            << "-" << static_cast<int>(tsTime.day)
                            << " " << static_cast<int>(tsTime.hour)
                            << ":" << static_cast<int>(tsTime.minute)
                            << ":" << static_cast<int>(tsTime.second)
                            << ":" << static_cast<int>(tsTime.fraction)
                            << std::endl;
                m_columnMeasurementTime.GetValue( time );
                std::wcout <<" time ntp: " << time << std::endl;
            }
            if (m_columnAlargeinteger.IsNull())
                std::wcout << " AlargeInteger: NULL" << std::endl;
            else
                std::wcout << " AlargeInteger: " << m_columnAlargeinteger.GetValue() << std::endl;
            std::wcout.flush();
        }
        m_GetAllUnitsStmt.CloseCursor();
        
        m_connection.Commit();    // TODO: Is commit necessary when reading.
        ClearNoOfErrors();
    }
    catch(const Safir::Databases::Odbc::RetryException & ex)
    {
        std::wcout 
            << "DbUnitAccess CreateUnit " 
            << ex.GetExceptionInfo()  
            << std::endl;
        m_nNoOfErrors++;
        m_connection.Rollback();
    }
    catch(const Safir::Databases::Odbc::ReconnectException & ex)
    {
        std::wcout 
            << "DbUnitAccess CreateUnit " 
            << ex.GetExceptionInfo()  
            << std::endl;
        m_nNoOfErrors++;
        m_connection.Rollback();
    }
}

void DbUnitAccess::CreateUnit()
{
    if (!m_connection.IsConnected())
    {
        std::wcout << "Not Connected" << std::endl;
        return;
    }

    try
    {
        std::wstring aString;
        Safir::Dob::Typesystem::Float64 aFloat;
        Safir::Dob::Typesystem::Float32 aSmallFloat;
        TIMESTAMP_STRUCT tsTime;
        bool aBoolean;
        Safir::Dob::Typesystem::Int64 nAlargeinteger;
        Safir::Dob::Typesystem::Int32 nASmallinteger;

        std::wcout << "Set Callsign (y/n):";
        std::getline( std::wcin, aString );
        if (aString.compare(L"y") == 0)
        {
            std::wcout << "Callsign:";
            std::getline( std::wcin, aString );
            m_paramCallsign.SetValue( aString );
        }
        else
            m_paramCallsign.SetNull();

        std::wcout << "Set UnitSize (y/n):";
        std::getline( std::wcin, aString );
        if (aString.compare(L"y") == 0)
        {
            std::wcout << "UnitSize:";
            std::getline( std::wcin, aString );
            m_paramUnitSizeId.SetValue( aString.c_str(), static_cast<unsigned int>(aString.size()) );
        }
        else
            m_paramUnitSizeId.SetNull();

        std::wcout << "Set UnitIdentity (y/n):";
        std::getline( std::wcin, aString );
        if (aString.compare(L"y") == 0)
        {
            std::wcout << "UnitIdentity:";
            std::getline( std::wcin, aString );
            m_paramUnitIdentityId.SetValue( aString );
        }
        else
            m_paramUnitIdentityId.SetNull();

        std::wcout << "Set CombatReadiness (y/n):";
        std::getline( std::wcin, aString );
        if (aString.compare(L"y") == 0)
        {
            std::wcout << "CombatReadiness:";
            std::getline( std::wcin, aString );
            nASmallinteger = boost::lexical_cast<Safir::Dob::Typesystem::Int32>( aString.c_str() );
            m_paramCombatReadiness.SetValue( nASmallinteger );
        }
        else
            m_paramCombatReadiness.SetNull();

        std::wcout << "Set CombatReadinessDescription (y/n):";
        std::getline( std::wcin, aString );
        if (aString.compare(L"y") == 0)
        {
            std::wcout << "CombatReadinessDescription:";
            std::getline( std::wcin, aString );
            m_paramCombatReadinessDescription.SetValue( aString );
        }
        else
            m_paramCombatReadinessDescription.SetNull();

        std::wcout << "Set Position (y/n):";
        std::getline( std::wcin, aString );
        if (aString.compare(L"y") == 0)
        {
            std::wcout << "Latitude:";
            std::getline( std::wcin, aString );
            aFloat = boost::lexical_cast<Safir::Dob::Typesystem::Float64>( aString.c_str() );
            m_paramLatitude.SetValue( aFloat );

            std::wcout << "Longitude:";
            std::getline( std::wcin, aString );
            aFloat = boost::lexical_cast<Safir::Dob::Typesystem::Float64>( aString.c_str() );
            m_paramLongitude.SetValue( aFloat );
        }
        else
        {
            m_paramLatitude.SetNull();
            m_paramLongitude.SetNull();
        }

        std::wcout << "Set Course(y/n):";
        std::getline( std::wcin, aString );
        if (aString.compare(L"y") == 0)
        {
            std::wcout << "Course:";
            std::getline( std::wcin, aString );
            aSmallFloat = boost::lexical_cast<Safir::Dob::Typesystem::Float32>( aString.c_str() );
            m_paramCourse.SetValue( aSmallFloat );
        }
        else
            m_paramCourse.SetNull();

        std::wcout << "Set Speed(y/n):";
        std::getline( std::wcin, aString );
        if (aString.compare(L"y") == 0)
        {
            std::wcout << "Speed:";
            std::getline( std::wcin, aString );
            aSmallFloat = boost::lexical_cast<Safir::Dob::Typesystem::Float32>( aString.c_str() );
            m_paramSpeed.SetValue( aSmallFloat );
        }
        else
            m_paramSpeed.SetNull();

        std::wcout << "Set MeasurementTime(y/n):";
        std::getline( std::wcin, aString );
        if (aString.compare(L"y") == 0)
        {
            std::wcout << "Set current ntptime(y/n):";
            std::getline( std::wcin, aString );
            if (aString.compare(L"y") == 0)
            {
                m_paramMeasurementTime.SetValue( 
                    Safir::Time::TimeProvider::GetUtcTime() );
            }
            else
            {
                std::wcout << "Year:";
                std::getline( std::wcin, aString );
                tsTime.year = boost::lexical_cast<SQLSMALLINT>( aString.c_str() );
                std::wcout << "Month:";
                std::getline( std::wcin, aString );
                tsTime.month = boost::lexical_cast<SQLSMALLINT>( aString.c_str() );
                std::wcout << "Day:";
                std::getline( std::wcin, aString );
                tsTime.day = boost::lexical_cast<SQLSMALLINT>( aString.c_str() );
                std::wcout << "Hour:";
                std::getline( std::wcin, aString );
                tsTime.hour = boost::lexical_cast<SQLSMALLINT>( aString.c_str() );
                std::wcout << "Minute:";
                std::getline( std::wcin, aString );
                tsTime.minute = boost::lexical_cast<SQLSMALLINT>( aString.c_str() );
                std::wcout << "Second:";
                std::getline( std::wcin, aString );
                tsTime.second = boost::lexical_cast<SQLSMALLINT>( aString.c_str() );
                std::wcout << "Fraction:";
                std::getline( std::wcin, aString );
                tsTime.fraction = boost::lexical_cast<SQLUINTEGER>( aString.c_str() );

                m_paramMeasurementTime.SetTimeStamp( tsTime );
            }
        }
        else
            m_paramMeasurementTime.SetNull();

        std::wcout << "Set IsAlive(y/n):";
        std::getline( std::wcin, aString );
        if (aString.compare(L"y") == 0)
        {
            std::wcout << "IsAlive:";
            std::getline( std::wcin, aString );
            aBoolean = boost::lexical_cast<bool>( aString.c_str() ) != 0;
            m_paramIsAlive.SetValue( 1 );
            //m_paramIsAlive.SetValue( aBoolean );
        }
        else
            m_paramIsAlive.SetNull();

        std::wcout << "Set Alargeinteger(y/n):";
        std::getline( std::wcin, aString );
        if (aString.compare(L"y") == 0)
        {
            std::wcout << "Alargeinteger:";
            std::wcin >> nAlargeinteger;
            m_paramAlargeinteger.SetValue( nAlargeinteger );
        }
        else
            m_paramAlargeinteger.SetNull();

        m_CreateStmt.Execute();
        m_connection.Commit();
        ClearNoOfErrors();
    }
    catch(const Safir::Databases::Odbc::ReconnectException & ex)
    {
        std::wcout 
            << "DbUnitAccess CreateUnit " 
            << ex.GetExceptionInfo()  
            << std::endl;
        m_connection.Rollback();
    }
}

void DbUnitAccess::UpdateUnit()
{
    if (!m_connection.IsConnected())
    {
        std::wcout << "Not Connected" << std::endl;
        return;
    }

    try
    {
        std::wstring aString;
        Safir::Dob::Typesystem::Float64 aFloat;
        Safir::Dob::Typesystem::Float32 aSmallFloat;
        int nId;
        TIMESTAMP_STRUCT tsTime;
        bool aBoolean;
        Safir::Dob::Typesystem::Int32 nASmallinteger;

        std::wcout << "Unit:";
        std::getline( std::wcin, aString );
        nId = boost::lexical_cast<int>( aString.c_str() );
        m_paramUnitId.SetValue( nId );

        std::wcout << "Set Callsign (y/n):";
        std::getline( std::wcin, aString );
        if (aString.compare(L"y") == 0)
        {
            std::wcout << "Callsign:";
            std::getline( std::wcin, aString );
            m_paramCallsign.SetValue( aString );
        }
        else
            m_paramCallsign.SetNull();

        std::wcout << "Set UnitSize (y/n):";
        std::getline( std::wcin, aString );
        if (aString.compare(L"y") == 0)
        {
            std::wcout << "UnitSize:";
            std::getline( std::wcin, aString );
            m_paramUnitSizeId.SetValue( aString.c_str(), static_cast<unsigned int>(aString.size()) );
        }
        else
            m_paramUnitSizeId.SetNull();

        std::wcout << "Set UnitIdentity (y/n):";
        std::getline( std::wcin, aString );
        if (aString.compare(L"y") == 0)
        {
            std::wcout << "UnitIdentity:";
            std::getline( std::wcin, aString );
            m_paramUnitIdentityId.SetValue( aString );
        }
        else
            m_paramUnitIdentityId.SetNull();

        std::wcout << "Set CombatReadiness (y/n):";
        std::getline( std::wcin, aString );
        if (aString.compare(L"y") == 0)
        {
            std::wcout << "CombatReadiness:";
            std::getline( std::wcin, aString );
            nASmallinteger = boost::lexical_cast<Safir::Dob::Typesystem::Int32>( aString.c_str() );
            m_paramCombatReadiness.SetValue( nASmallinteger );
        }
        else
            m_paramCombatReadiness.SetNull();

        std::wcout << "Set CombatReadinessDescription (y/n):";
        std::getline( std::wcin, aString );
        if (aString.compare(L"y") == 0)
        {
            std::wcout << "CombatReadinessDescription:";
            std::getline( std::wcin, aString );
            m_paramCombatReadinessDescription.SetValue( aString );
        }
        else
            m_paramCombatReadinessDescription.SetNull();

        std::wcout << "Set Position (y/n):";
        std::getline( std::wcin, aString );
        if (aString.compare(L"y") == 0)
        {
            std::wcout << "Latitude:";
            std::getline( std::wcin, aString );
            aFloat = boost::lexical_cast<Safir::Dob::Typesystem::Float64>( aString.c_str() );
            m_paramLatitude.SetValue( aFloat );

            std::wcout << "Longitude:";
            std::getline( std::wcin, aString );
            aFloat = boost::lexical_cast<Safir::Dob::Typesystem::Float64>( aString.c_str() );
            m_paramLongitude.SetValue( aFloat );
        }
        else
        {
            m_paramLatitude.SetNull();
            m_paramLongitude.SetNull();
        }

        std::wcout << "Set Course(y/n):";
        std::getline( std::wcin, aString );
        if (aString.compare(L"y") == 0)
        {
            std::wcout << "Course:";
            std::getline( std::wcin, aString );
            aSmallFloat = boost::lexical_cast<Safir::Dob::Typesystem::Float32>( aString.c_str() );
            m_paramCourse.SetValue( aSmallFloat );
        }
        else
            m_paramCourse.SetNull();

        std::wcout << "Set Speed(y/n):";
        std::getline( std::wcin, aString );
        if (aString.compare(L"y") == 0)
        {
            std::wcout << "Speed:";
            std::getline( std::wcin, aString );
            aSmallFloat = boost::lexical_cast<Safir::Dob::Typesystem::Float32>( aString.c_str() );
            m_paramSpeed.SetValue( aSmallFloat );
        }
        else
            m_paramSpeed.SetNull();

        std::wcout << "Set MeasurementTime(y/n):";
        std::getline( std::wcin, aString );
        if (aString.compare(L"y") == 0)
        {
            std::wcout << "Set current ntptime(y/n):";
            std::getline( std::wcin, aString );
            if (aString.compare(L"y") == 0)
            {
                m_paramMeasurementTime.SetValue( 
                    Safir::Time::TimeProvider::GetUtcTime() );
            }
            else
            {
                std::wcout << "Year:";
                std::getline( std::wcin, aString );
                tsTime.year = boost::lexical_cast<SQLSMALLINT>( aString.c_str() );
                std::wcout << "Month:";
                std::getline( std::wcin, aString );
                tsTime.month = boost::lexical_cast<SQLSMALLINT>( aString.c_str() );
                std::wcout << "Day:";
                std::getline( std::wcin, aString );
                tsTime.day = boost::lexical_cast<SQLSMALLINT>( aString.c_str() );
                std::wcout << "Hour:";
                std::getline( std::wcin, aString );
                tsTime.hour = boost::lexical_cast<SQLSMALLINT>( aString.c_str() );
                std::wcout << "Minute:";
                std::getline( std::wcin, aString );
                tsTime.minute = boost::lexical_cast<SQLSMALLINT>( aString.c_str() );
                std::wcout << "Second:";
                std::getline( std::wcin, aString );
                tsTime.second = boost::lexical_cast<SQLSMALLINT>( aString.c_str() );
                std::wcout << "Fraction:";
                std::getline( std::wcin, aString );
                tsTime.fraction = boost::lexical_cast<SQLUINTEGER>( aString.c_str() );

                m_paramMeasurementTime.SetTimeStamp( tsTime );
            }
        }
        else
            m_paramMeasurementTime.SetNull();

        std::wcout << "Set IsAlive(y/n):";
        std::getline( std::wcin, aString );
        if (aString.compare(L"y") == 0)
        {
            std::wcout << "IsAlive:";
            std::getline( std::wcin, aString );
            aBoolean = boost::lexical_cast<bool>( aString.c_str() ) != 0;
            m_paramIsAlive.SetValue( aBoolean );
        }
        else
            m_paramIsAlive.SetNull();

        m_paramAlargeinteger.SetNull();

        m_UpdateStmt.Execute();
        m_connection.Commit();
        ClearNoOfErrors();
    }
    catch(const Safir::Databases::Odbc::ReconnectException & ex)
    {
        std::wcout 
            << "DbUnitAccess UpdateUnit " 
            << ex.GetExceptionInfo()  
            << std::endl;
        m_connection.Rollback();
    }
}

void DbUnitAccess::DeleteUnit()
{
    if (!m_connection.IsConnected())
    {
        std::wcout << "NotConnected" << std::endl;
        return;
    }

    try
    {
        int nId;
        std::wcout << "Unit:";
        std::wcin >> nId;
        m_paramUnitId.SetValue( nId );
        m_DeleteStmt.Execute();
        m_connection.Commit();
        ClearNoOfErrors();
    }
    catch(const Safir::Databases::Odbc::ReconnectException & ex)
    {
        std::wcout 
            << "DbUnitAccess DeleteUnit " 
            << ex.GetExceptionInfo()  
            << std::endl;
        m_connection.Rollback();
    }
    catch(const Safir::Databases::Odbc::RetryException & ex)
    {
        std::wcout 
            << "DbUnitAccess DeleteUnit " 
            << ex.GetExceptionInfo()  
            << std::endl;
        m_connection.Rollback();
    }
    catch(const Safir::Dob::Typesystem::SoftwareViolationException & ex)
    {               
        std::wcout << "DbUnitAccess DeleteUnit " << ex.GetExceptionInfo().c_str()  << std::endl;
        m_connection.Rollback();
    }
}

void DbUnitAccess::WriteNClobs()
{
    if (!m_connection.IsConnected())
    {
        std::wcout << "NotConnected" << std::endl;
        return;
    }

    try
    {
        int nId;
        int nSize;
        std::wstring aString;
        unsigned short sParameterNumber;
        std::wstring strBuffer = 
            L"012345678901234567890123456789012345678901234567890123456789"
            L"0123456789012345678901234567890123456789";

        std::wcout << L"Unit:";
        std::getline( std::wcin, aString );
        nId = boost::lexical_cast<int>( aString.c_str() );
        m_paramUnitId.SetValue( nId );

        std::wcout << L"Size:";
        std::getline( std::wcin, aString );
        nSize = boost::lexical_cast<int>( aString.c_str() );

        m_paramNClob.SetValueAtExecution(nSize * sizeof(wchar_t));
        m_WriteNClobStmt.Execute();

        m_WriteNClobStmt.ParamData(sParameterNumber);
        std::wcout << "Setting parameter " << sParameterNumber << std::endl;
        while (nSize >= 100)
        {
            m_paramNClob.SetValue( &strBuffer );
            //m_paramNClob.SetValueAtExecution( 100 );
            m_WriteNClobStmt.PutData( m_paramNClob );
            nSize -= 100;
        }
        m_WriteNClobStmt.ParamData(sParameterNumber);
        m_connection.Commit();
    }
    catch(const Safir::Databases::Odbc::ReconnectException & ex)
    {
        std::wcout 
            << "DbUnitAccess BinaryTest " 
            << ex.GetExceptionInfo()  
            << std::endl;
        m_connection.Rollback();
    }
}

void DbUnitAccess::ReadNClobs()
{
    if (!m_connection.IsConnected())
    {
        std::wcout << "NotConnected" << std::endl;
        return;
    }

    try
    {
        int nId;
        std::wstring aString;

        std::wcout << L"Unit:";
        std::getline( std::wcin, aString );
        nId = boost::lexical_cast<int>( aString.c_str() );
        m_paramUnitId.SetValue( nId );

        m_ReadNClobStmt.Execute();
        while (m_ReadNClobStmt.Fetch())
        {
            std::wcout << L"New row " << std::endl;
            while (m_ReadNClobStmt.GetData(1, m_columnNClob))
            {
                if (m_columnNClob.IsRetrievedSizeAvailable())
                    std::wcout << L"Data read " << m_columnNClob.GetRetrievedSize() << std::endl;

                std::wcout << m_columnNClob.GetValue() << std::endl;
            }
        }
        m_ReadNClobStmt.CloseCursor();
        m_connection.Commit();
    }
    catch(const Safir::Databases::Odbc::ReconnectException & ex)
    {
        std::wcout 
            << "DbUnitAccess BinaryTest " 
            << ex.GetExceptionInfo()  
            << std::endl;
        m_connection.Rollback();
    }
}

void DbUnitAccess::WriteBlob()
{
    if (!m_connection.IsConnected())
    {
        std::wcout << "NotConnected" << std::endl;
        return;
    }

    try
    {
        int nId;
        int nSize;
        std::wstring aString;
        unsigned short sParameterNumber;
        Safir::Databases::Odbc::byte strBuffer[] = 
            "012345678901234567890123456789012345678901234567890123456789"
            "0123456789012345678901234567890123456789";

        std::wcout << L"Unit:";
        std::getline( std::wcin, aString );
        nId = boost::lexical_cast<int>( aString.c_str() );
        m_paramUnitId.SetValue( nId );

        std::wcout << L"Size:";
        std::getline( std::wcin, aString );
        nSize = boost::lexical_cast<int>( aString.c_str() );

        m_paramBlob.SetValueAtExecution(nSize * sizeof(unsigned char));
        m_WriteBlobStmt.Execute();

        m_WriteBlobStmt.ParamData(sParameterNumber);
        std::wcout << "Setting parameter " << sParameterNumber << std::endl;
        while (nSize >= 100)
        {
            m_paramBlob.SetValue( strBuffer , (nSize > 100 ? 100 : nSize) );
            //m_paramNClob.SetValueAtExecution( 100 );
            m_WriteBlobStmt.PutData( m_paramBlob );
            nSize -= 100;
        }
        m_WriteBlobStmt.ParamData(sParameterNumber);
        m_connection.Commit();
    }
    catch(const Safir::Databases::Odbc::ReconnectException & ex)
    {
        std::wcout 
            << "DbUnitAccess BinaryTest " 
            << ex.GetExceptionInfo()  
            << std::endl;
        m_connection.Rollback();
    }
}

void DbUnitAccess::ReadBlob()
{
    if (!m_connection.IsConnected())
    {
        std::wcout << "NotConnected" << std::endl;
        return;
    }

    try
    {
        int nId;
        std::wstring aString;

        std::wcout << L"Unit:";
        std::getline( std::wcin, aString );
        nId = boost::lexical_cast<int>( aString.c_str() );
        m_paramUnitId.SetValue( nId );

        m_ReadBlobStmt.Execute();
        while (m_ReadBlobStmt.Fetch())
        {
            std::wcout << L"New row " << std::endl;
            while (m_ReadBlobStmt.GetData(1, m_columnBlob))
            {
                if (m_columnBlob.IsRetrievedSizeAvailable())
                    std::wcout << L"Data read " << m_columnBlob.GetRetrievedSize() << std::endl;

                std::wcout << reinterpret_cast<char *>(m_columnBlob.GetValue()) << std::endl;
            }
        }
        m_ReadBlobStmt.CloseCursor();
        m_connection.Commit();
    }
    catch(const Safir::Databases::Odbc::ReconnectException & ex)
    {
        std::wcout 
            << "DbUnitAccess BinaryTest " 
            << ex.GetExceptionInfo()  
            << std::endl;
        m_connection.Rollback();
    }
}

void DbUnitAccess::TestInputOutputParameters()
{
    if (!m_connection.IsConnected())
    {
        std::wcout << "Not Connected" << std::endl;
        return;
    }

    try
    {
        std::wstring aString;
        Safir::Dob::Typesystem::Float64 aFloat;
        Safir::Dob::Typesystem::Float32 aSmallFloat;
        SQLSMALLINT anInt;
        int nId;
        TIMESTAMP_STRUCT tsTime;
        bool aBoolean;
        Safir::Dob::Typesystem::Int64 nAlargeinteger;
        Safir::Dob::Typesystem::Int32 nASmallinteger;

        std::wcout << "Unit:";
        std::wcin >> nId;
        m_inoutParamUnitId.SetValue( nId );

        std::wcout << "Set Callsign (y/n):";
        std::wcin >> aString;
        if (aString.compare(L"y") == 0)
        {
            std::wcout << "Callsign:";
            std::wcin >> aString;
            m_inoutParamCallsign.SetValue( aString );
        }
        else
            m_inoutParamCallsign.SetNull();

        std::wcout << "Set UnitSize (y/n):";
        std::wcin >> aString;
        if (aString.compare(L"y") == 0)
        {
            std::wcout << "UnitSize:";
            std::wcin >> aString;
            m_inoutParamUnitSizeId.SetValue( aString );
        }
        else
            m_inoutParamUnitSizeId.SetNull();

        std::wcout << "Set UnitIdentity (y/n):";
        std::wcin >> aString;
        if (aString.compare(L"y") == 0)
        {
            std::wcout << "UnitIdentity:";
            std::wcin >> aString;
            m_inoutParamUnitIdentityId.SetValue( aString );
        }
        else
            m_inoutParamUnitIdentityId.SetNull();

        std::wcout << "Set CombatReadiness (y/n):";
        std::wcin >> aString;
        if (aString.compare(L"y") == 0)
        {
            std::wcout << "CombatReadiness:";
            std::wcin >> nASmallinteger;
            m_inoutParamCombatReadiness.SetValue( nASmallinteger );
        }
        else
            m_inoutParamCombatReadiness.SetNull();

        std::wcout << "Set CombatReadinessDescription (y/n):";
        std::wcin >> aString;
        if (aString.compare(L"y") == 0)
        {
            std::wcout << "CombatReadinessDescription:";
            std::wcin >> aString;
            m_inoutParamCombatReadinessDescription.SetValue( aString );
        }
        else
            m_inoutParamCombatReadinessDescription.SetNull();

        std::wcout << "Set Position (y/n):";
        std::wcin >> aString;
        if (aString.compare(L"y") == 0)
        {
            std::wcout << "Latitude:";
            std::wcin >> aFloat;
            m_inoutParamLatitude.SetValue( aFloat );

            std::wcout << "Longitude:";
            std::wcin >> aFloat;
            m_inoutParamLongitude.SetValue( aFloat );
        }
        else
        {
            m_inoutParamLatitude.SetNull();
            m_inoutParamLongitude.SetNull();
        }

        std::wcout << "Set Course(y/n):";
        std::wcin >> aString;
        if (aString.compare(L"y") == 0)
        {
            std::wcout << "Course:";
            std::wcin >> aSmallFloat;
            m_inoutParamCourse.SetValue( aSmallFloat );
        }
        else
            m_inoutParamCourse.SetNull();

        std::wcout << "Set Speed(y/n):";
        std::wcin >> aString;
        if (aString.compare(L"y") == 0)
        {
            std::wcout << "Speed:";
            std::wcin >> aSmallFloat;
            m_inoutParamSpeed.SetValue( aSmallFloat );
        }
        else
            m_inoutParamSpeed.SetNull();

        std::wcout << "Set MeasurementTime(y/n):";
        std::wcin >> aString;
        if (aString.compare(L"y") == 0)
        {
            std::wcout << "Year:";
            std::wcin >> anInt;
            tsTime.year = anInt;
            std::wcout << "Month:";
            std::wcin >> anInt;
            tsTime.month = anInt;
            std::wcout << "Day:";
            std::wcin >> anInt;
            tsTime.day = anInt;
            std::wcout << "Hour:";
            std::wcin >> anInt;
            tsTime.hour = anInt;
            std::wcout << "Minute:";
            std::wcin >> anInt;
            tsTime.minute = anInt;
            std::wcout << "Second:";
            std::wcin >> anInt;
            tsTime.second = anInt;
            std::wcout << "Fraction:";
            std::wcin >> anInt;
            tsTime.fraction = anInt;

            m_inoutParamMeasurementTime.SetTimeStamp( tsTime );
        }
        else
            m_inoutParamMeasurementTime.SetNull();

        std::wcout << "Set IsAlive(y/n):";
        std::wcin >> aString;
        if (aString.compare(L"y") == 0)
        {
            std::wcout << "IsAlive:";
            std::wcin >> aBoolean;
            m_inoutParamIsAlive.SetValue( aBoolean );
        }
        else
            m_inoutParamIsAlive.SetNull();

        std::wcout << "Set Alargeinteger(y/n):";
        std::wcin >> aString;
        if (aString.compare(L"y") == 0)
        {
            std::wcout << "Alargeinteger:";
            std::wcin >> nAlargeinteger;
            m_inoutParamAlargeinteger.SetValue( nAlargeinteger );
        }
        else
            m_inoutParamAlargeinteger.SetNull();

        m_InputOutputStmt.Execute();
        //m_InputOutputStmt.Fetch();
        m_InputOutputStmt.MoreResults();

        if (m_inoutParamUnitId.IsNull())
            std::wcout << " Unit: NULL" << std::endl;
        else
            std::wcout  << "Unit:" << m_inoutParamUnitId.GetValue() << std::endl;
        if (m_inoutParamCallsign.IsNull())
            std::wcout << " CallSign: NULL" << std::endl;
        else
            std::wcout  << " CallSign:" << m_inoutParamCallsign.GetValue() << std::endl;
        if (m_inoutParamUnitSizeId.IsNull())
            std::wcout << " Size: NULL" << std::endl;
        else
            std::wcout << " Size: " << m_inoutParamUnitSizeId.GetValue() << std::endl;
        if (m_inoutParamUnitIdentityId.IsNull())
            std::wcout << " Identity: NULL" << std::endl;
        else
            std::wcout << " Identity: " << m_inoutParamUnitIdentityId.GetValue() << std::endl;
        if (m_inoutParamCombatReadiness.IsNull())
            std::wcout << " CombatReadiness: NULL" << std::endl;
        else
            std::wcout << " CombatReadiness: " << m_inoutParamCombatReadiness.GetValue() << std::endl;
        if (m_inoutParamCombatReadinessDescription.IsNull())
            std::wcout << " CombatReadinessDescription: NULL" << std::endl;
        else
            std::wcout << " CombatReadinessDescription: " << m_inoutParamCombatReadinessDescription.GetValue() << std::endl;
        if (m_inoutParamLatitude.IsNull())
            std::wcout << " Latitude: NULL" << std::endl;
        else
            std::wcout << " Latitude: " << m_inoutParamLatitude.GetValue() << std::endl;
        if (m_inoutParamLongitude.IsNull())
            std::wcout << " Longitude: NULL" << std::endl;
        else
            std::wcout << " Longitude: " << m_inoutParamLongitude.GetValue() << std::endl;
        if (m_inoutParamSpeed.IsNull())
            std::wcout << " Speed: null" << std::endl;
        else
            std::wcout << " Speed: " << m_inoutParamSpeed.GetValue() << std::endl;
        if (m_inoutParamCourse.IsNull())
            std::wcout << " Course: NULL" << std::endl;
        else
            std::wcout << " Course: " << m_inoutParamCourse.GetValue() << std::endl;
        if (m_inoutParamIsAlive.IsNull())
            std::wcout << " IsAlive: NULL" << std::endl;
        else
            std::wcout << " IsAlive: " << m_inoutParamIsAlive.GetValue() << std::endl;
        if (m_inoutParamMeasurementTime.IsNull())
            std::wcout << " Measurementtime: NULL" << std::endl;
        else
        {
            tsTime = m_inoutParamMeasurementTime.GetTimeStamp();
            std::wcout  << " Measurementtime: " << static_cast<int>(tsTime.year)
                        << "-" << static_cast<int>(tsTime.month)
                        << "-" << static_cast<int>(tsTime.day)
                        << " " << static_cast<int>(tsTime.hour)
                        << ":" << static_cast<int>(tsTime.minute)
                        << ":" << static_cast<int>(tsTime.second)
                        << std::endl;
        }
        if (m_inoutParamAlargeinteger.IsNull())
            std::wcout << " AlargeInteger: NULL" << std::endl;
        else
            std::wcout << " AlargeInteger: " << m_inoutParamAlargeinteger.GetValue() << std::endl;
        std::wcout.flush();
        m_connection.Commit();
        ClearNoOfErrors();
    }
    catch(const Safir::Databases::Odbc::ReconnectException & ex)
    {
        std::wcout 
            << "DbUnitAccess UpdateUnit " 
            << ex.GetExceptionInfo()  
            << std::endl;
        m_connection.Rollback();
    }
}

void DbUnitAccess::BinaryTestRead()
{
    unsigned char * pData = NULL;

    if (!m_connection.IsConnected())
    {
        std::wcout << "NotConnected" << std::endl;
        return;
    }

    try
    {
        std::wstring aString;

        m_BinaryReadStmt.Execute();
        while (m_BinaryReadStmt.Fetch())
        {
            if (m_columnUnitId.IsNull())
                std::wcout << " Unit: NULL" << std::endl;
            else
                std::wcout  << "Unit:" << m_columnUnitId.GetValue() << std::endl;

            if (m_columnBinary.IsNull())
                std::wcout << " Data: NULL" << std::endl;
            else
                std::wcout  << "Data:" << reinterpret_cast<char *>(m_columnBinary.GetValue()) << std::endl;
        }
        m_BinaryReadStmt.CloseCursor();
        m_connection.Commit();
        delete [] pData;
        ClearNoOfErrors();
    }
    catch(const Safir::Databases::Odbc::ReconnectException & ex)
    {
        std::wcout 
            << "DbUnitAccess BinaryTestRead " 
            << ex.GetExceptionInfo()  
            << std::endl;
        m_connection.Rollback();
    }

}

void DbUnitAccess::BinaryTestWrite()
{
    Safir::Databases::Odbc::byte * pData = NULL;

    if (!m_connection.IsConnected())
    {
        std::wcout << "NotConnected" << std::endl;
        return;
    }

    try
    {
        int nId;
        int nSize;
        std::wstring aString;

        std::wcout << "Unit:";
        std::getline( std::wcin, aString );
        nId = boost::lexical_cast<int>( aString.c_str() );
        m_paramUnitId.SetValue( nId );
        std::wcout << "Size:";
        std::getline( std::wcin, aString );
        nSize = boost::lexical_cast<int>( aString.c_str() );
        if (nSize == 0)
        {
            m_paramBinary.SetNull();
        }
        else
        {
            pData = new Safir::Databases::Odbc::byte[nSize];
            memset(pData, 42, nSize);
            m_paramBinary.SetValue( pData, nSize );
        }
        m_BinaryWriteStmt.Execute();
        m_connection.Commit();
        delete [] pData;
        ClearNoOfErrors();
    }
    catch(const Safir::Databases::Odbc::ReconnectException & ex)
    {
        std::wcout 
            << "DbUnitAccess BinaryTestWrite " 
            << ex.GetExceptionInfo()  
            << std::endl;
        m_connection.Rollback();
    }
}

char * DbUnitAccess::BinaryData = 
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"

    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
    "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890";

