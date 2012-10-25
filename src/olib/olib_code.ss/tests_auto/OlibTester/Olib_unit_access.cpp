/******************************************************************************
*
* Copyright Saab AB, 2005-2008 (http://www.safirsdk.com)
*
* Created by: Jörgen Johansson / stjrjo
* Modified by: Amin Allalou 2012
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
#include <safir/Dob/Typesystem/Serialization.h>
#include <safir/Dob/Typesystem/BlobOperations.h>
#include <safir/Dob/Typesystem/ObjectFactory.h>


DbUnitAccess::DbUnitAccess() : 
    m_outParamCallsign(6), m_outParamUnitSizeId(50), m_outParamUnitIdentityId(100), m_paramUnitId(1), 
    m_outParamCombatReadinessDescription(100), m_paramCallsign(6), m_paramUnitSizeId(50), 
    m_paramUnitIdentityId(50), m_paramCombatReadinessDescription(100), m_paramUnitTypeId(50), 
    m_paramData(2000), m_columnCallsign(6), m_columnUnitSizeId(50), m_columnUnitIdentityId(50), 
    m_columnCombatReadinessDescription(100), m_columnUnitTypeId(50), m_paramNClob(100), 
    m_columnNClob(1000), m_paramBinary(6000), m_columnBinary( 6000 ), m_paramBlob(1000),
    m_columnBlob(500), m_inoutParamCallsign(6), m_inoutParamUnitSizeId(50), m_inoutParamUnitIdentityId(50),
    m_inoutParamCombatReadinessDescription(100), m_bInputOutputIsPrepared( false ), 
    m_bOutputIsPrepared( false ), m_bCreateIsPrepared( false ), m_bUpdateIsPrepared( false ), 
    m_bDeleteIsPrepared(false), m_bGetAllUnitsIsPrepared( false ), m_bPerfTestIsPrepared( false ), 
    m_bUseDbIsPrepared( false ), m_bBinaryReadIsPrepared( false ), m_bBinaryWriteIsPrepared( false ), 
    m_bWriteNClobIsPrepared(false), m_bReadNClobIsPrepared(false), m_bWriteBlobIsPrepared(false),
    m_bReadBlobIsPrepared(false), m_bLongTimeQueryIsPrepared(false),
    m_bInsertInto42IsPrepared( false )
{   
    //Safir::Olib::TestObject* test=new Safir::Olib::TestObject();
    m_Object=Safir::Olib::TestObject::Create();

    m_Object->Callsign().SetVal(L"CS");
    m_Object->UnitSizeId().SetVal(L"Brigade");
    m_Object->UnitIdentity().SetVal(L"Own");
    m_Object->CombatReadines().SetVal(1);
    m_Object->CombatReadinessDescription().SetVal(L"Descp");
    m_Object->Latitude().SetVal(17.6389);
    m_Object->Longitude().SetVal(59.8586);
    m_Object->Course().SetVal(43.0);
    m_Object->Speed().SetVal(29.0);
    m_Object->IsAlive().SetVal(true);
    m_Object->Alargeinteger().SetVal(1);


    //Setting param values from object
    m_paramCallsign.SetValue( m_Object->Callsign().GetVal() );
    std::wstring aString = m_Object->UnitSizeId().GetVal();
    m_paramUnitSizeId.SetValue( aString.c_str(), static_cast<unsigned int>(aString.size()) );
    m_paramUnitIdentityId.SetValue( m_Object->UnitIdentity().GetVal() );
    m_paramCombatReadiness.SetValue( m_Object->CombatReadines().GetVal() );
    m_paramCombatReadinessDescription.SetValue( m_Object->CombatReadinessDescription().GetVal() );
    m_paramLongitude.SetValue( m_Object->Longitude().GetVal() );
    m_paramLatitude.SetValue( m_Object->Latitude().GetVal() );
    m_paramCourse.SetValue( m_Object->Course().GetVal() );
    m_paramSpeed.SetValue(m_Object->Speed().GetVal());
    m_paramMeasurementTime.SetNull();
    m_paramIsAlive.SetValue( m_Object->IsAlive().GetVal() );
    m_paramAlargeinteger.SetValue( m_Object->Alargeinteger().GetVal() );

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

void DbUnitAccess::Connect(const std::wstring DatabaseLogin)
{   
    std::string strTimestamp("2007-08-05 13:34");
    std::string strMsgSeqNo;
    std::string strRefMsgSeqNo;

    if (!m_environment.IsValid())
        m_environment.Alloc();

    if (!m_connection.IsValid())
        m_connection.Alloc(m_environment);

    if (!m_connection.IsConnected())
    {

      // m_connection.Connect(DatabaseLogin ); // MIMER
       // m_connection.Connect(L"DSN=safirDbMySQL;PWD=olibtesteruser;UID=olibtesteruser;SERVER=localhost;" );  //MYSQL
       m_connection.Connect(L"DSN=SafirDbPSQL;DRIVER=SQL;" );  //postgresql

       m_connection.UseManualTransactions();
       //SQL_ATTR_CONNECTION_TIMEOUT seems not to work with postgresql
       //m_connection.SetConnectAttr(SQL_ATTR_CONNECTION_TIMEOUT, 5L);
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

    m_LongTimeQuery.Alloc( m_connection );
    if (!m_bLongTimeQueryIsPrepared && m_LongTimeQuery.IsValid())
    {
        m_LongTimeQuery.Prepare(  L"Update tblOlibTest set Latitude = 42.0;" );
        m_LongTimeQuery.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, 1L);  // Query timeout is 5 secs
        m_bLongTimeQueryIsPrepared = true;
    }    

    m_InsertInto42.Alloc( m_connection );
    if (!m_bInsertInto42IsPrepared && m_InsertInto42.IsValid())
    {
        m_InsertInto42.Prepare(  
            L"insert into TBLOLIBTEST (UNITID, CALLSIGN, COMBATREADINESS, "
            L"COMBATREADINESSDESCRIPTION, UNITSIZEID, UNITIDENTITYID, LATITUDE, "
            L"LONGITUDE, SPEED, COURSE, MEASUREMENTTIME, ISALIVE, ALARGEINT) "
            L"values (42, '', 0, '', '', '', 0, 0, 0, 0, NULL, 0, 0)" );
        m_InsertInto42.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, 5L);  // Query timeout is 5 secs
        m_bInsertInto42IsPrepared = true;
    }
}


void DbUnitAccess::ClearTables(void)
{
    Safir::Databases::Odbc::Statement ClearStmt;
    ClearStmt.Alloc( m_connection );
    ClearStmt.Prepare(L"DELETE FROM TBLOLIBTEST WHERE UNITID >=0;");
    ClearStmt.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, 5L);
    ClearStmt.Execute();


    ClearStmt.Prepare(L"DELETE FROM TBLOLIBTESTBLOB WHERE ID >=0;");
    ClearStmt.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, 5L);
    ClearStmt.Execute();

    ClearStmt.Prepare(L"DELETE FROM TBLOLIBTESTBINARY WHERE ID >=0;");
    ClearStmt.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, 5L);
    ClearStmt.Execute();

    ClearStmt.Prepare(L"DELETE FROM TBLOLIBTESTNCLOB WHERE ID >=0;");
    ClearStmt.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, 5L);
    ClearStmt.Execute();
 
    m_connection.Commit();
   
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

void DbUnitAccess::ReadUnit(int Id)
{
    //Check Connection
    if (!m_connection.IsConnected())
    {
        throw Safir::Databases::Odbc::ReconnectException(L"Not connected to database. ",__WFILE__,__LINE__);
    }

    //Allocate statement
    if (!m_ReadUnitStmt.IsValid())
    {
        m_ReadUnitStmt.Alloc( m_connection );
    }

    //Set statement to columns for unit with Id
    m_ReadUnitStmt.Prepare( L"select UnitId, CallSign, UnitSizeId, UnitIdentityId, CombatReadiness, "
                          L"CombatReadinessDescription, Latitude, Longitude, Speed, Course, MeasurementTime, "
                          L"IsAlive, ALargeInt from TBLOLIBTEST "
                          L"where UnitId = " + boost::lexical_cast<std::wstring>( Id ));

    //Bind parameters
    m_ReadUnitStmt.BindColumn(1, m_columnUnitId);
    m_ReadUnitStmt.BindColumn(2, m_columnCallsign);
    m_ReadUnitStmt.BindColumn(3, m_columnUnitSizeId);
    m_ReadUnitStmt.BindColumn(4, m_columnUnitIdentityId);
    m_ReadUnitStmt.BindColumn(5, m_columnCombatReadiness);
    m_ReadUnitStmt.BindColumn(6, m_columnCombatReadinessDescription);
    m_ReadUnitStmt.BindColumn(7, m_columnLatitude);
    m_ReadUnitStmt.BindColumn(8, m_columnLongitude);
    m_ReadUnitStmt.BindColumn(9, m_columnSpeed);
    m_ReadUnitStmt.BindColumn(10, m_columnCourse);
    m_ReadUnitStmt.BindColumn(11, m_columnMeasurementTime);
    m_ReadUnitStmt.BindColumn(12, m_columnIsAlive);
    m_ReadUnitStmt.BindColumn(13, m_columnAlargeinteger);

    m_ReadUnitStmt.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, 5L);  // Query timeout is 5 secs

    //Execute statment
    m_ReadUnitStmt.Execute();
    m_ReadUnitStmt.Fetch();
    m_ReadUnitStmt.CloseCursor();
    m_connection.Commit();


}

void DbUnitAccess::EvaluateOutData()
{
    //Create objectptr to testobject
    Safir::Olib::TestObjectPtr outObjPtr =Safir::Olib::TestObject::Create();

    //Get values from columndata and set to outObject
    outObjPtr->Callsign().SetVal(m_columnCallsign.GetValue());
    outObjPtr->UnitSizeId().SetVal(m_columnUnitSizeId.GetValue());
    outObjPtr->UnitIdentity().SetVal(m_columnUnitIdentityId.GetValue());
    outObjPtr->CombatReadines().SetVal(m_columnCombatReadiness.GetValue());
    outObjPtr->CombatReadinessDescription().SetVal(m_columnCombatReadinessDescription.GetValue());
    outObjPtr->Latitude().SetVal(m_columnLatitude.GetValue());
    outObjPtr->Longitude().SetVal(m_columnLongitude.GetValue());
    outObjPtr->Speed().SetVal(m_columnSpeed.GetValue());
    outObjPtr->Course().SetVal(m_columnCourse.GetValue());
    outObjPtr->IsAlive().SetVal(m_columnIsAlive.GetValue());

    if(m_columnAlargeinteger.IsNull())
    {
        outObjPtr->Alargeinteger().SetNull();
    }
    else
    {
        outObjPtr->Alargeinteger().SetVal(m_columnAlargeinteger.GetValue());
    }

    //Create xml from input and output object
    std::wstring inObjXml, outObjXml;
    inObjXml=Safir::Dob::Typesystem::Serialization::ToXml(m_Object);
    outObjXml=Safir::Dob::Typesystem::Serialization::ToXml(outObjPtr);

//    std::wcout<<inObjXml<<std::endl;
//    std::wcout<<outObjXml<<std::endl;

    //Reset shared pointer
    outObjPtr.reset();

    //Check if diff between input and output xml
    if(inObjXml.compare(outObjXml)!=0)
        throw Safir::Databases::Odbc::ReconnectException(L"Input and output not equal. ",__WFILE__,__LINE__);
}
void DbUnitAccess::TestPSQL()
{
    //Check connection
    if (!m_connection.IsConnected())
    {
        throw Safir::Databases::Odbc::ReconnectException(L"Not connected to database. ",__WFILE__,__LINE__);
    }

    //Check if statement is allocated
    if (!m_CreateStmt.IsValid())
    {
        m_CreateStmt.Alloc( m_connection );
    }

    //Set create statement


    //Set create statement
    m_CreateStmt.Prepare(L"{call spCreateOlibTest(?, ?,?, ?,?,?,?,?,?,?,?,?)}");

    //Bind parameters to create statement
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
    //Execute statement
    m_CreateStmt.Execute();

    //Commit changes to the database
    m_connection.Commit();
}

void DbUnitAccess::CreateUnit()
{
    //Check connection
    if (!m_connection.IsConnected())
    {
        throw Safir::Databases::Odbc::ReconnectException(L"Not connected to database. ",__WFILE__,__LINE__);
    }

    //Check if statement is allocated
    if (!m_CreateStmt.IsValid())
    {
        m_CreateStmt.Alloc( m_connection );
    }

    //Set create statement
    m_CreateStmt.Prepare(L"{call spCreateOlibTest(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)}");

    //Bind parameters to create statement
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

    //Execute statement
    m_CreateStmt.Execute();

    //Commit changes to the database
    m_connection.Commit();
}

void DbUnitAccess::UpdateUnit()
{
    //Check connection
    if (!m_connection.IsConnected())
    {
        throw Safir::Databases::Odbc::ReconnectException(L"Not connected to database. ",__WFILE__,__LINE__);
    }

    //Allocate statement
    if (!m_UpdateStmt.IsValid())
    {
        m_UpdateStmt.Alloc( m_connection );
    }

    //Set statement for update
    m_UpdateStmt.Prepare(L"{call spUpdateOlibTest(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)}");

    //Bind parameters
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

    //UnitId to update (0)
    m_paramUnitId.SetValue(0);

    //Change value of Callsign and speed and AlargeInteger
    m_Object->Callsign().SetVal(L"CS2");
    m_paramCallsign.SetValue(m_Object->Callsign().GetVal());

    m_Object->Speed().SetVal(33.3f);
    m_paramSpeed.SetValue( m_Object->Speed().GetVal());

    m_Object->Alargeinteger().SetNull();
    m_paramAlargeinteger.SetNull();

    m_UpdateStmt.Execute();
    m_connection.Commit();
}

Safir::Dob::Typesystem::Int64 DbUnitAccess::TblRowCount()
{

    //Safir::Databases::Odbc::Int64OutputParameter    RowCount;

    //std::wcout<<"RowCount: "<<RowCount.GetValue()<<std::endl;
    m_RowCountStmt.Alloc(m_connection);
    //m_RowCountStmt.Prepare(L"{call ROWCOUNT(?)}");
    m_RowCountStmt.Prepare(L"select count(*) from tblOlibTest;");
    //m_RowCountStmt.BindParameter(1,RowCount);
    Safir::Databases::Odbc::Int64Column RowCount;
    //m_RowCountStmt.BindColumn(1,RowCount);
    m_RowCountStmt.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, 5L);
    m_RowCountStmt.Execute();

     //std::wcout<<"RowCount: "<<RowCount.GetValue()<<std::endl;


    while (m_RowCountStmt.Fetch())
    {
        m_RowCountStmt.GetData(1, RowCount);
    }

    m_RowCountStmt.Free();
    m_connection.Commit();

    std::wcout<<"RowCount: "<<RowCount.GetValue()<<std::endl;
    return RowCount.GetValue();
}

void DbUnitAccess::DeleteUnit(const Safir::Dob::Typesystem::Int32 Id)
{
    if (!m_connection.IsConnected())
    {
        throw Safir::Databases::Odbc::ReconnectException(L"Not connected to database. ",__WFILE__,__LINE__);
    }

    Safir::Dob::Typesystem::Int64 RowCountBefore, RowCountAfter;

    RowCountBefore=TblRowCount();

    if (!m_DeleteStmt.IsValid())
    {
        m_DeleteStmt.Alloc( m_connection );
    }

    m_DeleteStmt.Prepare(L"{call spDeleteOlibTest(?)}");
    m_DeleteStmt.BindParameter(1, m_paramUnitId);
    m_DeleteStmt.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, 5L);   // Query timeout is 5 secs

    m_paramUnitId.SetValue( Id );
    m_DeleteStmt.Execute();
    m_connection.Commit();

    RowCountAfter=TblRowCount();

    if((RowCountBefore-RowCountAfter)!=1)
    {
      throw Safir::Databases::Odbc::ReconnectException(L"Row not deleted. ",__WFILE__,__LINE__);
    }
}

void DbUnitAccess::WriteNClobs()
{
    //Check connection
    if (!m_connection.IsConnected())
    {
        throw Safir::Databases::Odbc::ReconnectException(L"Not connected to database. ",__WFILE__,__LINE__);
    }

    if(!m_WriteNClobStmt.IsValid())
    {
        m_WriteNClobStmt.Alloc(m_connection);
    }

    //prepare statment and bind paramters
    m_WriteNClobStmt.Prepare(L"insert into tblOlibTestNClob (id, data) values (?, ?);");
    m_WriteNClobStmt.BindParameter(1, m_paramUnitId);
    //m_WriteBlobStmt.BindParameter(2, m_paramNClob); // This line causes software violation
    m_WriteNClobStmt.BindLongParameter(2, m_paramNClob);

    m_WriteNClobStmt.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, 5L);    // Query timeout is 5 secs

    int nId=0;
    int nSize=200;
    unsigned short sParameterNumber;

    //set values for putting into the database
    m_paramUnitId.SetValue( nId );
    m_paramNClob.SetValueAtExecution(static_cast<int>(nSize* sizeof (char)));
    m_WriteNClobStmt.Execute();

    if (!m_WriteNClobStmt.ParamData(sParameterNumber))
       {
           throw Safir::Dob::Typesystem::SoftwareViolationException(L"There should only be one call to ParamData!",__WFILE__,__LINE__);
       }

    m_paramNClob.SetValue( &NCstrBuffer);

    m_WriteNClobStmt.PutData( m_paramNClob );

    if ( m_WriteNClobStmt.ParamData(sParameterNumber))
       {
           throw Safir::Dob::Typesystem::SoftwareViolationException(L"There should only be one call to ParamData!",__WFILE__,__LINE__);
       }

    m_connection.Commit();
}

void DbUnitAccess::ReadNClobs()
{
    if (!m_connection.IsConnected())
    {
        std::wcout << "NotConnected" << std::endl;
        return;
    }

    //Allocate statement
    if(!m_ReadNClobStmt.IsValid())
    {
        m_ReadNClobStmt.Alloc(m_connection);
    }

    //Prepare statement and bind paramters
    m_ReadNClobStmt.Prepare(L"select data from tblOlibTestNClob where id = ?;");
    m_ReadNClobStmt.BindParameter(1, m_paramUnitId);
    //m_ReadNClobStmt.BindColumn(1, m_columnNClob);
    m_ReadNClobStmt.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, 5L);    // Query timeout is 5 secs

    //Set unit Id
    int nId=0;
    m_paramUnitId.SetValue( nId );
    m_ReadNClobStmt.Execute();

    while (m_ReadNClobStmt.Fetch())
    {
        m_ReadNClobStmt.GetData(1, m_columnNClob);
    }

    m_ReadNClobStmt.CloseCursor();
    m_connection.Commit();

    //Get the read data and compare to input data
    std::wstring outBuffer=m_columnNClob.GetValue();
    if(NCstrBuffer.compare(outBuffer)!=0)
        throw Safir::Databases::Odbc::ReconnectException(L"NCLOBS: Input and output not equal. ",__WFILE__,__LINE__);
}

void DbUnitAccess::WriteBlob()
{
    //Check connection
    if (!m_connection.IsConnected())
    {
        throw Safir::Databases::Odbc::ReconnectException(L"Not connected to database. ",__WFILE__,__LINE__);
    }

    //Allocate statment
    if(!m_WriteBlobStmt.IsValid())
    {
        m_WriteBlobStmt.Alloc(m_connection);
    }

    //prepare statment and bind paramters
    m_WriteBlobStmt.Prepare(L"insert into tblOlibTestBlob (id, data) values (?, ?);");
    m_WriteBlobStmt.BindParameter(1, m_paramUnitId);
    //m_WriteBlobStmt.BindParameter(2, m_paramBlob); // This line causes software violation
    m_WriteBlobStmt.BindLongParameter(2, m_paramBlob);
    m_WriteBlobStmt.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, 5L);    // Query timeout is 5 secs

    //binarize object
    Safir::Dob::Typesystem::BinarySerialization binary;
    Safir::Dob::Typesystem::Serialization::ToBinary(m_Object,binary);

    //set uint ID and size of blob
    int nId=0;
    int nSize=binary.size();
    unsigned short sParameterNumber;


    //set values for putting into the database
    m_paramUnitId.SetValue( nId );
    m_paramBlob.SetValueAtExecution(static_cast<int>(nSize* sizeof (char)));
    m_WriteBlobStmt.Execute();

    if (!m_WriteBlobStmt.ParamData(sParameterNumber))
       {
           throw Safir::Dob::Typesystem::SoftwareViolationException(L"There should only be one call to ParamData!",__WFILE__,__LINE__);
       }

    m_paramBlob.SetValue( &binary[0] , static_cast<int>(nSize * sizeof (char)) );
    m_WriteBlobStmt.PutData( m_paramBlob );

    if ( m_WriteBlobStmt.ParamData(sParameterNumber))
       {
           throw Safir::Dob::Typesystem::SoftwareViolationException(L"There should only be one call to ParamData!",__WFILE__,__LINE__);
       }

    m_connection.Commit();
}

void DbUnitAccess::ReadBlob()
{
    if (!m_connection.IsConnected())
    {
        std::wcout << "NotConnected" << std::endl;
        return;
    }

    //Allocate statement
    if(!m_ReadBlobStmt.IsValid())
    {
        m_ReadBlobStmt.Alloc(m_connection);
    }

    m_ReadBlobStmt.Prepare(L"select data from tblOlibTestBlob where id = ?;");
    m_ReadBlobStmt.BindParameter(1, m_paramUnitId);
    m_ReadBlobStmt.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, 5L); // Query timeout is 5 secs

    //Set unit ID to read
    int nId=0;
    m_paramUnitId.SetValue( nId );
    m_ReadBlobStmt.Execute();

    while (m_ReadBlobStmt.Fetch())
    {
        m_ReadBlobStmt.GetData(1, m_columnBlob);
    }

    m_ReadBlobStmt.CloseCursor();
    m_connection.Commit();

    //Get data from database
    const char * const data = reinterpret_cast<const char * const>(m_columnBlob.GetValue());

    //Create object and serialize
    Safir::Dob::EntityPtr entity = boost::dynamic_pointer_cast<Safir::Dob::Entity>
                                (Safir::Dob::Typesystem::ObjectFactory::Instance().CreateObject(data));
    std::wstring outputXml=Safir::Dob::Typesystem::Serialization::ToXml(entity);
    std::wstring inputXml=Safir::Dob::Typesystem::Serialization::ToXml(m_Object);

//    std::wcout<<outputXml<<std::endl;
//    std::wcout<<inputXml<<std::endl;

    //Check if diff between input and output xml
    if(inputXml.compare(outputXml)!=0)
        throw Safir::Databases::Odbc::ReconnectException(L"BLOB: Input and output not equal. ",__WFILE__,__LINE__);
}

void DbUnitAccess::LotsOfInput()
{
    if (!m_connection.IsConnected())
    {
        std::wcout << "Not Connected" << std::endl;
        return;
    }
    int NrElemnts = 10000;
    Safir::Dob::Typesystem::Int64 NrRowsBef=TblRowCount();


    //Check if statement is allocated
    if (!m_CreateStmt.IsValid())
    {
        m_CreateStmt.Alloc( m_connection );
    }

    //Set create statement
    m_CreateStmt.Prepare(L"{call spCreateOlibTest(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)}");

    //Bind parameters to create statement
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
    for (int i = 0; i< NrElemnts; ++i)
    {
        m_CreateStmt.Execute();
        m_CreateStmt.CloseCursor();
//        if((i%20)==0)
//            std::wcout<<"in loop i="<<i<<std::endl;
    }

    m_connection.Commit();

    Safir::Dob::Typesystem::Int64 NrRowsAfter=TblRowCount();
    if((NrRowsAfter-NrRowsBef)!=(NrElemnts))
        throw Safir::Databases::Odbc::ReconnectException(L"LotsOfInput: All elements were not created in table.",__WFILE__,__LINE__);


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
    //Check connection
    if (!m_connection.IsConnected())
    {
        throw Safir::Databases::Odbc::ReconnectException(L"Not connected to database. ",__WFILE__,__LINE__);
    }

    if (!m_BinaryReadStmt.IsValid())
    {
        m_BinaryReadStmt.Alloc( m_connection );
    }

    m_BinaryReadStmt.Prepare(L"select id, data from tblOlibTestBinary;");

    m_BinaryReadStmt.BindColumn(1, m_columnUnitId);
    m_BinaryReadStmt.BindColumn(2, m_columnBinary);

    m_BinaryReadStmt.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, 5L);   // Query timeout is 5 secs


    m_BinaryReadStmt.Execute();

    while (m_BinaryReadStmt.Fetch())
    {
        if (m_columnBinary.IsNull())
            throw Safir::Databases::Odbc::ReconnectException(L"Binary data is null. ",__WFILE__,__LINE__);
    }

    m_BinaryReadStmt.CloseCursor();
    m_connection.Commit();

    const char * const data = reinterpret_cast<const char * const>(m_columnBinary.GetValue());

    Safir::Dob::EntityPtr entity = boost::dynamic_pointer_cast<Safir::Dob::Entity>
        (Safir::Dob::Typesystem::ObjectFactory::Instance().CreateObject(data));


    std::wstring outputXml=Safir::Dob::Typesystem::Serialization::ToXml(entity);
    std::wstring inputXml=Safir::Dob::Typesystem::Serialization::ToXml(m_Object);

    std::wcout<<outputXml<<std::endl;
    std::wcout<<inputXml<<std::endl;

    //Check if diff between input and output xml
    if(inputXml.compare(outputXml)!=0)
        throw Safir::Databases::Odbc::ReconnectException(L"Input and output not equal. ",__WFILE__,__LINE__);


}

void DbUnitAccess::BinaryTestWrite()
{


    //Check connection
    if (!m_connection.IsConnected())
    {
        throw Safir::Databases::Odbc::ReconnectException(L"Not connected to database. ",__WFILE__,__LINE__);
    }

    //Check if statement is valid
    if(!m_BinaryWriteStmt.IsValid())
    {
        m_BinaryWriteStmt.Alloc(m_connection);
    }

    //Prepare statement and bind paramters
    m_BinaryWriteStmt.Prepare(L"insert into tblOlibTestBinary (id, data) values (?, ?);");
    m_BinaryWriteStmt.BindParameter(1, m_paramUnitId);
    m_BinaryWriteStmt.BindParameter(2, m_paramBinary);
    m_BinaryWriteStmt.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, 5L);  // Query timeout is 5 secs

    // unit id
    int nId=0;

    //Create binary
    Safir::Dob::Typesystem::BinarySerialization binary;
    Safir::Dob::Typesystem::Serialization::ToBinary(m_Object,binary);

    m_paramUnitId.SetValue( nId );
    m_paramBinary.SetValue( &binary[0], binary.size() );
    m_BinaryWriteStmt.Execute();
    m_connection.Commit();



}
std::wstring DbUnitAccess::NCstrBuffer =
            L"012345678901234567890123456789012345678901234567890123456789"
            L"0123456789012345678901234567890123456789";

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

