/******************************************************************************
 *
 * Copyright Saab AB, 2005-2013 (http://safir.sourceforge.net)
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
#include "DbOlibTest.h"
#include <Safir/Dob/Typesystem/Exceptions.h>
#include <iostream>
#include <Safir/Databases/Odbc/ReconnectException.h>
#include <Safir/Databases/Odbc/RetryException.h>
#include <Safir/Databases/Odbc/Defs.h>
#include <Safir/Dob/Typesystem/Serialization.h>
#include <Safir/Dob/Typesystem/BlobOperations.h>
#include <Safir/Dob/Typesystem/ObjectFactory.h>
#include <boost/shared_ptr.hpp>

DbOlibTest::DbOlibTest():

    m_outParamId(1),
    m_outParamStringName(100),
    m_outParamStringDescription(100),
    m_paramStringName(10),
    m_paramStringDescription(40),
    m_paramData(2000),
    m_columnStringName(10),
    m_columnStringDescription(40),
    m_paramNClob(100),
    m_columnNClob(1000),
    m_paramBinary(6000),
    m_columnBinary( 6000 ),
    m_paramBlob(1000),
    m_columnBlob(500),
    m_inoutParamStringName(0),
    m_inoutParamStringDescription(0),
    m_bInputOutputIsPrepared( false ),
    m_bOutputIsPrepared( false ),
    m_bCreateIsPrepared( false ),
    m_bUpdateIsPrepared( false ),
    m_bDeleteIsPrepared(false),
    m_bGetAllUnitsIsPrepared( false ),
    m_bPerfTestIsPrepared( false ),
    m_bUseDbIsPrepared( false ),
    m_bBinaryReadIsPrepared( false ),
    m_bBinaryWriteIsPrepared( false ),
    m_bWriteNClobIsPrepared(false),
    m_bReadNClobIsPrepared(false),
    m_bWriteBlobIsPrepared(false),
    m_bReadBlobIsPrepared(false),
    m_bLongTimeQueryIsPrepared(false),
    m_bInsertInto42IsPrepared( false )
{   
    m_Object=OlibTest::TestObject::Create();
    m_Object->StringName().SetVal(L"Name");
    m_Object->StringDescription().SetVal(L"Description");
    m_Object->Int32().SetVal(32);
    m_Object->Int64().SetVal(64);
    m_Object->Float32().SetVal(32.32f);
    m_Object->Float64().SetVal(64.64);
    m_Object->Bool().SetVal(1);

    //Setting param values from object
    m_paramStringName.SetValue(m_Object->StringName().GetVal());
    std::wstring aString = m_Object->StringDescription().GetVal();
    m_paramStringDescription.SetValue(m_Object->StringDescription().GetVal());
    m_paramInt32.SetValue(m_Object->Int32().GetVal());
    m_paramInt64.SetValue(m_Object->Int64().GetVal());
    m_paramFloat32.SetValue(m_Object->Float32().GetVal());
    m_paramFloat64.SetValue(m_Object->Float64().GetVal());
    m_paramBool.SetValue(m_Object->Bool().GetVal());
}

DbOlibTest::~DbOlibTest(void)
{
    m_Object.reset();
}

void DbOlibTest::TestOutputParameters(const bool curlyBracesNeeded)
{
    if (!m_connection.IsConnected())
    {
        std::wcout << "Not Connected" << std::endl;
        return;
    }

    if(!m_OutputStmt.IsValid())
    {
        m_OutputStmt.Alloc(m_connection);
    }

    if(curlyBracesNeeded)
    {
        m_OutputStmt.Prepare(L"{call spOutputOlibTest (?, ?, ?, ?, ?, ?, ?)}");
    }
    else
    {
        m_OutputStmt.Prepare(L"call spOutputOlibTest (?, ?, ?, ?, ?, ?, ?)");
    }

    m_OutputStmt.BindParameter(1, m_outParamStringName);
    m_OutputStmt.BindParameter(2, m_outParamStringDescription);
    m_OutputStmt.BindParameter(3, m_outParamInt32);
    m_OutputStmt.BindParameter(4, m_outParamInt64);
    m_OutputStmt.BindParameter(5, m_outParamFloat32);
    m_OutputStmt.BindParameter(6, m_outParamFloat64);
    m_OutputStmt.BindParameter(7, m_outParamBool);

    m_OutputStmt.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, 5L);   // Query timeout is 5 secs

    m_outParamStringName.SetValue(L"NameOut");
    m_outParamStringDescription.SetValue(L"None");
    m_outParamInt32.SetValue(0);
    m_outParamInt64.SetValue(0);
    m_outParamFloat32.SetValue(0);
    m_outParamFloat64.SetValue(0);
    m_outParamBool.SetValue(0);


    m_OutputStmt.Execute();
    while( m_OutputStmt.MoreResults())
        ;

    //Create objectptr to testobject
    OlibTest::TestObjectPtr outObjPtr = OlibTest::TestObject::Create();

    //Get values from columndata and set to outObject
    outObjPtr->StringName().SetVal(m_outParamStringName.GetValue());
    outObjPtr->StringDescription().SetVal(m_outParamStringDescription.GetValue());
    outObjPtr->Int32().SetVal(m_outParamInt32.GetValue());
    outObjPtr->Int64().SetVal(m_outParamInt64.GetValue());
    outObjPtr->Float32().SetVal(m_outParamFloat32.GetValue());
    outObjPtr->Float64().SetVal(m_outParamFloat64.GetValue());
    outObjPtr->Bool().SetVal(m_outParamBool.GetValue());

    m_connection.Commit();

    //Create xml from input and output object
    std::wstring inObjXml, outObjXml;
    inObjXml=Safir::Dob::Typesystem::Serialization::ToXml(m_Object);
    outObjXml=Safir::Dob::Typesystem::Serialization::ToXml(outObjPtr);

    //Reset shared pointer
    outObjPtr.reset();

    //Check if diff between input and output xml
    if(inObjXml.compare(outObjXml)!=0)
    {
        std::wcout<<inObjXml<<std::endl;
        std::wcout<<outObjXml<<std::endl;
        throw Safir::Databases::Odbc::ReconnectException(L"Input and output not equal. ",__WFILE__,__LINE__);
    }
}

void DbOlibTest::Connect(const std::wstring DatabaseLogin)
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
        //m_isMimerSQL = true;
        m_connection.Connect(DatabaseLogin );//m_isMimerSQL=true; // MIMER
        // m_connection.Connect(L"DSN=safirDbMySQL;PWD=olibtesteruser;UID=olibtesteruser;SERVER=localhost;" ); m_isMySQL=true; //MYSQL
        //m_connection.Connect(L"DSN=SafirDbPSQL;DRIVER=SQL;" ); m_isPostgreSQL=true; //postgresql

        m_connection.UseManualTransactions();
        //SQL_ATTR_CONNECTION_TIMEOUT seems not to work with postgresql
        //m_connection.SetConnectAttr(SQL_ATTR_CONNECTION_TIMEOUT, 5L);
    }
}

void DbOlibTest::Disconnect()
{
    if (m_connection.IsConnected())
        m_connection.Disconnect();

    if (m_connection.IsValid()) 
        m_connection.Free();

    if (m_environment.IsValid())
        m_environment.Free();
}

void DbOlibTest::AllocStmt()
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
    if (!m_PerfTestStmt.IsValid())
        m_PerfTestStmt.Alloc( m_connection );
    if (!m_ReadUnitStmt.IsValid())
        m_ReadUnitStmt.Alloc( m_connection );
    if(!m_RowCountStmt.IsValid())
        m_RowCountStmt.Alloc(m_connection);

    if (!m_bRowCountIsPrepared && m_RowCountStmt.IsValid())
    {
        m_RowCountStmt.Prepare(L"select count(*) from tblOlibTest;");

        m_RowCountStmt.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, 5L);
    }

    if (!m_bReadUnitIsPrepared && m_ReadUnitStmt.IsValid())
    {
        //Set statement to columns for unit with Id
        m_ReadUnitStmt.Prepare( L"select Id, StringName, StringDescription, Int32, Int64,"
                                L"Float32, Float64, Bool from tblOlibTest "
                                L"where Id = 0" );

        m_ReadUnitStmt.BindColumn(1, m_columnId);
        m_ReadUnitStmt.BindColumn(2, m_columnStringName);
        m_ReadUnitStmt.BindColumn(3, m_columnStringDescription);
        m_ReadUnitStmt.BindColumn(4, m_columnInt32);
        m_ReadUnitStmt.BindColumn(5, m_columnInt64);
        m_ReadUnitStmt.BindColumn(6, m_columnFloat32);
        m_ReadUnitStmt.BindColumn(7, m_columnFloat64);
        m_ReadUnitStmt.BindColumn(8, m_columnBool);

        m_ReadUnitStmt.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, 5L);  // Query timeout is 5 secs
    }

    if (!m_bPerfTestIsPrepared && m_PerfTestStmt.IsValid())
    {
        m_PerfTestStmt.Prepare(L"Insert into tblPerfTest values(?, ?, ?)");

        m_PerfTestStmt.BindParameter(1, m_paramTypeId);
        m_PerfTestStmt.BindParameter(2, m_paramInstanceNo);
        m_PerfTestStmt.BindParameter(3, m_paramData);

        m_PerfTestStmt.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, 5L);   // Query timeout is 5 secs

        m_bPerfTestIsPrepared = true;
    }

    if (!m_bOutputIsPrepared && m_OutputStmt.IsValid())
    {
        m_OutputStmt.Prepare(L"{call spOutputOlibTest (?, ?, ?, ?, ?, ?, ?)}");

        m_OutputStmt.BindParameter(1, m_outParamStringName);
        m_OutputStmt.BindParameter(2, m_outParamStringDescription);
        m_OutputStmt.BindParameter(3, m_outParamInt32);
        m_OutputStmt.BindParameter(4, m_outParamInt64);
        m_OutputStmt.BindParameter(5, m_outParamFloat32);
        m_OutputStmt.BindParameter(6, m_outParamFloat64);
        m_OutputStmt.BindParameter(7, m_outParamBool);

        m_OutputStmt.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, 5L);   // Query timeout is 5 secs

        m_bOutputIsPrepared = true;
    }

    if (!m_bInputOutputIsPrepared && m_InputOutputStmt.IsValid())
    {
        m_InputOutputStmt.Prepare(L"{call spInputOutputOlibTest(?)}");
        m_InputOutputStmt.BindParameter(1, m_inoutParamInt32);
        m_InputOutputStmt.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, 5L);  // Query timeout is 5 secs

        m_bInputOutputIsPrepared = true;
    }

    if (!m_bCreateIsPrepared && m_CreateStmt.IsValid())
    {
        m_CreateStmt.Prepare(L"{call spCreateOlibTest(?, ?, ?, ?, ?, ?, ?)}");

        //Bind parameters to create statement
        m_CreateStmt.BindParameter(1, m_paramStringName);
        m_CreateStmt.BindParameter(2, m_paramStringDescription);
        m_CreateStmt.BindParameter(3, m_paramInt32);
        m_CreateStmt.BindParameter(4, m_paramInt64);
        m_CreateStmt.BindParameter(5, m_paramFloat32);
        m_CreateStmt.BindParameter(6, m_paramFloat64);
        m_CreateStmt.BindParameter(7, m_paramBool);


        m_CreateStmt.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, 5L);   // Query timeout is 5 secs

        m_bCreateIsPrepared = true;
    }

    if (!m_bUpdateIsPrepared && m_UpdateStmt.IsValid())
    {
        m_UpdateStmt.Prepare(L"{call spUpdateOlibTest(?, ?, ?, ?, ?, ?, ?, ?)}");

        //Bind parameters
        m_UpdateStmt.BindParameter(1, m_paramId);
        m_UpdateStmt.BindParameter(2, m_paramStringName);
        m_UpdateStmt.BindParameter(3, m_paramStringDescription);
        m_UpdateStmt.BindParameter(4, m_paramInt32);
        m_UpdateStmt.BindParameter(5, m_paramInt64);
        m_UpdateStmt.BindParameter(6, m_paramFloat32);
        m_UpdateStmt.BindParameter(7, m_paramFloat64);
        m_UpdateStmt.BindParameter(8, m_paramBool);

        m_UpdateStmt.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, 5L);   // Query timeout is 5 secs

        m_bUpdateIsPrepared = true;
    }

    if (!m_bDeleteIsPrepared && m_DeleteStmt.IsValid())
    {
        m_DeleteStmt.Prepare(L"{call spDeleteOlibTest(?)}");

        m_DeleteStmt.BindParameter(1, m_paramId);

        m_DeleteStmt.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, 5L);   // Query timeout is 5 secs

        m_bDeleteIsPrepared = true;
    }

    if (!m_bBinaryReadIsPrepared && m_BinaryReadStmt.IsValid())
    {
        m_BinaryReadStmt.Prepare(L"select id, data from tblOlibTestBinary;");

        m_BinaryReadStmt.BindColumn(1, m_columnId);
        m_BinaryReadStmt.BindColumn(2, m_columnBinary);

        m_BinaryReadStmt.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, 5L);   // Query timeout is 5 secs

        m_bBinaryReadIsPrepared = true;
    }

    if (!m_bBinaryWriteIsPrepared && m_BinaryWriteStmt.IsValid())
    {
        m_BinaryWriteStmt.Prepare(L"insert into tblOlibTestBinary (id, data) values (?, ?);");

        m_BinaryWriteStmt.BindParameter(1, m_paramId);
        m_BinaryWriteStmt.BindParameter(2, m_paramBinary);

        m_BinaryWriteStmt.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, 5L);  // Query timeout is 5 secs

        m_bBinaryWriteIsPrepared = true;
    }

    if (!m_bWriteNClobIsPrepared && m_WriteNClobStmt.IsValid())
    {
        m_WriteNClobStmt.Prepare(L"insert into tblOlibTestNClob (id, data) values (?, ?);");

        m_WriteNClobStmt.BindParameter(1, m_paramId);
        //m_WriteNClobStmt.BindParameter(2, m_paramNClob); // This line causes software violation
        m_WriteNClobStmt.BindLongParameter(2, m_paramNClob);

        m_WriteNClobStmt.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, 5L);   // Query timeout is 5 secs

        m_bWriteNClobIsPrepared = true;
    }

    if (!m_bReadNClobIsPrepared && m_ReadNClobStmt.IsValid())
    {
        m_ReadNClobStmt.Prepare(L"select data from tblOlibTestNClob where id = ?;");

        m_ReadNClobStmt.BindParameter(1, m_paramId);
        //m_ReadNClobStmt.BindColumn(1, m_columnNClob);

        m_ReadNClobStmt.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, 5L);    // Query timeout is 5 secs

        m_bReadNClobIsPrepared  = true;
    }

    if (!m_bWriteBlobIsPrepared && m_WriteBlobStmt.IsValid())
    {
        m_WriteBlobStmt.Prepare(L"insert into tblOlibTestBlob (id, data) values (?, ?);");

        m_WriteBlobStmt.BindParameter(1, m_paramId);
        //m_WriteBlobStmt.BindParameter(2, m_paramBlob); // This line causes software violation
        m_WriteBlobStmt.BindLongParameter(2, m_paramBlob);

        m_WriteBlobStmt.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, 5L);    // Query timeout is 5 secs

        m_bWriteBlobIsPrepared = true;
    }

    if (!m_bReadBlobIsPrepared && m_ReadBlobStmt.IsValid())
    {
        m_ReadBlobStmt.Prepare(L"select data from tblOlibTestBlob where id = ?;");

        m_ReadBlobStmt.BindParameter(1, m_paramId);
        //m_ReadBlobStmt.BindColumn(1, m_paramBlob);

        m_ReadBlobStmt.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, 5L); // Query timeout is 5 secs

        m_bReadBlobIsPrepared  = true;
    }

    m_LongTimeQuery.Alloc( m_connection );
    if (!m_bLongTimeQueryIsPrepared && m_LongTimeQuery.IsValid())
    {
        m_LongTimeQuery.Prepare(  L"Update tblOlibTest set Float32 = 42.0;" );
        m_LongTimeQuery.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, 1L);  // Query timeout is 5 secs
        m_bLongTimeQueryIsPrepared = true;
    }

    m_InsertInto42.Alloc( m_connection );
    if (!m_bInsertInto42IsPrepared && m_InsertInto42.IsValid())
    {
        m_InsertInto42.Prepare(
                               L"insert into tblOlibTest (Id, StringName,StringDescription, Int32, Int64,"
                               L"Float32, Float64, Bool) "
                               L"values (42, 'Name42', 'Desc42',23, 46, 0.0, 0.0,1)" );
        m_InsertInto42.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, 5L);  // Query timeout is 5 secs
        m_bInsertInto42IsPrepared = true;
    }
}


void DbOlibTest::ClearTables(void)
{
    Safir::Databases::Odbc::Statement ClearStmt;
    ClearStmt.Alloc( m_connection );
    ClearStmt.Prepare(L"DELETE FROM tblOlibTest WHERE ID >=0;");
    ClearStmt.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, 5L);
    ClearStmt.Execute();
    m_connection.Commit();

    ClearStmt.Prepare(L"DELETE FROM tblOlibTestBlob WHERE ID >=0;");
    ClearStmt.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, 5L);
    ClearStmt.Execute();
    m_connection.Commit();

    ClearStmt.Prepare(L"DELETE FROM tblOlibTestBinary WHERE ID >=0;");
    ClearStmt.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, 5L);
    ClearStmt.Execute();
    m_connection.Commit();

    ClearStmt.Prepare(L"DELETE FROM tblOlibTestNClob WHERE ID >=0;");
    ClearStmt.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, 5L);
    ClearStmt.Execute();
    m_connection.Commit();

    ClearStmt.Prepare(L"DELETE FROM tblPerfTest WHERE TypeId >=0;");
    ClearStmt.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, 20L);
    ClearStmt.Execute();
    m_connection.Commit();

    ClearStmt.Free();
}

void DbOlibTest::PerfTest()
{
    if (!m_connection.IsConnected())
    {
        std::wcout << "Not Connected" << std::endl;
        return;
    }

    if (!m_PerfTestStmt.IsValid())
    {
        m_PerfTestStmt.Alloc( m_connection );
    }

    m_PerfTestStmt.Prepare(L"Insert into tblPerfTest values(?, ?, ?)");
    m_PerfTestStmt.BindParameter(1, m_paramTypeId);
    m_PerfTestStmt.BindParameter(2, m_paramInstanceNo);
    m_PerfTestStmt.BindParameter(3, m_paramData);
    m_PerfTestStmt.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, 5L);   // Query timeout is 5 secs

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
                         //        L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
                         //        L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
                         //        L"1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
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
}

void DbOlibTest::SetReadAllTimeout()
{
    if(!m_CreateStmt.IsValid())
    {
        m_CreateStmt.Alloc(m_connection);
    }

    long lTimeout = 1;
    m_CreateStmt.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, lTimeout);
}

void DbOlibTest::GetReadAllTimeout()
{
    long lTimeout = 0;
    m_CreateStmt.GetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, lTimeout);

    if(lTimeout!=1)
    {
        throw Safir::Databases::Odbc::ReconnectException(L"SQL_ATTR_QUERY_TIMEOUT not correctly set. ",__WFILE__,__LINE__);
    }
}

void DbOlibTest::SetConnectionTimeout()
{
    long lTimeout = 1;
    m_connection.SetConnectAttr(SQL_ATTR_CONNECTION_TIMEOUT, lTimeout);
}

void DbOlibTest::GetConnectionTimeout()
{
    long lTimeout = 0;
    m_connection.GetConnectAttr(SQL_ATTR_CONNECTION_TIMEOUT, lTimeout);

    if(lTimeout!=1)
    {
        std::wcout << "timeout = " << lTimeout << std::endl;
        throw Safir::Databases::Odbc::ReconnectException(L"SQL_ATTR_CONNECTION_TIMEOUT not correctly set. ",__WFILE__,__LINE__);
    }
}

void DbOlibTest::ReadData(int Id)
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

    //Set statement to columns for data with Id
    m_ReadUnitStmt.Prepare( L"select Id, StringName, StringDescription, Int32, Int64,"
                            L"Float32, Float64, Bool from tblOlibTest "
                            L"where Id = " + boost::lexical_cast<std::wstring>( Id ));

    //Bind parameters
    m_ReadUnitStmt.BindColumn(1, m_columnId);
    m_ReadUnitStmt.BindColumn(2, m_columnStringName);
    m_ReadUnitStmt.BindColumn(3, m_columnStringDescription);
    m_ReadUnitStmt.BindColumn(4, m_columnInt32);
    m_ReadUnitStmt.BindColumn(5, m_columnInt64);
    m_ReadUnitStmt.BindColumn(6, m_columnFloat32);
    m_ReadUnitStmt.BindColumn(7, m_columnFloat64);
    m_ReadUnitStmt.BindColumn(8, m_columnBool);

    m_ReadUnitStmt.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, 5L);  // Query timeout is 5 secs

    //Execute statment
    m_ReadUnitStmt.Execute();
    m_ReadUnitStmt.Fetch();
    m_ReadUnitStmt.CloseCursor();
    m_connection.Commit();
}

void DbOlibTest::EvaluateOutData()
{
    //Create objectptr to testobject
    OlibTest::TestObjectPtr outObjPtr =OlibTest::TestObject::Create();

    //Get values from columndata and set to outObject
    if (m_columnStringName.IsNull())
    {
        std::wcout << "Null string name" << std::endl;
    }

    outObjPtr->StringName().SetVal(m_columnStringName.GetValue());
    outObjPtr->StringDescription().SetVal(m_columnStringDescription.GetValue());
    outObjPtr->Int32().SetVal(m_columnInt32.GetValue());
    outObjPtr->Float32().SetVal(m_columnFloat32.GetValue());
    outObjPtr->Float64().SetVal(m_columnFloat64.GetValue());
    outObjPtr->Bool().SetVal(m_columnBool.GetValue());
    if(m_columnInt64.IsNull())
    {
        outObjPtr->Int64().SetNull();
    }
    else
    {
        outObjPtr->Int64().SetVal(m_columnInt64.GetValue());
    }

    //Create xml from input and output object
    std::wstring inObjXml, outObjXml;
    inObjXml=Safir::Dob::Typesystem::Serialization::ToXml(m_Object);
    outObjXml=Safir::Dob::Typesystem::Serialization::ToXml(outObjPtr);

    //Reset shared pointer
    outObjPtr.reset();

    //Check if diff between input and output xml
    if(inObjXml != outObjXml)
    {
        std::wcout<<"In data: " << inObjXml<< std::endl;
        std::wcout<<"Out data: " << outObjXml<<std::endl;
        throw Safir::Databases::Odbc::ReconnectException(L"Input and output not equal. ",__WFILE__,__LINE__);
    }
}

void DbOlibTest::CloseStmt(void)
{
    if (m_InputOutputStmt.IsValid())
    {
        m_InputOutputStmt.Free();
    }
    else
    {
        throw Safir::Databases::Odbc::ReconnectException(L"Statement is not valid. ",__WFILE__,__LINE__);
    }

    if (m_OutputStmt.IsValid())
    {
        m_OutputStmt.Free();
    }
    else
    {
        throw Safir::Databases::Odbc::ReconnectException(L"Statement is not valid. ",__WFILE__,__LINE__);
    }

    if (m_CreateStmt.IsValid())
    {
        m_CreateStmt.Free();
    }
    else
    {
        throw Safir::Databases::Odbc::ReconnectException(L"Statement is not valid. ",__WFILE__,__LINE__);
    }

    if (m_UpdateStmt.IsValid())
    {
        m_UpdateStmt.Free();
    }

    if (m_DeleteStmt.IsValid())
    {
        m_DeleteStmt.Free();
    }
    else
    {
        throw Safir::Databases::Odbc::ReconnectException(L"Statement is not valid. ",__WFILE__,__LINE__);
    }

    if (m_GetAllUnitsStmt.IsValid())
    {
        m_GetAllUnitsStmt.Free();
    }
    else
    {
        throw Safir::Databases::Odbc::ReconnectException(L"Statement is not valid. ",__WFILE__,__LINE__);
    }

    if (m_PerfTestStmt.IsValid())
    {
        m_PerfTestStmt.Free();
    }
    else
    {
        throw Safir::Databases::Odbc::ReconnectException(L"Statement is not valid. ",__WFILE__,__LINE__);
    }

    if (m_BinaryReadStmt.IsValid())
    {
        m_BinaryReadStmt.Free();
    }
    else
    {
        throw Safir::Databases::Odbc::ReconnectException(L"Statement is not valid. ",__WFILE__,__LINE__);
    }

    if (m_BinaryWriteStmt.IsValid())
    {
        m_BinaryWriteStmt.Free();
    }
    else
    {
        throw Safir::Databases::Odbc::ReconnectException(L"Statement is not valid. ",__WFILE__,__LINE__);
    }

    if (m_WriteNClobStmt.IsValid())
    {
        m_WriteNClobStmt.Free();
    }
    else
    {
        throw Safir::Databases::Odbc::ReconnectException(L"Statement is not valid. ",__WFILE__,__LINE__);
    }

    if (m_ReadNClobStmt.IsValid())
    {
        m_ReadNClobStmt.Free();
    }
    else
    {
        throw Safir::Databases::Odbc::ReconnectException(L"Statement is not valid. ",__WFILE__,__LINE__);
    }

    if (m_WriteBlobStmt.IsValid())
    {
        m_WriteBlobStmt.Free();
    }
    else
    {
        throw Safir::Databases::Odbc::ReconnectException(L"Statement is not valid. ",__WFILE__,__LINE__);
    }

    if (m_ReadBlobStmt.IsValid())
    {
        m_ReadBlobStmt.Free();
    }
    else
    {
        throw Safir::Databases::Odbc::ReconnectException(L"Statement is not valid. ",__WFILE__,__LINE__);
    }

    if (m_ReadUnitStmt.IsValid())
    {
        m_ReadUnitStmt.Free();
    }
    else
    {
        throw Safir::Databases::Odbc::ReconnectException(L"Statement is not valid. ",__WFILE__,__LINE__);
    }

    if (m_LongTimeQuery.IsValid())
    {
        m_LongTimeQuery.Free();
    }
    else
    {
        throw Safir::Databases::Odbc::ReconnectException(L"Statement is not valid. ",__WFILE__,__LINE__);
    }

    if (m_InsertInto42.IsValid())
    {
        m_InsertInto42.Free();
    }
    else
    {
        throw Safir::Databases::Odbc::ReconnectException(L"Statement is not valid. ",__WFILE__,__LINE__);
    }

    if (m_RowCountStmt.IsValid())
    {
        m_RowCountStmt.Free();
    }
    else
    {
        throw Safir::Databases::Odbc::ReconnectException(L"Statement is not valid. ",__WFILE__,__LINE__);
    }
}

void DbOlibTest::CreateData()
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
    m_CreateStmt.Prepare(L"{call spCreateOlibTest(?, ?, ?, ?, ?, ?, ?)}");

    //Bind parameters to create statement
    m_CreateStmt.BindParameter(1, m_paramStringName);
    m_CreateStmt.BindParameter(2, m_paramStringDescription);
    m_CreateStmt.BindParameter(3, m_paramInt32);
    m_CreateStmt.BindParameter(4, m_paramInt64);
    m_CreateStmt.BindParameter(5, m_paramFloat32);
    m_CreateStmt.BindParameter(6, m_paramFloat64);
    m_CreateStmt.BindParameter(7, m_paramBool);

    m_CreateStmt.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, 5L);   // Query timeout is 5 secs

    //Execute statement
    m_CreateStmt.Execute();

    //Commit changes to the database
    m_connection.Commit();
}

void DbOlibTest::UpdateData()
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
    m_UpdateStmt.Prepare(L"{call spUpdateOlibTest(?, ?, ?, ?, ?, ?, ?, ?)}");

    //Bind parameters
    m_UpdateStmt.BindParameter(1, m_paramId);
    m_UpdateStmt.BindParameter(2, m_paramStringName);
    m_UpdateStmt.BindParameter(3, m_paramStringDescription);
    m_UpdateStmt.BindParameter(4, m_paramInt32);
    m_UpdateStmt.BindParameter(5, m_paramInt64);
    m_UpdateStmt.BindParameter(6, m_paramFloat32);
    m_UpdateStmt.BindParameter(7, m_paramFloat64);
    m_UpdateStmt.BindParameter(8, m_paramBool);

    m_UpdateStmt.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, 5L);   // Query timeout is 5 secs

    //Id to update (0)
    m_paramId.SetValue(0);

    //Change value of StringName, Float32 and Int64
    m_Object->StringName().SetVal(L"Name2");
    m_paramStringName.SetValue(m_Object->StringName().GetVal());

    m_Object->Float32().SetVal(33.3f);
    m_paramFloat32.SetValue( m_Object->Float32().GetVal());

    m_Object->Int64().SetNull();
    m_paramInt64.SetNull();

    m_UpdateStmt.Execute();
    m_connection.Commit();
}


void DbOlibTest::SetConnectionPooling()
{
    if (!m_environment.IsValid())
        m_environment.Alloc();

    m_environment.SetEnvAttr(SQL_ATTR_CONNECTION_POOLING, SQL_CP_ONE_PER_HENV);
}

void DbOlibTest::GetConnectionPooling()
{
    long lValue = 0;
    m_environment.GetEnvAttr(SQL_ATTR_CONNECTION_POOLING, lValue);
    switch(lValue)
    {
    case SQL_CP_OFF :
        throw Safir::Databases::Odbc::ReconnectException(L"Connection pooling set to SQL_CP_OFF. ",__WFILE__,__LINE__);
    case SQL_CP_ONE_PER_DRIVER:
        throw Safir::Databases::Odbc::ReconnectException(L"Connection pooling set to SQL_CP_ONE_PER_DRIVER. ",__WFILE__,__LINE__);
    case SQL_CP_ONE_PER_HENV:
        break;
    default:
        std::wcout << "Unexpected value for SQL_ATTR_CONNECTION_POOLING: expected " << SQL_CP_ONE_PER_HENV << ", got " << lValue << std::endl;
        throw Safir::Databases::Odbc::ReconnectException(L"Connection pooling is undefined. ",__WFILE__,__LINE__);
    }
}

Safir::Dob::Typesystem::Int64 DbOlibTest::TblRowCount()
{
    if(!m_RowCountStmt.IsValid())
    {
        m_RowCountStmt.Alloc(m_connection);
    }

    Safir::Databases::Odbc::Int64Column RowCount;

    m_RowCountStmt.Prepare(L"select count(*) from tblOlibTest;");
    m_RowCountStmt.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, 5L);
    m_RowCountStmt.Execute();

    while (m_RowCountStmt.Fetch())
    {
        m_RowCountStmt.GetData(1, RowCount);
    }

    m_RowCountStmt.Free();
    m_connection.Commit();

    return RowCount.GetValue();
}

void DbOlibTest::DeleteData(const Safir::Dob::Typesystem::Int32 Id)
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
    m_DeleteStmt.BindParameter(1, m_paramId);
    m_DeleteStmt.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, 5L);   // Query timeout is 5 secs

    m_paramId.SetValue( Id );
    m_DeleteStmt.Execute();
    m_connection.Commit();

    RowCountAfter=TblRowCount();

    if((RowCountBefore-RowCountAfter)!=1)
    {
        throw Safir::Databases::Odbc::ReconnectException(L"Row not deleted. ",__WFILE__,__LINE__);
    }
}

void DbOlibTest::WriteNClobs()
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
    m_WriteNClobStmt.BindParameter(1, m_paramId);
    //m_WriteBlobStmt.BindParameter(2, m_paramNClob); // This line causes software violation
    m_WriteNClobStmt.BindLongParameter(2, m_paramNClob);
    m_WriteNClobStmt.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, 5L);    // Query timeout is 5 secs

    int nId=0;
    int nSize=200;
    unsigned short sParameterNumber;

    //set values for putting into the database
    m_paramId.SetValue( nId );
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

void DbOlibTest::ReadNClobs()
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
    m_ReadNClobStmt.BindParameter(1, m_paramId);
    //m_ReadNClobStmt.BindColumn(1, m_columnNClob);
    m_ReadNClobStmt.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, 5L);    // Query timeout is 5 secs

    //Set unit Id
    int nId=0;
    m_paramId.SetValue( nId );
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

void DbOlibTest::WriteBlob()
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
    m_WriteBlobStmt.BindParameter(1, m_paramId);
    //m_WriteBlobStmt.BindParameter(2, m_paramBlob); // This line causes software violation
    m_WriteBlobStmt.BindLongParameter(2, m_paramBlob);
    m_WriteBlobStmt.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, 5L);    // Query timeout is 5 secs

    //binarize object
    Safir::Dob::Typesystem::BinarySerialization binary;
    Safir::Dob::Typesystem::Serialization::ToBinary(m_Object,binary);

    //set ID and size of blob
    int nId=0;
    size_t nSize=binary.size();
    unsigned short sParameterNumber;


    //set values for putting into the database
    m_paramId.SetValue( nId );
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

void DbOlibTest::ReadBlob()
{
    //Set ID to read
    int nId=0;

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
    m_ReadBlobStmt.BindParameter(1, m_paramId);
    m_ReadBlobStmt.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, 5L); // Query timeout is 5 secs
    m_paramId.SetValue( nId );
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
    Safir::Dob::Typesystem::ObjectPtr obj = 
        Safir::Dob::Typesystem::ObjectFactory::Instance().CreateObject(data);
    std::wstring outputXml=Safir::Dob::Typesystem::Serialization::ToXml(obj);
    std::wstring inputXml=Safir::Dob::Typesystem::Serialization::ToXml(m_Object);

    //Check if diff between input and output xml
    if(inputXml.compare(outputXml)!=0)
    {
        std::wcout<<outputXml<<std::endl;
        std::wcout<<inputXml<<std::endl;
        throw Safir::Databases::Odbc::ReconnectException(L"BLOB: Input and output not equal. ",__WFILE__,__LINE__);
    }
}
void DbOlibTest::InsertInto42()
{
    if (!m_connection.IsConnected())
    {
        std::wcout << "Not Connected" << std::endl;
        return;
    }

    if(!m_InsertInto42.IsValid())
    {
        m_InsertInto42.Alloc( m_connection );
    }

    m_InsertInto42.Prepare(
                           L"insert into tblOlibTest (Id, StringName,StringDescription, Int32, Int64,"
                           L"Float32, Float64, Bool) "
                           L"values (42, 'Name42', 'Desc42',23, 46, 0.0, 0.0,1)" );
    m_InsertInto42.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, 5L);  // Query timeout is 5 secs
    m_InsertInto42.Execute();

    m_connection.Commit();
    m_Object->StringName().SetVal(L"Name42");
    m_Object->StringDescription().SetVal(L"Desc42");
    m_Object->Int32().SetVal(23);
    m_Object->Int64().SetVal(46);
    m_Object->Float32().SetVal(0.0f);
    m_Object->Float64().SetVal(0.0f);
    m_Object->Bool().SetVal(1);

}

void DbOlibTest::LotsOfInput()
{
    //Nr of elements to add to the table
    int NrElemnts = 100;

    if (!m_connection.IsConnected())
    {
        throw Safir::Databases::Odbc::ReconnectException(L"Not connected to database. ",__WFILE__,__LINE__);
    }

    Safir::Dob::Typesystem::Int64 NrRowsBef=TblRowCount();

    //Check if statement is allocated
    if (!m_CreateStmt.IsValid())
    {
        m_CreateStmt.Alloc( m_connection );
    }

    //Set create statement
    m_CreateStmt.Prepare(L"{call spCreateOlibTest(?, ?, ?, ?, ?, ?, ?)}");

    //Bind parameters to create statement
    m_CreateStmt.BindParameter(1, m_paramStringName);
    m_CreateStmt.BindParameter(2, m_paramStringDescription);
    m_CreateStmt.BindParameter(3, m_paramInt32);
    m_CreateStmt.BindParameter(4, m_paramInt64);
    m_CreateStmt.BindParameter(5, m_paramFloat32);
    m_CreateStmt.BindParameter(6, m_paramFloat64);
    m_CreateStmt.BindParameter(7, m_paramBool);
    m_CreateStmt.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, 5L);   // Query timeout is 5 secs

    for (int i = 0; i< NrElemnts; ++i)
    {
        m_CreateStmt.Execute();
        m_CreateStmt.CloseCursor();
        m_connection.Commit();
    }

    Safir::Dob::Typesystem::Int64 NrRowsAfter=TblRowCount();
    if((NrRowsAfter-NrRowsBef)!=(NrElemnts))
        throw Safir::Databases::Odbc::ReconnectException(L"LotsOfInput: All elements were not created in table.",__WFILE__,__LINE__);
}

void DbOlibTest::TestInputOutputParameters(const bool curlyBracesNeeded)
{
    if (!m_connection.IsConnected())
    {
        std::wcout << "Not Connected" << std::endl;
        return;
    }

    if(!m_InputOutputStmt.IsValid())
    {
        m_InputOutputStmt.Alloc(m_connection);
    }

    if(curlyBracesNeeded)
    {
        m_InputOutputStmt.Prepare(L"{call spInputOutputOlibTest(?)}");
    }
    else
    {
        m_InputOutputStmt.Prepare(L"call spInputOutputOlibTest(?)");
    }

    m_InputOutputStmt.BindParameter(1, m_inoutParamInt32);
    m_InputOutputStmt.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, 5L);  // Query timeout is 5 secs
    m_inoutParamInt32.SetValue( m_Object->Int32().GetVal() );
    m_InputOutputStmt.Execute();
    //m_InputOutputStmt.Fetch();
    m_InputOutputStmt.MoreResults();
    m_connection.Commit();

    if(m_inoutParamInt32.GetValue()!=(2*m_Object->Int32().GetVal()))
    {
        throw Safir::Databases::Odbc::ReconnectException(L"Input and output value did not match. ",__WFILE__,__LINE__);
    }
}

void DbOlibTest::BinaryTestRead()
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
    m_BinaryReadStmt.BindColumn(1, m_columnId);
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

    Safir::Dob::Typesystem::ObjectPtr obj = 
        Safir::Dob::Typesystem::ObjectFactory::Instance().CreateObject(data);


    std::wstring outputXml=Safir::Dob::Typesystem::Serialization::ToXml(obj);
    std::wstring inputXml=Safir::Dob::Typesystem::Serialization::ToXml(m_Object);

    //Check if diff between input and output xml
    if(inputXml.compare(outputXml)!=0)
    {
        std::wcout<<inputXml<<std::endl;
        std::wcout<<outputXml<<std::endl;
        throw Safir::Databases::Odbc::ReconnectException(L"Input and output not equal. ",__WFILE__,__LINE__);
    }
}

void DbOlibTest::BinaryTestWrite()
{
    // Id to write to
    int nId=0;

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
    m_BinaryWriteStmt.BindParameter(1, m_paramId);
    m_BinaryWriteStmt.BindParameter(2, m_paramBinary);
    m_BinaryWriteStmt.SetStmtAttr(SQL_ATTR_QUERY_TIMEOUT, 5L);  // Query timeout is 5 secs

    //Create binary
    Safir::Dob::Typesystem::BinarySerialization binary;
    Safir::Dob::Typesystem::Serialization::ToBinary(m_Object,binary);

    m_paramId.SetValue( nId );
    m_paramBinary.SetValue( &binary[0], static_cast<unsigned int>(binary.size()) );
    m_BinaryWriteStmt.Execute();
    m_connection.Commit();
}

std::wstring DbOlibTest::NCstrBuffer =
            L"012345678901234567890123456789012345678901234567890123456789"
            L"0123456789012345678901234567890123456789";

char DbOlibTest::BinaryData [] =
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

