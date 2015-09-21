/******************************************************************************
*
* Copyright Saab AB, 2006-2013 (http://safir.sourceforge.net)
*
* Created by: Lars Hagström / stlrha
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
#ifndef NO_DATABASE_SUPPORT

#include "OdbcPersistor.h"

#include <Safir/Dob/Typesystem/Serialization.h>
#include <Safir/Dob/Typesystem/ObjectFactory.h>
#include <Safir/Dob/PersistenceParameters.h>
#include <Safir/Logging/Log.h>
#include <Safir/Dob/ConnectionAspectInjector.h>

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning (disable: 4913)
#pragma warning (disable: 4100)
#endif

#include <boost/thread.hpp>
#include <boost/chrono.hpp>

#ifdef _MSC_VER
#pragma warning(pop)
#endif

const boost::chrono::steady_clock::duration RECONNECT_EXCEPTION_DELAY = boost::chrono::milliseconds(100);
const boost::chrono::steady_clock::duration RETRY_EXCEPTION_DELAY = boost::chrono::milliseconds(10);

const int REPORT_AFTER_RECONNECTS = 100;

//-------------------------------------------------------
OdbcPersistor::OdbcPersistor(boost::asio::io_service& ioService) :
    PersistenceHandler(ioService, false),
    m_hOdbcConnection(SQL_NULL_HANDLE),
    m_hEnvironment(SQL_NULL_HANDLE),
    m_hRestoreAllConnection(SQL_NULL_HANDLE),
    m_hStoreStatement(SQL_NULL_HANDLE),
    m_hInsertStatement(SQL_NULL_HANDLE),
    m_hRowExistsStatement(SQL_NULL_HANDLE),
    m_storeBinarySmallData(new unsigned char[Safir::Dob::PersistenceParameters::BinarySmallDataColumnSize()]),
    m_currentSmallDataSize(0),
    m_storeBinaryLargeData(new unsigned char[Safir::Dob::PersistenceParameters::BinaryDataColumnSize()]),
    m_currentLargeDataSize(0),
    m_currentInt64Size(sizeof(Safir::Dob::Typesystem::Int64)),
    m_typeName( new wchar_t[Safir::Dob::PersistenceParameters::TypeNameColumnSize()]),
    m_hDeleteAllStatement(SQL_NULL_HANDLE),
    m_hDeleteStatement(SQL_NULL_HANDLE),
    m_hDeleteODBCStatement(SQL_NULL_HANDLE),
    m_hDeleteConnection(SQL_NULL_HANDLE),
    m_bIsOdbcConnected(false),
    m_bIsRestoreAllConnected(false),
    m_bStoreStatementIsValid(false),
    m_bDeleteAllIsValid(false),
    m_bDeleteIsConnected(false),
    m_bInsertIsValid(false),
    m_bDeleteIsValid(false),
    m_bDeleteODBCIsValid(false),
    m_debug(L"OdbcPersistor")
{
    m_debug << "Using ODBC connect string " << Safir::Dob::PersistenceParameters::OdbcStorageConnectString() <<std::endl;

    try
    {
        SQLRETURN ret = ::SQLAllocHandle(SQL_HANDLE_ENV, SQL_NULL_HANDLE, &m_hEnvironment);
        if (!SQL_SUCCEEDED(ret))
        {
            ThrowException(SQL_HANDLE_ENV, SQL_NULL_HANDLE);
        }

        // SetEnvAttr(SQL_ATTR_ODBC_VERSION, SQL_OV_ODBC3);
        ret = ::SQLSetEnvAttr(  m_hEnvironment,
                                SQL_ATTR_ODBC_VERSION,
                                reinterpret_cast<SQLPOINTER>(SQL_OV_ODBC3),
                                SQL_IS_UINTEGER );
        if (!SQL_SUCCEEDED(ret))
        {
            ThrowException(SQL_HANDLE_ENV, m_hEnvironment);
        }

        ret = ::SQLAllocHandle(SQL_HANDLE_DBC, m_hEnvironment, &m_hOdbcConnection);
        if (!SQL_SUCCEEDED(ret))
        {
            ThrowException(SQL_HANDLE_ENV, m_hEnvironment);
        }

        ret = ::SQLAllocHandle(SQL_HANDLE_DBC, m_hEnvironment, &m_hRestoreAllConnection);
        if (!SQL_SUCCEEDED(ret))
        {
            ThrowException(SQL_HANDLE_ENV, m_hEnvironment);
        }

        ret = ::SQLAllocHandle(SQL_HANDLE_DBC, m_hEnvironment, &m_hDeleteConnection);
        if (!SQL_SUCCEEDED(ret))
        {
            ThrowException(SQL_HANDLE_ENV, m_hEnvironment);
        }
    }
    catch(const std::exception & ex)
    {
        Safir::Logging::SendSystemLog(
            Safir::Logging::Error,
            Safir::Dob::Typesystem::Utilities::ToWstring(ex.what()) );

    }
}


//-------------------------------------------------------
OdbcPersistor::~OdbcPersistor()
{
    try
    {
        SQLRETURN ret;

        if (m_bDeleteIsConnected)
        {
            Disconnect(m_hDeleteConnection);
            m_bDeleteIsConnected = false;
        }

        if (m_bIsOdbcConnected)
        {
            Disconnect(m_hOdbcConnection);
            m_bIsOdbcConnected = false;
        }

        Free(m_hDeleteConnection);
        Free(m_hRestoreAllConnection);
        Free(m_hOdbcConnection);

        ret = ::SQLFreeHandle(SQL_HANDLE_ENV, m_hEnvironment);
        if (!SQL_SUCCEEDED(ret))
        {
            ThrowException(SQL_HANDLE_ENV, m_hEnvironment);
        }
    }
    catch(const std::exception & ex)
    {
        Safir::Logging::SendSystemLog(
            Safir::Logging::Error,
            Safir::Dob::Typesystem::Utilities::ToWstring(ex.what()) );

    }
}


//-------------------------------------------------------
void OdbcPersistor::Store(const Safir::Dob::Typesystem::EntityId& entityId,
                          const Safir::Dob::Typesystem::HandlerId& handlerId,
                          Safir::Dob::Typesystem::BinarySerialization & bin,
                          const bool update)
{
    const bool small = static_cast<int>(bin.size()) < Safir::Dob::PersistenceParameters::BinarySmallDataColumnSize();

    int retries = 0;
    bool errorReported = false;

    bool paramSet = false;
    bool done = false;
    while (!done)
    {
        try
        {
            ConnectIfNeeded(m_hOdbcConnection, m_bIsOdbcConnected, retries);

            if (!m_bStoreStatementIsValid)
            {
                AllocStatement(&m_hStoreStatement, m_hOdbcConnection);
                m_bStoreStatementIsValid = true;

                Prepare(
                    m_hStoreStatement,
                    L"UPDATE PersistentEntity SET xmlData=NULL, binarySmallData=?, binaryData=?, handlerid=? "
                    L"WHERE typeId=? AND instance=?" );

                BindParamBinary(m_hStoreStatement, 1, Safir::Dob::PersistenceParameters::BinarySmallDataColumnSize(), m_storeBinarySmallData.get(), &m_currentSmallDataSize );
                BindParamBinary(m_hStoreStatement, 2, Safir::Dob::PersistenceParameters::BinaryDataColumnSize(), m_storeBinaryLargeData.get(), &m_currentLargeDataSize );
                BindParamInt64(m_hStoreStatement, 3, &m_handler);
                BindParamInt64(m_hStoreStatement, 4, &m_type);
                BindParamInt64(m_hStoreStatement, 5, &m_instance);

                SetStmtTimeout(m_hStoreStatement);

                paramSet = false;
            }

            if (!paramSet)
            {
                if (small)
                {
                    memcpy(m_storeBinarySmallData.get(),&bin[0], bin.size());
                    m_currentSmallDataSize = bin.size();
                    m_currentLargeDataSize = SQL_NULL_DATA;
                }
                else
                {
                    memcpy(m_storeBinaryLargeData.get(),&bin[0], bin.size());
                    m_currentLargeDataSize = bin.size();
                    m_currentSmallDataSize = SQL_NULL_DATA;
                }

                m_type = entityId.GetTypeId();
                m_instance = entityId.GetInstanceId().GetRawValue();
                m_handler = handlerId.GetRawValue();
                paramSet = true;
            }

            if (!update)
            {
                // After global instanceid's we can no longer assume that all instanceid's are written to the
                // the database at startup. So try to insert them here if the entity already exist
                Insert( entityId );
            }

            Execute( m_hStoreStatement );

            m_debug << "Successfully stored binary entity in database. Size = "<<bin.size() << std::endl;
            done = true;

            if (errorReported)
            {
                Safir::Logging::SendSystemLog(Safir::Logging::Informational,
                                                L"Successfully connected to the database");
                errorReported = false;
                retries = 0;
            }
        }
        catch(const std::exception & e)
        {
            const std::wstring err = Safir::Dob::Typesystem::Utilities::ToWstring( e.what() );
            m_debug << "Caught a ReconnectException in Store:\n" << err << std::endl;
            if (retries > REPORT_AFTER_RECONNECTS && !errorReported)
            {
                Safir::Logging::SendSystemLog(Safir::Logging::Error,
                                              L"Failed to connect to the database, will keep trying. Exception info: " +
                                              err);
                errorReported = true;
            }

            DisconnectOdbcConnection();

            boost::this_thread::sleep_for(RECONNECT_EXCEPTION_DELAY);
        }
    }
}



//-------------------------------------------------------
void OdbcPersistor::Remove(const Safir::Dob::EntityProxy & entityProxy)
{
    m_debug << "Deleting " << entityProxy.GetEntityId() <<std::endl;
    int retries = 0;
    bool errorReported = false;
    bool paramSet = false;
    bool done = false;

    while (!done)
    {
        try
        {
            ConnectIfNeeded(m_hOdbcConnection, m_bIsOdbcConnected, retries);

            if (!m_bDeleteODBCIsValid)
            {
                AllocStatement(&m_hDeleteODBCStatement, m_hOdbcConnection);
                Prepare(m_hDeleteODBCStatement, L"DELETE FROM PersistentEntity WHERE typeId=? AND instance=?");
                BindParamInt64(m_hDeleteODBCStatement, 1, &m_type);
                BindParamInt64(m_hDeleteODBCStatement, 2, &m_instance);
                SetStmtTimeout(m_hDeleteODBCStatement);
                m_bDeleteODBCIsValid = true;
                paramSet = false;
            }

            if (!paramSet)
            {
                m_type = entityProxy.GetEntityId().GetTypeId();
                m_instance = entityProxy.GetEntityId().GetInstanceId().GetRawValue();
                paramSet = true;
            }

            Execute(m_hDeleteODBCStatement);
            done = true;
            if (errorReported)
            {
                Safir::Logging::SendSystemLog(Safir::Logging::Informational,
                                                L"Successfully connected to the database");
                errorReported = false;
                retries = 0;
            }
        }
        catch(const std::exception & e)
        {
            const std::wstring err = Safir::Dob::Typesystem::Utilities::ToWstring( e.what() );
            m_debug << "Caught a ReconnectException in Delete:\n" << err << std::endl;
            if (retries > REPORT_AFTER_RECONNECTS && !errorReported)
            {
                Safir::Logging::SendSystemLog(Safir::Logging::Error,
                                              L"Failed to connect to the database, will keep trying. Exception info: " +
                                              err );
                errorReported = true;
            }
            DisconnectOdbcConnection();
            boost::this_thread::sleep_for(RECONNECT_EXCEPTION_DELAY);
        }
    }
}


//-------------------------------------------------------
void OdbcPersistor::RemoveAll()
{
    DeleteAll();
}

//-------------------------------------------------------
void OdbcPersistor::DeleteAll()
{
    m_debug << "Deleting all" <<std::endl;
    int retries = 0;
    bool errorReported = false;
    bool done = false;

    while (!done)
    {
        try
        {
            ConnectIfNeeded(m_hDeleteConnection, m_bDeleteIsConnected, retries);

            if (!m_bDeleteAllIsValid)
            {
                AllocStatement( &m_hDeleteAllStatement, m_hOdbcConnection );
                m_bDeleteAllIsValid = true;

                Prepare(m_hDeleteAllStatement, L"DELETE FROM PersistentEntity");

                SetStmtTimeout(m_hDeleteAllStatement);
            }

            Execute( m_hDeleteAllStatement );

            done = true;
            if (errorReported)
            {
                Safir::Logging::SendSystemLog(Safir::Logging::Informational,
                                                L"Successfully connected to the database");
                errorReported = false;
                retries = 0;
            }
        }
        catch(const std::exception & e)
        {
            const std::wstring err = Safir::Dob::Typesystem::Utilities::ToWstring( e.what() );
            m_debug << "Caught a ReconnectException in RemoveAll:\n" << err << std::endl;
            if (retries > REPORT_AFTER_RECONNECTS && !errorReported)
            {
                Safir::Logging::SendSystemLog(Safir::Logging::Error,
                                              L"Failed to connect to the database, will keep trying. Exception info: " +
                                              err);
                errorReported = true;
            }
            if (m_bDeleteIsConnected)
            {
                Disconnect(m_hDeleteConnection);
                m_bDeleteIsConnected = false;
            }
            Free(m_hDeleteAllStatement);
            boost::this_thread::sleep_for(RECONNECT_EXCEPTION_DELAY);
        }
    }
}

//-------------------------------------------------------
void OdbcPersistor::RestoreAll()
{
    int connectionAttempts = 0;
    bool errorReported = false;
    EntityIdSet restoredObjects;

    const int binaryLargeSize = Safir::Dob::PersistenceParameters::BinaryDataColumnSize();
    const int binarySmallSize = Safir::Dob::PersistenceParameters::BinarySmallDataColumnSize();
    const int xmlSize = Safir::Dob::PersistenceParameters::XmlDataColumnSize();

    bool                                        bGetAllIsValid = false;
    SQLHSTMT                                    hGetAllStatement = SQL_NULL_HANDLE;
    Safir::Dob::Typesystem::Int64               typeId = 0;
    Safir::Dob::Typesystem::Int64               instance = 0;
    Safir::Dob::Typesystem::Int64               handler = 0;
    boost::scoped_array<unsigned char>          storeBinarySmallData(new unsigned char[binarySmallSize]);
    SQLINTEGER                                  currentSmallDataSize = 0;
    boost::scoped_array<unsigned char>          storeBinaryLargeData(new unsigned char[binaryLargeSize]);
    SQLINTEGER                                  currentLargeDataSize = 0;
    boost::scoped_array<wchar_t>                xmlBuffer(new wchar_t[xmlSize]);
    SQLINTEGER                                  currentXmlSize = 0;

    const boost::chrono::steady_clock::time_point startTime = boost::chrono::steady_clock::now();

    bool done = false;
    while (!done)
    {
        try
        {
            ConnectIfNeeded(m_hRestoreAllConnection, m_bIsRestoreAllConnected, connectionAttempts);

            for (;;)
            {
                //getAll statement execution (will loop here until we successfully execute)
                if (!bGetAllIsValid)
                {
                    AllocStatement(&hGetAllStatement, m_hRestoreAllConnection);
                    Prepare(
                        hGetAllStatement,
                        L"SELECT typeId, instance, handlerid, xmlData, binaryData, binarySmallData from PersistentEntity" );
                    BindColumnInt64(hGetAllStatement, 1, &typeId);
                    BindColumnInt64(hGetAllStatement, 2, &instance);
                    BindColumnInt64(hGetAllStatement, 3, &handler);

                    BindColumnString(hGetAllStatement, 4, xmlSize, xmlBuffer.get(), &currentXmlSize);

                    BindColumnBinary(hGetAllStatement, 5, binaryLargeSize, storeBinaryLargeData.get(), &currentLargeDataSize);
                    BindColumnBinary(hGetAllStatement, 6, binarySmallSize, storeBinarySmallData.get(), &currentSmallDataSize);
                }

                try
                {
                    Execute( hGetAllStatement );
                    break;
                }
                catch(const std::exception & e)
                {
                    const std::wstring err = Safir::Dob::Typesystem::Utilities::ToWstring( e.what() );
                    m_debug << "Caught a RetryException in GetAll:\n" << err << std::endl;
                    boost::this_thread::sleep_for(RETRY_EXCEPTION_DELAY);
                }
            }

            for (;;)
            {
                if (!Fetch(hGetAllStatement))
                {//we've got all rows!
                    done = true;
                    break;
                }

                const Safir::Dob::Typesystem::EntityId entityId(
                        typeId,
                        Safir::Dob::Typesystem::InstanceId(instance));

                const Safir::Dob::Typesystem::HandlerId handler(handler);

                auto findIt = GetPersistentTypes().find(entityId.GetTypeId());
                if (findIt == GetPersistentTypes().end())
                { //not to be persisted!
                    Delete(m_hDeleteConnection,m_bDeleteIsConnected, entityId);
                    continue;
                }

                if (restoredObjects.find(entityId) != restoredObjects.end())
                { //already restored this object
                    continue;
                }

                try
                {
                    Safir::Dob::ConnectionAspectInjector injector(m_dobConnection);

                    if (currentXmlSize != SQL_NULL_DATA)
                    { //some xml persistent data set
                        std::wstring xml = xmlBuffer.get();
                        m_debug
                            << "Restoring from xml"                 << entityId
                            << ", size = "                          << xml.size()
                            << ". First 100 chars of the data: "    << xml.substr(0,100)
                            << std::endl;

                        Safir::Dob::Typesystem::ObjectPtr object =
                            Safir::Dob::Typesystem::Serialization::ToObject(xml);
                        Safir::Dob::EntityPtr entity =
                            boost::dynamic_pointer_cast<Safir::Dob::Entity>(object);
                        m_debug << "Successfully deserialized" <<std::endl;

                        injector.InitialSet(entity, entityId.GetInstanceId(), handler );
                        m_debug << "InitialSet successful"<<std::endl;

                        Safir::Dob::Typesystem::BinarySerialization bin;
                        Safir::Dob::Typesystem::Serialization::ToBinary( entity, bin);
                        Store(entityId, handler, bin, true);
                        m_debug << "Stored it as binary" << std::endl;
                    }
                    else if (currentSmallDataSize != SQL_NULL_DATA)
                    {
                        const char * const data = reinterpret_cast<const char * const>(storeBinarySmallData.get());
                        m_debug << "Restoring " << entityId << " from binary " <<std::endl;

                        Safir::Dob::EntityPtr entity =
                            boost::dynamic_pointer_cast<Safir::Dob::Entity>
                            (Safir::Dob::Typesystem::ObjectFactory::Instance().CreateObject(data));
                        m_debug << "Successfully deserialized" <<std::endl;

                        injector.InitialSet(entity, entityId.GetInstanceId(), handler );
                        m_debug << "InitialSet successful"<<std::endl;
                    }
                    else
                    {
                        if (currentLargeDataSize != SQL_NULL_DATA)
                        { //some binarypersistent data set
                            const char * const data = reinterpret_cast<const char * const>(storeBinaryLargeData.get());
                            m_debug << "Restoring " << entityId << " from binary " <<std::endl;

                            Safir::Dob::EntityPtr entity =
                                boost::dynamic_pointer_cast<Safir::Dob::Entity>
                                (Safir::Dob::Typesystem::ObjectFactory::Instance().CreateObject(data));
                            m_debug << "Successfully deserialized" <<std::endl;

                            injector.InitialSet(entity, entityId.GetInstanceId(), handler );
                            m_debug << "InitialSet successful"<<std::endl;
                        }
                        else
                        {
                            m_debug << "No data set for " << entityId <<std::endl;
                        }
                    }
                    //add the objectid to the list of objectids that we've done, so that we can resume from where
                    //we were in case the db goes down during restore
                    restoredObjects.insert(entityId);
                }
                catch(const Safir::Dob::Typesystem::IllegalValueException &)
                {
                    m_debug << "Could not restore "
                            << entityId.ToString()
                            << ", removing it" << std::endl;

                    Safir::Logging::SendSystemLog(Safir::Logging::Error,
                                                    L"Failed to restore entity" +
                                                    entityId.ToString() +
                                                    L", will remove persisted data.");

                    //we don't want to try it again if the connection fails later.
                    restoredObjects.insert(entityId);
                    //remove the row from the db
                    Delete(m_hDeleteConnection,m_bDeleteIsConnected, entityId);
                    //since we did not remove the objectid from persistentObjects an empty row will be inserted below.
                }
            }
        }
        catch(const std::exception & e)
        {
            const std::wstring err = Safir::Dob::Typesystem::Utilities::ToWstring( e.what() );
            m_debug << "Caught a ReconnectException in RestoreAll:\n" << err << std::endl;
            if (connectionAttempts > REPORT_AFTER_RECONNECTS && !errorReported)
            {
                Safir::Logging::SendSystemLog(Safir::Logging::Error,
                                              L"Failed to connect to the database, will keep trying. Exception info: " +
                                              err );
                errorReported = true;
            }

            if (m_bIsRestoreAllConnected)
            {
                Disconnect(m_hRestoreAllConnection);
                m_bIsRestoreAllConnected= false;
            }
            Free(hGetAllStatement);
            bGetAllIsValid = false;

            boost::this_thread::sleep_for(RECONNECT_EXCEPTION_DELAY);
        }
    }

    if (errorReported)
    {
        Safir::Logging::SendSystemLog(Safir::Logging::Informational,
                                      L"Successfully connected to the database");
        errorReported = false;
        connectionAttempts = 0;
    }

    Free(hGetAllStatement);

    Free(m_hInsertStatement);

    //we don't need the deleteconnection any more, so close it.
    try
    {
        if (m_bIsRestoreAllConnected)
        {
            Disconnect(m_hRestoreAllConnection);
            m_bIsRestoreAllConnected= false;
        }

        if (m_bDeleteIsConnected)
        {
            Disconnect(m_hDeleteConnection);
            m_bDeleteIsConnected = false;
        }
        Free(m_hDeleteConnection);

        m_debug << "RestoreAll completed" <<std::endl;
        m_debug
            << restoredObjects.size()                                           << " objects restored in time "
            << boost::chrono::steady_clock::now() - startTime << std::endl;
    }
    catch(const std::exception & e)
    {
        const std::wstring err = Safir::Dob::Typesystem::Utilities::ToWstring( e.what() );
        Safir::Logging::SendSystemLog(Safir::Logging::Error,
                                        L"Whoops. Error while disconnecting from the DeleteConnection. Ignoring this and moving on. Exception info: " +
                                        err );
    }
}

//-------------------------------------------------------
void
OdbcPersistor::Insert(const Safir::Dob::Typesystem::EntityId & entityId)
{
    int retries = 0;
    bool errorReported = false;
    m_debug << "Inserting " << entityId <<std::endl;
    bool paramSet = false;
    bool done = false;

    while (!done)
    {
        try
        {
            ConnectIfNeeded(m_hOdbcConnection, m_bIsOdbcConnected, retries);

            if (!m_bInsertIsValid)
            {
                AllocStatement(&m_hRowExistsStatement, m_hOdbcConnection);

                AllocStatement(&m_hInsertStatement, m_hOdbcConnection);
                m_bInsertIsValid = true;

                Prepare(
                    m_hRowExistsStatement,
                    L"SELECT count(*) as antal from PersistentEntity where typeId=? AND instance=? " );

                Prepare(
                    m_hInsertStatement,
                    L"INSERT INTO PersistentEntity (typeid, instance, typename) "
                    L"values (?, ?, ?); " );

                const int typeNameMaxSize = Safir::Dob::PersistenceParameters::TypeNameColumnSize();

                BindParamInt64(m_hRowExistsStatement, 1, &m_type);
                BindParamInt64(m_hRowExistsStatement, 2, &m_instance);
                BindColumnInt64(m_hRowExistsStatement,1, &m_rowCount);

                BindParamInt64(m_hInsertStatement, 1, &m_type);
                BindParamInt64(m_hInsertStatement, 2, &m_instance);
                BindParamString(m_hInsertStatement, 3, typeNameMaxSize, m_typeName.get(), &m_typeNameSize);

                SetStmtTimeout(m_hInsertStatement);
                SetStmtTimeout(m_hRowExistsStatement);

                paramSet = false;
            }

            if (!paramSet)
            {
                m_type = entityId.GetTypeId();
                m_instance = entityId.GetInstanceId().GetRawValue();

                std::wstring name = Safir::Dob::Typesystem::Operations::GetName(entityId.GetTypeId());
                const size_t length = (name.size() + 1) * sizeof(wchar_t);
                memcpy(m_typeName.get(),name.c_str(), length);
                m_typeNameSize = SQL_NTS;

                paramSet = true;
            }

            Execute(m_hRowExistsStatement);

            bool bExecuteInsert = false;
            if (Fetch(m_hRowExistsStatement))
            {
                // Insert if no row exist
                if (m_rowCount <= 0)
                {
                    bExecuteInsert = true;
                }
            }

            CloseCursor(m_hRowExistsStatement);

            if (bExecuteInsert)
                Execute(m_hInsertStatement);

            done = true;
            if (errorReported)
            {
                Safir::Logging::SendSystemLog(Safir::Logging::Informational,
                                                L"Successfully connected to the database");
                errorReported = false;
                retries = 0;
            }

        }
        catch(const std::exception & e)
        {
            const std::wstring err = Safir::Dob::Typesystem::Utilities::ToWstring( e.what() );
            m_debug << "Caught a ReconnectException in Insert:\n" << err << std::endl;
            if (retries > REPORT_AFTER_RECONNECTS && !errorReported)
            {
                Safir::Logging::SendSystemLog(Safir::Logging::Error,
                                              L"Failed to connect to the database, will keep trying. Exception info: " +
                                              err);
                errorReported = true;
            }

            DisconnectOdbcConnection();

            boost::this_thread::sleep_for(RECONNECT_EXCEPTION_DELAY);
        }

    }
}

//-------------------------------------------------------
void
OdbcPersistor::DisconnectOdbcConnection()
{
    if (m_bIsOdbcConnected)
    {
        Disconnect(m_hOdbcConnection);
        m_bIsOdbcConnected = false;
    }
    Free(m_hInsertStatement);
    m_bInsertIsValid = false;

    Free(m_hStoreStatement);
    m_bStoreStatementIsValid = false;

    Free(m_hDeleteODBCStatement);
    m_bDeleteODBCIsValid = false;
}


//-------------------------------------------------------
void
OdbcPersistor::Delete(SQLHDBC connectionToUse,
                      bool & connectionIsValid,
                      const Safir::Dob::Typesystem::EntityId& entityId)
{
    m_debug << "Deleting " << entityId <<std::endl;
    int retries = 0;
    bool errorReported = false;
    bool paramSet = false;
    bool done = false;

    while (!done)
    {
        try
        {
            ConnectIfNeeded(connectionToUse, connectionIsValid, retries);

            if (!connectionIsValid)
            {
                AllocStatement(&m_hDeleteStatement, connectionToUse);
                Prepare(m_hDeleteStatement, L"DELETE FROM PersistentEntity WHERE typeId=? AND instance=?");
                BindParamInt64(m_hDeleteStatement, 1, &m_type);
                BindParamInt64(m_hDeleteStatement, 2, &m_instance);
                SetStmtTimeout(m_hDeleteStatement);
                m_bDeleteIsValid = true;
                paramSet = false;
            }

            if (!paramSet)
            {
                m_type = entityId.GetTypeId();
                m_instance = entityId.GetInstanceId().GetRawValue();
                paramSet = true;
            }

            Execute(m_hDeleteStatement);
            done = true;
            if (errorReported)
            {
                Safir::Logging::SendSystemLog(Safir::Logging::Informational,
                                                L"Successfully connected to the database");
                errorReported = false;
                retries = 0;
            }
        }
        catch(const std::exception & e)
        {
            const std::wstring err = Safir::Dob::Typesystem::Utilities::ToWstring( e.what() );
            m_debug << "Caught a ReconnectException in Delete:\n" << err << std::endl;
            if (retries > REPORT_AFTER_RECONNECTS && !errorReported)
            {
                Safir::Logging::SendSystemLog(Safir::Logging::Error,
                                              L"Failed to connect to the database, will keep trying. Exception info: " +
                                              err );
                errorReported = true;
            }
            if (connectionIsValid)
            {
                Disconnect(connectionToUse);
                connectionIsValid = false;
            }
            Free(m_hDeleteStatement);
            m_bDeleteIsValid = false;
            boost::this_thread::sleep_for(RECONNECT_EXCEPTION_DELAY);
        }
    }
}

//-------------------------------------------------------
void
OdbcPersistor::BindParamInt64(SQLHSTMT hStmt, const SQLUSMALLINT paramNumber, Safir::Dob::Typesystem::Int64 * value)
{
    SQLRETURN ret = ::SQLBindParameter(
        hStmt,                                  // StatementHandle
        paramNumber,                            // ParameterNumber,
        SQL_PARAM_INPUT,                        // InputOutputType
        SQL_C_SBIGINT,                          // ValueType
        SQL_BIGINT,                             // ParameterType
        20,                                     // ColumnSize
        0,                                      // DecimalDigits
        value,                                  // ParameterValuePtr
        sizeof(Safir::Dob::Typesystem::Int64),  // BufferLength
        &m_currentInt64Size );                  // StrLen_or_Ind
    if (!SQL_SUCCEEDED(ret))
    {
        ThrowException(SQL_HANDLE_STMT,hStmt);
    }
}

void
OdbcPersistor::ThrowException(SQLSMALLINT   HandleType,
                              SQLHANDLE     Handle)
{
    wchar_t     wszSqlState[6];
    SQLINTEGER  lpNativeErrorPtr;
    wchar_t     wszMessageText[512];
    SQLRETURN   ret;

    ret = ::SQLGetDiagRecW( HandleType,
                            Handle,
                            1,
                            wszSqlState,
                            &lpNativeErrorPtr,
                            wszMessageText,
                            256,
                            0 );
    if (SQL_SUCCEEDED(ret))
    {
        std::wstring string = wszSqlState;
        string += L":";
        string += wszMessageText;

        throw std::exception(Safir::Dob::Typesystem::Utilities::ToUtf8(string).c_str());
    }
}

void
OdbcPersistor::BindParamBinary(SQLHSTMT hStmt,
                               const SQLUSMALLINT paramNumber,
                               const SQLUINTEGER maxSize,
                               unsigned char * buffer,
                               SQLINTEGER * sizePtr)
{

    SQLRETURN ret = ::SQLBindParameter(
        hStmt,                          // StatementHandle
        paramNumber,                    // ParameterNumber,
        SQL_PARAM_INPUT,                // InputOutputType
        SQL_C_BINARY,                   // ValueType
        SQL_LONGVARBINARY,              // ParameterType
        //SQL_VARBINARY,                  // ParameterType
        maxSize,                        // ColumnSize
        0,                              // DecimalDigits
        buffer,                         // ParameterValuePtr
        maxSize,                        // BufferLength
        sizePtr );                      // StrLen_or_Ind
    if (!SQL_SUCCEEDED(ret))
    {
        ThrowException(SQL_HANDLE_STMT,hStmt);
    }
}


void
OdbcPersistor::BindParamString(SQLHSTMT hStmt,
                               const SQLUSMALLINT paramNumber,
                               const SQLUINTEGER maxSize,
                               wchar_t * string,
                               SQLINTEGER * sizePtr)
{
    const SQLUINTEGER number_of_chars = static_cast<SQLUINTEGER>( maxSize ) + 1;
    const SQLUINTEGER size_of_char = static_cast<SQLUINTEGER>(sizeof(wchar_t));
    const SQLUINTEGER columnSize = number_of_chars * size_of_char;

    SQLRETURN ret = ::SQLBindParameter(
        hStmt,                          // StatementHandle
        paramNumber,                    // ParameterNumber,
        SQL_PARAM_INPUT,                // InputOutputType
        SQL_C_WCHAR,                    // ValueType
        SQL_WLONGVARCHAR,               // ParameterType
        //SQL_WVARCHAR,                   // ParameterType
        columnSize,                     // ColumnSize
        0,                              // DecimalDigits
        string,                         // ParameterValuePtr
        maxSize,                        // BufferLength
        sizePtr );                      // StrLen_or_Ind
    if (!SQL_SUCCEEDED(ret))
    {
        ThrowException(SQL_HANDLE_STMT,hStmt);
    }
}

void
OdbcPersistor::ConnectIfNeeded(SQLHDBC hConnection, bool & isConnected, int & connectionAttempts)
{
    if(!isConnected)
    {
        ++connectionAttempts;
        m_debug << L"Connecting to database, attempt " << connectionAttempts << std::endl;

        SQLRETURN ret = ::SQLDriverConnectW(
            hConnection,
            NULL,
            const_cast<wchar_t *>(Safir::Dob::PersistenceParameters::OdbcStorageConnectString().c_str()),
            SQL_NTS,
            NULL,
            0,
            NULL,
            SQL_DRIVER_NOPROMPT );
        if (SQL_SUCCEEDED(ret))
        {
            isConnected = true;
        }
        else
        {
            ThrowException(SQL_HANDLE_DBC, hConnection );
        }

        // SQLSetConnectAttr(SQL_ATTR_AUTOCOMMIT, SQL_AUTOCOMMIT_ON);
        ret = ::SQLSetConnectAttr(  hConnection,
                                    SQL_ATTR_AUTOCOMMIT,
                                    reinterpret_cast<SQLPOINTER>(SQL_AUTOCOMMIT_ON),
                                    SQL_IS_UINTEGER );
        if (!SQL_SUCCEEDED(ret))
        {
            ThrowException(SQL_HANDLE_DBC, hConnection );
        }

    }
}

void
OdbcPersistor::Prepare(SQLHSTMT hStmt, const std::wstring & sql)
{
    // const_cast is used because StatementText is declared as input in the ODBC
    // specification and should be a const wchar_t *.
    SQLRETURN ret = ::SQLPrepareW(
        hStmt,
        const_cast<wchar_t *>(sql.c_str()),
        SQL_NTS );
    if (!SQL_SUCCEEDED(ret))
    {
        ThrowException(SQL_HANDLE_STMT,hStmt);
    }
}

void
OdbcPersistor::SetStmtTimeout(SQLHSTMT hStmt)
{
    SQLRETURN ret = ::SQLSetStmtAttr(
        hStmt,
        SQL_ATTR_QUERY_TIMEOUT,
        reinterpret_cast<SQLPOINTER>(15),
        SQL_IS_UINTEGER );
    if (!SQL_SUCCEEDED(ret))
    {
        ThrowException(SQL_HANDLE_STMT,hStmt);
    }
}

void
OdbcPersistor::CloseCursor(SQLHSTMT hStmt)
{
    SQLRETURN ret = ::SQLCloseCursor(hStmt);
    if (!SQL_SUCCEEDED(ret))
    {
        ThrowException(SQL_HANDLE_STMT,hStmt);
    }
}


void
OdbcPersistor::Execute(SQLHSTMT hStmt)
{
    SQLRETURN ret = ::SQLExecute( hStmt );
    if (!SQL_SUCCEEDED(ret))
    {
        ThrowException(SQL_HANDLE_STMT,hStmt);
    }
}

void
OdbcPersistor::AllocStatement(SQLHSTMT * hStmt, SQLHDBC hConnection)
{
    SQLRETURN ret = ::SQLAllocHandle(SQL_HANDLE_STMT, hConnection, hStmt);
    if (!SQL_SUCCEEDED(ret))
    {
        ThrowException(SQL_HANDLE_STMT,hStmt);
    }
}

void
OdbcPersistor::BindColumnInt64( SQLHSTMT hStmt,
                                unsigned short usColumnNumber,
                                Safir::Dob::Typesystem::Int64 * value )
{
    SQLRETURN ret;

    ret = ::SQLBindCol( hStmt,                                  // StatementHandle
                        usColumnNumber,                         // ColumnNumber,
                        SQL_C_SBIGINT,                          // TargetType,
                        value,                                  // TargetValuePtr,
                        sizeof(Safir::Dob::Typesystem::Int64),  // BufferLength,
                        &m_currentInt64Size);                   // StrLen_or_Ind
    if (!SQL_SUCCEEDED(ret))
    {
        ThrowException(SQL_HANDLE_STMT,hStmt);
    }
}

void
OdbcPersistor::BindColumnBinary( SQLHSTMT hStmt,
                                 unsigned short usColumnNumber,
                                 const int maxSize,
                                 unsigned char * buffer,
                                 SQLINTEGER * sizePtr )
{
    SQLRETURN ret;

    ret = ::SQLBindCol( hStmt,                      // StatementHandle
                        usColumnNumber,             // ColumnNumber,
                        SQL_C_BINARY,               // TargetType,
                        buffer,                     // TargetValuePtr,
                        maxSize,                    // BufferLength,
                        sizePtr);                   // StrLen_or_Ind
    if (!SQL_SUCCEEDED(ret))
    {
        ThrowException(SQL_HANDLE_STMT,hStmt);
    }
}

void
OdbcPersistor::BindColumnString( SQLHSTMT hStmt,
                                 unsigned short usColumnNumber,
                                 const int maxSize,
                                 wchar_t * string,
                                 SQLINTEGER * sizePtr )
{
    SQLRETURN ret;

    ret = ::SQLBindCol( hStmt,                      // StatementHandle
                        usColumnNumber,             // ColumnNumber,
                        SQL_C_WCHAR,                // TargetType,
                        string,                     // TargetValuePtr,
                        maxSize,                    // BufferLength,
                        sizePtr);                   // StrLen_or_Ind
    if (!SQL_SUCCEEDED(ret))
    {
        ThrowException(SQL_HANDLE_STMT,hStmt);
    }
}

bool
OdbcPersistor::Fetch( SQLHSTMT hStmt )
{
    SQLRETURN ret;
    bool bDataFound = true;

    ret = ::SQLFetch( hStmt );
    if (ret==SQL_NO_DATA_FOUND)
    {
        bDataFound = false;
    }
    else if (!SQL_SUCCEEDED(ret))
    {
        ThrowException(SQL_HANDLE_STMT,hStmt);
    }
    return bDataFound;
}

void
OdbcPersistor::Free( SQLHDBC hConnection )
{
    SQLRETURN ret = ::SQLFreeHandle(SQL_HANDLE_DBC, hConnection);
    if (!SQL_SUCCEEDED(ret))
    {
        ThrowException(SQL_HANDLE_DBC, hConnection );
    }
}

void
OdbcPersistor::Disconnect( SQLHDBC hConnection )
{
    SQLRETURN ret = ::SQLDisconnect( hConnection );
    if (!SQL_SUCCEEDED(ret))
    {
        ThrowException(SQL_HANDLE_DBC, hConnection );
    }
}

#endif
