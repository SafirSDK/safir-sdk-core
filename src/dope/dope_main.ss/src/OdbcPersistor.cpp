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
#include <Safir/Databases/Odbc/Columns.h>
#include <Safir/Dob/PersistenceParameters.h>
#include <Safir/Logging/Log.h>
#include <Safir/Databases/Odbc/Exception.h>
#include <Safir/Dob/Typesystem/BlobOperations.h>
#include <Safir/Dob/ConnectionAspectInjector.h>

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning (disable: 4913)
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
template <class T>
void Free(T & thing)
{
    if (thing.IsValid())
    {
        thing.Free();
    }
}

//-------------------------------------------------------
template <class T>
void Disconnect(T & thing)
{
    if (thing.IsConnected())
    {
        thing.Disconnect();
    }
}

//-------------------------------------------------------
template <class T>
void Alloc(T & thing)
{
    int retries = 0;
    bool errorReported = false;
    while (!thing.IsValid())
    {
        try
        {
            thing.Alloc();
            if (errorReported)
            {
                Safir::Logging::SendSystemLog(Safir::Logging::Informational,
                                              L"Successfully allocated the something that failed before...");
                errorReported = false;
                retries = 0;
            }
        }
        catch (const Safir::Databases::Odbc::ReconnectException& e)
        {
            Free(thing);
            ++retries;
            if (retries > REPORT_AFTER_RECONNECTS && !errorReported)
            {
                Safir::Logging::SendSystemLog(Safir::Logging::Error,
                                              L"Dope failed to Alloc something. Will retry. Exception info: " + e.GetExceptionInfo());
                errorReported = true;
            }
            boost::this_thread::sleep_for(RECONNECT_EXCEPTION_DELAY);
        }
    }
}

//-------------------------------------------------------
template <class T, class U>
void Alloc(T & thing, U & arg)
{
    int retries = 0;
    bool errorReported = false;
    while (!thing.IsValid())
    {
        try
        {
            thing.Alloc(arg);
            if (errorReported)
            {
                Safir::Logging::SendSystemLog(Safir::Logging::Informational,
                                              L"Successfully allocated the something that failed before...");
                errorReported = false;
                retries = 0;
            }
        }
        catch (const Safir::Databases::Odbc::ReconnectException& e)
        {
            Free(thing);
            ++retries;
            if (retries > REPORT_AFTER_RECONNECTS && !errorReported)
            {
                Safir::Logging::SendSystemLog(Safir::Logging::Error,
                                              L"Dope failed to Alloc something. Will retry. Exception info: " + e.GetExceptionInfo());
                errorReported = true;
            }
            boost::this_thread::sleep_for(RECONNECT_EXCEPTION_DELAY);
        }
    }
}



//-------------------------------------------------------
OdbcPersistor::OdbcPersistor(boost::asio::io_service& ioService) :
    PersistenceHandler(ioService, false),
    m_odbcConnection(),
    m_environment(),
    m_storeBinaryDataParam(Safir::Dob::PersistenceParameters::XmlDataColumnSize()),
    m_storeBinarySmallDataParam(Safir::Dob::PersistenceParameters::BinarySmallDataColumnSize()),
    m_insertTypeNameParam(Safir::Dob::PersistenceParameters::TypeNameColumnSize()),
    m_deleteConnection(),
    m_debug(L"OdbcPersistor")
{
    m_debug << "Using ODBC connect string " << Safir::Dob::PersistenceParameters::OdbcStorageConnectString() <<std::endl;
    Alloc(m_environment);
    Alloc(m_odbcConnection, m_environment);
    Alloc(m_deleteConnection, m_environment);
}


//-------------------------------------------------------
OdbcPersistor::~OdbcPersistor()
{
    Disconnect(m_deleteConnection);
    Disconnect(m_odbcConnection);

    Free(m_deleteConnection);
    Free(m_odbcConnection);
    Free(m_environment);
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
            if( !m_odbcConnection.IsConnected() )
            {
                ++retries;
                m_debug << "Store: Connecting to database, attempt " << retries << std::endl;
                m_odbcConnection.Connect(Safir::Dob::PersistenceParameters::OdbcStorageConnectString());
            }

            if (!m_storeStatement.IsValid())
            {
                m_storeStatement.Alloc(m_odbcConnection);
                m_storeStatement.Prepare(
                    L"UPDATE PersistentEntity SET xmlData=NULL, binarySmallData=?, binaryData=?, handlerid=? WHERE typeId=? AND instance=?");
                m_storeStatement.BindParameter( 1, m_storeBinarySmallDataParam );
                m_storeStatement.BindLongParameter( 2, m_storeBinaryDataParam );
                m_storeStatement.BindParameter( 3, m_storeHandlerParam );
                m_storeStatement.BindParameter( 4, m_storeTypeIdParam );
                m_storeStatement.BindParameter( 5, m_storeInstanceParam );
                m_storeStatement.SetStmtAttr( SQL_ATTR_QUERY_TIMEOUT, 15 );
                paramSet = false;
            }

            if (!paramSet)
            {
                if (small)
                {
                    m_storeBinarySmallDataParam.SetValue(&bin[0],static_cast<unsigned int>(bin.size()));
                    m_storeBinaryDataParam.SetNull();
                }
                else
                {
                    m_storeBinarySmallDataParam.SetNull();
                }

                m_storeTypeIdParam.SetValue(entityId.GetTypeId());
                m_storeInstanceParam.SetValue(entityId.GetInstanceId().GetRawValue());
                m_storeHandlerParam.SetValue(handlerId.GetRawValue() );
                paramSet = true;
            }

            try
            {
                if (!update)
                {
                    // After global instanceid's we can no longer assume that all instanceid's are written to the
                    // the database at startup. So try to insert them here if the entity already exist
                    Insert( entityId );
                }

                if (!small)
                {
                    m_storeBinaryDataParam.SetValueAtExecution(static_cast<int>(bin.size() * sizeof (char)));
                }

                m_storeStatement.Execute();

                if (!small)
                {
                    unsigned short param = 0;
                    if (!m_storeStatement.ParamData(param))
                    {
                        throw Safir::Dob::Typesystem::SoftwareViolationException(L"There should be one call to ParamData!",__WFILE__,__LINE__);
                    }
                    m_storeBinaryDataParam.SetValue(&bin[0],
                                                    static_cast<int>(bin.size() * sizeof (char)));
                    m_storeStatement.PutData(m_storeBinaryDataParam);
                    if (m_storeStatement.ParamData(param))
                    {
                        throw Safir::Dob::Typesystem::SoftwareViolationException(L"There should only be one call to ParamData!",__WFILE__,__LINE__);
                    }
                }
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
            catch(const Safir::Databases::Odbc::RetryException& e)
            {
                m_debug << "Caught a RetryException in Store:\n" << e.GetExceptionInfo() << std::endl;
                boost::this_thread::sleep_for(RETRY_EXCEPTION_DELAY);
            }
        }
        catch(const Safir::Databases::Odbc::ReconnectException& e)
        {
            m_debug << "Caught a ReconnectException in Store:\n" << e.GetExceptionInfo() << std::endl;
            if (retries > REPORT_AFTER_RECONNECTS && !errorReported)
            {
                Safir::Logging::SendSystemLog(Safir::Logging::Error,
                                              L"Failed to connect to the database, will keep trying. Exception info: " +
                                              e.GetExceptionInfo());
                errorReported = true;
            }
            Disconnect(m_odbcConnection);
            Free(m_storeStatement);
            boost::this_thread::sleep_for(RECONNECT_EXCEPTION_DELAY);
        }
    }
}



//-------------------------------------------------------
void OdbcPersistor::Remove(const Safir::Dob::EntityProxy & entityProxy)
{
    Delete(m_odbcConnection, entityProxy.GetEntityId());
}

//-------------------------------------------------------
void OdbcPersistor::RemoveAll()
{
    DeleteAll(m_odbcConnection);
}

//-------------------------------------------------------
void OdbcPersistor::DeleteAll(Safir::Databases::Odbc::Connection & connectionToUse)
{
    m_debug << "Deleting all" <<std::endl;
    int retries = 0;
    bool errorReported = false;
    bool done = false;

    while (!done)
    {
        try
        {
            if( !connectionToUse.IsConnected() )
            {
                ++retries;
                connectionToUse.Connect(Safir::Dob::PersistenceParameters::OdbcStorageConnectString());
            }

            if (!m_deleteAllStatement.IsValid())
            {
                m_deleteAllStatement.Alloc(connectionToUse);
                m_deleteAllStatement.Prepare(L"DELETE FROM PersistentEntity");
                m_deleteAllStatement.SetStmtAttr( SQL_ATTR_QUERY_TIMEOUT, 15 );
            }

            try
            {
                m_deleteAllStatement.Execute();
                done = true;
                if (errorReported)
                {
                    Safir::Logging::SendSystemLog(Safir::Logging::Informational,
                                                  L"Successfully connected to the database");
                    errorReported = false;
                    retries = 0;
                }
            }
            catch(const Safir::Databases::Odbc::RetryException& e)
            {
                m_debug << "Caught a RetryException in RemoveAll:\n" << e.GetExceptionInfo() << std::endl;
                boost::this_thread::sleep_for(RETRY_EXCEPTION_DELAY);
            }
        }
        catch(const Safir::Databases::Odbc::ReconnectException& e)
        {
            m_debug << "Caught a ReconnectException in RemoveAll:\n" << e.GetExceptionInfo() << std::endl;
            if (retries > REPORT_AFTER_RECONNECTS && !errorReported)
            {
                Safir::Logging::SendSystemLog(Safir::Logging::Error,
                                              L"Failed to connect to the database, will keep trying. Exception info: " +
                                              e.GetExceptionInfo());
                errorReported = true;
            }
            Disconnect(connectionToUse);
            Free(m_deleteAllStatement);
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

    Safir::Databases::Odbc::Statement getAllStatement;
    Safir::Databases::Odbc::Int64Column typeIdColumn;
    Safir::Databases::Odbc::Int64Column instanceColumn;
    Safir::Databases::Odbc::Int64Column handlerColumn;
    Safir::Databases::Odbc::WideStringColumn xmlDataColumn(Safir::Dob::PersistenceParameters::XmlDataColumnSize());
    Safir::Databases::Odbc::BinaryColumn binaryDataColumn(Safir::Dob::PersistenceParameters::BinaryDataColumnSize());
    Safir::Databases::Odbc::BinaryColumn binarySmallDataColumn(Safir::Dob::PersistenceParameters::BinarySmallDataColumnSize());

    const boost::chrono::steady_clock::time_point startTime = boost::chrono::steady_clock::now();

    bool done = false;
    while (!done)
    {
        try
        {
            if (!m_odbcConnection.IsConnected())
            {
                ++connectionAttempts;
                m_debug << "RestoreAll: Connecting to database, attempt " << connectionAttempts << std::endl;
                m_odbcConnection.Connect(Safir::Dob::PersistenceParameters::OdbcStorageConnectString());
            }

            for (;;)
            {
                //getAll statement execution (will loop here until we successfully execute)
                if (!getAllStatement.IsValid())
                {
                    getAllStatement.Alloc(m_odbcConnection);
                    getAllStatement.Prepare( L"SELECT typeId, instance, handlerid, xmlData, binaryData, binarySmallData from PersistentEntity" );
                    getAllStatement.BindColumn( 1, typeIdColumn );
                    getAllStatement.BindColumn( 2, instanceColumn);
                    getAllStatement.BindColumn( 3, handlerColumn);
                    //getAllStatement.BindColumn( 4, xmlDataColumn );
                    //getAllStatement.BindColumn( 5, binaryDataColumn );
                    getAllStatement.BindColumn( 6, binarySmallDataColumn );
                }

                try
                {
                    getAllStatement.Execute();
                    break;
                }
                catch(const Safir::Databases::Odbc::RetryException& e)
                {
                    m_debug << "Caught a RetryException in GetAll:\n" << e.GetExceptionInfo() << std::endl;
                    boost::this_thread::sleep_for(RETRY_EXCEPTION_DELAY);
                }
            }

            for (;;)
            {
                try
                {
                    if (!getAllStatement.Fetch())
                    {//we've got all rows!
                        done = true;
                        break;
                    }

                    const Safir::Dob::Typesystem::EntityId entityId
                        (typeIdColumn.GetValue(),
                         Safir::Dob::Typesystem::InstanceId(instanceColumn.GetValue()));

                    const Safir::Dob::Typesystem::HandlerId handler(handlerColumn.GetValue());

                    auto findIt = GetPersistentTypes().find(entityId.GetTypeId());
                    if (findIt == GetPersistentTypes().end())
                    { //not to be persisted!
                        Delete(m_deleteConnection,entityId);
                        continue;
                    }

                    if (restoredObjects.find(entityId) != restoredObjects.end())
                    { //already restored this object
                        continue;
                    }

                    try
                    {
                        Safir::Dob::ConnectionAspectInjector injector(m_dobConnection);
                        getAllStatement.GetData(4, xmlDataColumn);


                        if (!xmlDataColumn.IsNull())
                        { //some xml persistent data set
                            std::wstring xml = xmlDataColumn.GetValue();
                            m_debug << "Restoring from xml" << entityId << ", size = " << xml.size() << ". First 100 chars of the data: " << xml.substr(0,100) <<std::endl;

                            Safir::Dob::EntityPtr entity =
                                boost::dynamic_pointer_cast<Safir::Dob::Entity>(Safir::Dob::Typesystem::Serialization::ToObject(xml));
                            m_debug << "Successfully deserialized" <<std::endl;

                            injector.InitialSet(entity, entityId.GetInstanceId(), handler );
                            m_debug << "InitialSet successful"<<std::endl;

                            Safir::Dob::Typesystem::BinarySerialization bin;
                            Safir::Dob::Typesystem::Serialization::ToBinary( entity, bin);
                            Store(entityId, handler, bin, true);
                            m_debug << "Stored it as binary" << std::endl;
                        }
                        else if (!binarySmallDataColumn.IsNull())
                        {
                            const char * const data = reinterpret_cast<const char * const>(binarySmallDataColumn.GetValue());
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
                            getAllStatement.GetData(5, binaryDataColumn);
                            if (!binaryDataColumn.IsNull())
                            { //some binarypersistent data set
                                const char * const data = reinterpret_cast<const char * const>(binaryDataColumn.GetValue());
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
                        Delete(m_deleteConnection,entityId);
                        //since we did not remove the objectid from persistentObjects an empty row will be inserted below.
                    }
                }
                catch(const Safir::Databases::Odbc::RetryException& e)
                {
                    m_debug << "Caught a RetryException in Fetch:\n" << e.GetExceptionInfo() << std::endl;
                    boost::this_thread::sleep_for(RETRY_EXCEPTION_DELAY);
                }
            }
        }
        catch(const Safir::Databases::Odbc::ReconnectException& e)
        {
            m_debug << "Caught a ReconnectException in RestoreAll:\n" << e.GetExceptionInfo() << std::endl;
            if (connectionAttempts > REPORT_AFTER_RECONNECTS && !errorReported)
            {
                Safir::Logging::SendSystemLog(Safir::Logging::Error,
                                              L"Failed to connect to the database, will keep trying. Exception info: " +
                                              e.GetExceptionInfo());
                errorReported = true;
            }

            Disconnect(m_odbcConnection);
            Free(getAllStatement);

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

    Free(getAllStatement);

    Free(m_insertStatement);

    //we don't need the deleteconnection any more, so close it.
    Disconnect(m_deleteConnection);
    Free(m_deleteConnection);
    m_debug << "RestoreAll completed" <<std::endl;
    m_debug << restoredObjects.size() << " objects restored in time "
            << boost::chrono::steady_clock::now() - startTime << std::endl;
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
            if( !m_odbcConnection.IsConnected() )
            {
                ++retries;
                m_debug << "Insert: Connecting to database, attempt " << retries << std::endl;
                m_odbcConnection.Connect(Safir::Dob::PersistenceParameters::OdbcStorageConnectString());
            }

            if (!m_insertStatement.IsValid())
            {
                m_insertStatement.Alloc(m_odbcConnection);
                m_insertStatement.Prepare(
                    L"call spInsertEntity(?, ?, ?);");
                m_insertStatement.BindParameter( 1, m_insertTypeIdInsertParam );
                m_insertStatement.BindParameter( 2, m_insertInstanceInsertParam );
                m_insertStatement.BindParameter( 3, m_insertTypeNameParam );
                m_insertStatement.SetStmtAttr( SQL_ATTR_QUERY_TIMEOUT, 15 );
                paramSet = false;
            }

            if (!paramSet)
            {
                m_insertTypeIdInsertParam.SetValue(entityId.GetTypeId());
                m_insertInstanceInsertParam.SetValue(entityId.GetInstanceId().GetRawValue() );
                m_insertTypeNameParam.SetValue(Safir::Dob::Typesystem::Operations::GetName(entityId.GetTypeId()).c_str());
                paramSet = true;
            }

            try
            {
                m_insertStatement.Execute();
                done = true;
                if (errorReported)
                {
                    Safir::Logging::SendSystemLog(Safir::Logging::Informational,
                                                  L"Successfully connected to the database");
                    errorReported = false;
                    retries = 0;
                }
            }
            catch(const Safir::Databases::Odbc::RetryException& e)
            {
                m_debug << "Caught a RetryException in Insert:\n" << e.GetExceptionInfo() << std::endl;
                boost::this_thread::sleep_for(RETRY_EXCEPTION_DELAY);
            }

        }
        catch(const Safir::Databases::Odbc::ReconnectException& e)
        {
            m_debug << "Caught a ReconnectException in Insert:\n" << e.GetExceptionInfo() << std::endl;
            if (retries > REPORT_AFTER_RECONNECTS && !errorReported)
            {
                Safir::Logging::SendSystemLog(Safir::Logging::Error,
                                              L"Failed to connect to the database, will keep trying. Exception info: " +
                                              e.GetExceptionInfo());
                errorReported = true;
            }
            Disconnect(m_odbcConnection);
            Free(m_insertStatement);
            boost::this_thread::sleep_for(RECONNECT_EXCEPTION_DELAY);
        }

    }
}



//-------------------------------------------------------
void
OdbcPersistor::Delete(Safir::Databases::Odbc::Connection & connectionToUse,
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
            if( !connectionToUse.IsConnected() )
            {
                ++retries;
                connectionToUse.Connect(Safir::Dob::PersistenceParameters::OdbcStorageConnectString());
            }

            if (!m_deleteStatement.IsValid())
            {
                m_deleteStatement.Alloc(connectionToUse);
                m_deleteStatement.Prepare(L"DELETE FROM PersistentEntity WHERE typeId=? AND instance=?");
                m_deleteStatement.BindParameter( 1, m_deleteTypeIdParam );
                m_deleteStatement.BindParameter( 2, m_deleteInstanceParam );
                m_deleteStatement.SetStmtAttr( SQL_ATTR_QUERY_TIMEOUT, 15 );
                paramSet = false;
            }

            if (!paramSet)
            {
                m_deleteTypeIdParam.SetValue(entityId.GetTypeId());
                m_deleteInstanceParam.SetValue(entityId.GetInstanceId().GetRawValue() );
                paramSet = true;
            }

            try
            {
                m_deleteStatement.Execute();
                done = true;
                if (errorReported)
                {
                    Safir::Logging::SendSystemLog(Safir::Logging::Informational,
                                                  L"Successfully connected to the database");
                    errorReported = false;
                    retries = 0;
                }
            }
            catch(const Safir::Databases::Odbc::RetryException& e)
            {
                m_debug << "Caught a RetryException in Delete:\n" << e.GetExceptionInfo() << std::endl;
                boost::this_thread::sleep_for(RETRY_EXCEPTION_DELAY);
            }
        }
        catch(const Safir::Databases::Odbc::ReconnectException& e)
        {
            m_debug << "Caught a ReconnectException in Delete:\n" << e.GetExceptionInfo() << std::endl;
            if (retries > REPORT_AFTER_RECONNECTS && !errorReported)
            {
                Safir::Logging::SendSystemLog(Safir::Logging::Error,
                                              L"Failed to connect to the database, will keep trying. Exception info: " +
                                              e.GetExceptionInfo());
                errorReported = true;
            }
            Disconnect(connectionToUse);
            Free(m_deleteStatement);
            boost::this_thread::sleep_for(RECONNECT_EXCEPTION_DELAY);
        }
    }
}
#endif
