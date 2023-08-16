/******************************************************************************
*
* Copyright Saab AB, 2006-2015, 2023 (http://safirsdkcore.com)
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
#include <Safir/Dob/LowMemoryException.h>
#include <Safir/Dob/PersistenceParameters.h>
#include <Safir/Logging/Log.h>
#include <Safir/Dob/ConnectionAspectInjector.h>
#include <boost/algorithm/hex.hpp>
#include <boost/function_output_iterator.hpp>

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning (disable: 4913)
#pragma warning (disable: 4100)
#endif

#include <boost/thread.hpp>
#include <boost/chrono.hpp>

#ifdef _MSC_VER
#pragma warning(pop)

//Disable incorrect VS2010 warning about unicode characters.
#pragma warning (disable : 4428)

//Disable warning about constant conditional expressions, caused
//by USE_CHAR_OPERATIONS_FOR_TEXT_COLUMNS.
#pragma warning (disable : 4127)

#endif

const boost::chrono::steady_clock::duration RECONNECT_EXCEPTION_DELAY = boost::chrono::milliseconds(100);
const boost::chrono::steady_clock::duration RETRY_EXCEPTION_DELAY = boost::chrono::milliseconds(10);

const int REPORT_AFTER_RECONNECTS = 100;

#if defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
const bool USE_CHAR_OPERATIONS_FOR_TEXT_COLUMNS = false;
#elif defined(linux) || defined(__linux) || defined(__linux__)
const bool USE_CHAR_OPERATIONS_FOR_TEXT_COLUMNS = true;
#endif

//-------------------------------------------------------
OdbcPersistor::OdbcPersistor(boost::asio::io_service& ioService) :
    PersistenceHandler(ioService, false),
    m_environment(SQL_NULL_HANDLE),
    m_odbcConnection(SQL_NULL_HANDLE),
    m_isOdbcConnected(false),
    m_storeStatement(SQL_NULL_HANDLE),
    m_storeIsValid(false),
    m_insertStatement(SQL_NULL_HANDLE),
    m_insertIsValid(false),
    m_rowExistsStatement(SQL_NULL_HANDLE),
    m_storeBinarySmallData(new unsigned char[Safir::Dob::PersistenceParameters::BinarySmallDataColumnSize()]),
    m_currentSmallDataSize(0),
    m_storeBinaryLargeData(new unsigned char[Safir::Dob::PersistenceParameters::BinaryDataColumnSize()]),
    m_currentLargeDataSize(0),
    m_typename(new char[Safir::Dob::PersistenceParameters::TypeNameColumnSize()]),
    m_typenameW(new wchar_t[Safir::Dob::PersistenceParameters::TypeNameColumnSize()]),
    m_currentTypenameSize(0),
    m_deleteAllStatement(SQL_NULL_HANDLE),
    m_deleteAllIsValid(false),
    m_deleteStatement(SQL_NULL_HANDLE),
    m_deleteIsValid(false),
    m_debug(L"OdbcPersistor")
{
    m_debug << "Using ODBC connect string " << Safir::Dob::PersistenceParameters::OdbcStorageConnectString() <<std::endl;

    try
    {
        SQLRETURN ret = ::SQLAllocHandle(SQL_HANDLE_ENV, SQL_NULL_HANDLE, &m_environment);
        if (!SQL_SUCCEEDED(ret))
        {
            OdbcHelper::ThrowException(SQL_HANDLE_ENV, SQL_NULL_HANDLE);
        }

        // SetEnvAttr(SQL_ATTR_ODBC_VERSION, SQL_OV_ODBC3);
        ret = ::SQLSetEnvAttr(m_environment,
                              SQL_ATTR_ODBC_VERSION,
                              reinterpret_cast<SQLPOINTER>(SQL_OV_ODBC3),
                              SQL_IS_UINTEGER);
        if (!SQL_SUCCEEDED(ret))
        {
            OdbcHelper::ThrowException(SQL_HANDLE_ENV, m_environment);
        }

        ret = ::SQLAllocHandle(SQL_HANDLE_DBC, m_environment, &m_odbcConnection);
        if (!SQL_SUCCEEDED(ret))
        {
            OdbcHelper::ThrowException(SQL_HANDLE_ENV, m_environment);
        }
    }
    catch(const OdbcException& ex)
    {
        Safir::Logging::SendSystemLog
            (Safir::Logging::Error,
             Safir::Dob::Typesystem::Utilities::ToWstring(ex.what()));

    }
}


//-------------------------------------------------------
OdbcPersistor::~OdbcPersistor()
{
    try
    {
        DisconnectOdbcConnection();

        FreeConnection(m_odbcConnection);

        const SQLRETURN ret = ::SQLFreeHandle(SQL_HANDLE_ENV, m_environment);
        if (!SQL_SUCCEEDED(ret))
        {
            OdbcHelper::ThrowException(SQL_HANDLE_ENV, m_environment);
        }
    }
    catch(const OdbcException& ex)
    {
        Safir::Logging::SendSystemLog
            (Safir::Logging::Error,
             Safir::Dob::Typesystem::Utilities::ToWstring(ex.what()));

    }
}


void OdbcPersistor::PerformStartupChecks()
{
    int connectionAttempts = 0;
    bool errorReported = false;

    const int xmlSize = Safir::Dob::PersistenceParameters::XmlDataColumnSize();

    SQLHDBC                                     checkUnicodeConnection = SQL_NULL_HANDLE;
    bool                                        isConnected = false;
    bool                                        deleteUnicodeIsValid = false;
    SQLHSTMT                                    deleteUnicodeStatement = SQL_NULL_HANDLE;
    bool                                        writeUnicodeIsValid = false;
    SQLHSTMT                                    writeUnicodeStatement = SQL_NULL_HANDLE;
    bool                                        readUnicodeIsValid = false;
    SQLHSTMT                                    readUnicodeStatement = SQL_NULL_HANDLE;

    const std::wstring                          data = L"testelitest\n\u00e4\u203d.";
    boost::scoped_array<char>                   xmlDataParam(new char[Safir::Dob::PersistenceParameters::XmlDataColumnSize()]);
    boost::scoped_array<wchar_t>                xmlDataParamW(new wchar_t[Safir::Dob::PersistenceParameters::XmlDataColumnSize() / sizeof(wchar_t)]);
    SQLLEN                                      xmlDataParamSize(0);

    boost::scoped_array<char>                   xmlBuffer(new char[xmlSize]);
    boost::scoped_array<wchar_t>                xmlBufferW(new wchar_t[xmlSize / sizeof(wchar_t)]);
    SQLLEN                                      currentXmlSize = 0;

    const SQLRETURN ret = ::SQLAllocHandle(SQL_HANDLE_DBC, m_environment, &checkUnicodeConnection);
    if (!SQL_SUCCEEDED(ret))
    {
        OdbcHelper::ThrowException(SQL_HANDLE_ENV, m_environment);
    }

    m_debug << "Performing Unicode Database Read/Write test" << std::endl;

    bool done = false;
    while (!done)
    {
        try
        {
            ConnectIfNeeded(checkUnicodeConnection, isConnected, connectionAttempts);

            m_debug << " - Will delete any old test rows from db" << std::endl;
            //delete the test row from the db, if it is there
            for (;;)
            {
                //deleteUnicode statement execution (will loop here until we successfully execute)
                if (!deleteUnicodeIsValid)
                {
                    m_helper.AllocStatement(&deleteUnicodeStatement, checkUnicodeConnection);
                    m_helper.Prepare(deleteUnicodeStatement,
                                     "delete from PersistentEntity where typeid = 0 and instance = 0");
                }
                try
                {
                    m_helper.Execute(deleteUnicodeStatement);
                    break;
                }
                catch(const OdbcException& e)
                {
                    const std::wstring err = Safir::Dob::Typesystem::Utilities::ToWstring(e.what());
                    m_debug << "Caught a RetryException in PerformStartupChecks:\n" << err << std::endl;
                    boost::this_thread::sleep_for(RETRY_EXCEPTION_DELAY);
                }
            }

            m_debug << " - Writing Unicode string to db" << std::endl;
            //now write some unicode stuff in the db
            for (;;)
            {
                //writeUnicode statement execution (will loop here until we successfully execute)
                if (!writeUnicodeIsValid)
                {
                    m_helper.AllocStatement(&writeUnicodeStatement, checkUnicodeConnection);
                    m_helper.Prepare(writeUnicodeStatement,
                                     "INSERT INTO PersistentEntity (typeid, instance, typename, xmldata) "
                                     "values (0, 0, 'UnicodeTestingPlaceholder', ?)");

                    if (USE_CHAR_OPERATIONS_FOR_TEXT_COLUMNS)
                    {
                        OdbcHelper::BindParamString(writeUnicodeStatement,
                                                    1,
                                                    xmlSize,
                                                    xmlDataParam.get(),
                                                    &xmlDataParamSize);
                        const std::string utf8 = Safir::Dob::Typesystem::Utilities::ToUtf8(data);

                        const size_t size = (utf8.size() + 1)* sizeof (char);
                        if (size > static_cast<size_t>(Safir::Dob::PersistenceParameters::XmlDataColumnSize()))
                        {
                            throw Safir::Dob::Typesystem::SoftwareViolationException
                                (L"The size in bytes of the unicode test data"
                                 L" exceeds Safir.Dob.PersistenceParameters.XmlDataColumnSize",
                                 __WFILE__, __LINE__);
                        }
                        memcpy(xmlDataParam.get(), utf8.c_str(), size);


                    }
                    else
                    {
                        OdbcHelper::BindParamStringW(writeUnicodeStatement,
                                                     1,
                                                     xmlSize / sizeof(wchar_t),
                                                     xmlDataParamW.get(),
                                                     &xmlDataParamSize);
                        const size_t size = (data.size() + 1)* sizeof (wchar_t);
                        if (size > static_cast<size_t>(Safir::Dob::PersistenceParameters::XmlDataColumnSize()))
                        {
                            throw Safir::Dob::Typesystem::SoftwareViolationException
                                (L"The size in bytes of the unicode test data"
                                 L" exceeds Safir.Dob.PersistenceParameters.XmlDataColumnSize",
                                 __WFILE__, __LINE__);
                        }
                        memcpy(xmlDataParamW.get(), data.c_str(), size);
                    }
                }
                xmlDataParamSize = SQL_NTS;

                try
                {
                    m_helper.Execute(writeUnicodeStatement);
                    break;
                }
                catch(const OdbcException& e)
                {
                    const std::wstring err = Safir::Dob::Typesystem::Utilities::ToWstring(e.what());
                    m_debug << "Caught a RetryException in GetAll:\n" << err << std::endl;
                    boost::this_thread::sleep_for(RETRY_EXCEPTION_DELAY);
                }
            }

            m_debug << " - Reading the string again" << std::endl;
            //now read the data again and check that we got it right
            for (;;)
            {
                //readUnicode statement execution (will loop here until we successfully execute)
                if (!readUnicodeIsValid)
                {
                    m_helper.AllocStatement(&readUnicodeStatement, checkUnicodeConnection);
                    m_helper.Prepare(readUnicodeStatement,
                                     "SELECT xmldata FROM PersistentEntity "
                                     "WHERE typeid = 0 AND instance = 0");

                    if (USE_CHAR_OPERATIONS_FOR_TEXT_COLUMNS)
                    {
                        BindColumnString(readUnicodeStatement, 1, xmlSize, xmlBuffer.get(), &currentXmlSize);
                    }
                    else
                    {
                        BindColumnStringW(readUnicodeStatement,
                                          1,
                                          xmlSize / sizeof(wchar_t),
                                          xmlBufferW.get(),
                                          &currentXmlSize);
                    }
                }

                try
                {
                    m_helper.Execute(readUnicodeStatement);
                    m_helper.Fetch(readUnicodeStatement);
                    std::wstring readData;
                    if (USE_CHAR_OPERATIONS_FOR_TEXT_COLUMNS)
                    {
                        readData = Safir::Dob::Typesystem::Utilities::ToWstring(xmlBuffer.get());
                    }
                    else
                    {
                        readData = xmlBufferW.get();
                    }

                    if (readData != data)
                    {
                        // Define a lambda output iterator, that inserts spaces between each character
                        // in the hex output
                        int counter = 0;
                        std::function<void(const wchar_t c)> hexFormatter = [this, &counter](const wchar_t c)
                            {
                                ++counter;
                                m_debug << c;
                                if (counter % sizeof(wchar_t) * 2 == 0)
                                {
                                    m_debug << ' ';
                                }
                            };

                        m_debug << " - Unicode string mismatch, issuing warning.\n"
                                << " - Expected (hex): ";
                        boost::algorithm::hex(data,
                                              boost::make_function_output_iterator(hexFormatter));

                        m_debug << "\n"
                                << " - Got (hex)     : ";
                        counter = 0;
                        boost::algorithm::hex(readData,
                                              boost::make_function_output_iterator(hexFormatter));
                        m_debug << std::endl;

                        Safir::Logging::SendSystemLog
                            (Safir::Logging::Warning,
                             L"Database unicode encoding error! Failed to write and "
                             L"restore unicode string to persistence database.");
                    }
                    else
                    {
                        m_debug << " - String matched! Everything is golden!" << std::endl;
                    }
                    break;
                }
                catch(const OdbcException& e)
                {
                    const std::wstring err = Safir::Dob::Typesystem::Utilities::ToWstring(e.what());
                    m_debug << "Caught a RetryException in GetAll:\n" << err << std::endl;
                    boost::this_thread::sleep_for(RETRY_EXCEPTION_DELAY);
                }
            }

            m_debug << " - And delete the test row from the db" << std::endl;
            //delete the test row from the db,
            for (;;)
            {
                //deleteUnicode statement execution (will loop here until we successfully execute)
                if (!deleteUnicodeIsValid)
                {
                    m_helper.AllocStatement(&deleteUnicodeStatement, checkUnicodeConnection);
                    m_helper.Prepare(deleteUnicodeStatement,
                                     "delete from PersistentEntity where typeid = 0 and instance = 0");
                }
                try
                {
                    m_helper.Execute(deleteUnicodeStatement);
                    done = true;
                    break;
                }
                catch(const OdbcException& e)
                {
                    const std::wstring err = Safir::Dob::Typesystem::Utilities::ToWstring(e.what());
                    m_debug << "Caught a RetryException in PerformStartupChecks:\n" << err << std::endl;
                    boost::this_thread::sleep_for(RETRY_EXCEPTION_DELAY);
                }
            }
        }
        catch(const OdbcException& e)
        {
            const std::wstring err = Safir::Dob::Typesystem::Utilities::ToWstring(e.what());
            m_debug << "Caught a ReconnectException in PerformStartupChecks:\n" << err << std::endl;
            if (connectionAttempts > REPORT_AFTER_RECONNECTS && !errorReported)
            {
                Safir::Logging::SendSystemLog(Safir::Logging::Error,
                                              L"PerformStartupChecks: Failed to connect to the database, will keep trying. Exception info: " +
                                              err);
                errorReported = true;
            }

            if (isConnected)
            {
                OdbcHelper::Disconnect(checkUnicodeConnection);
                isConnected = false;
            }

            writeUnicodeIsValid = false;

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

    try
    {
        if (isConnected)
        {
            OdbcHelper::Disconnect(checkUnicodeConnection);
        }
        FreeConnection(checkUnicodeConnection);
    }
    catch(const OdbcException& e)
    {
        const std::wstring err = Safir::Dob::Typesystem::Utilities::ToWstring(e.what());
        Safir::Logging::SendSystemLog(Safir::Logging::Error,
                                        L"Whoops. Error while disconnecting the checkUnicodeConnection. Ignoring this and moving on. Exception info: " +
                                        err);
    }

    m_debug << "Unicode Database Read/Write test is complete" << std::endl;

}

//-------------------------------------------------------
void OdbcPersistor::Store(const Safir::Dob::Typesystem::EntityId& entityId,
                          const Safir::Dob::Typesystem::HandlerId& handlerId,
                          Safir::Dob::Typesystem::BinarySerialization& bin,
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
            ConnectIfNeeded(m_odbcConnection, m_isOdbcConnected, retries);

            if (!m_storeIsValid)
            {
                m_helper.AllocStatement(&m_storeStatement, m_odbcConnection);
                m_storeIsValid = true;

                m_helper.Prepare(m_storeStatement,
                                 "UPDATE PersistentEntity "
                                 "SET xmlData=NULL, binarySmallData=?, binaryData=?, handlerid=? "
                                 "WHERE typeId=? AND instance=?");

                BindParamBinary(m_storeStatement, 1, Safir::Dob::PersistenceParameters::BinarySmallDataColumnSize(), m_storeBinarySmallData.get(), &m_currentSmallDataSize);
                BindParamBinary(m_storeStatement, 2, Safir::Dob::PersistenceParameters::BinaryDataColumnSize(), m_storeBinaryLargeData.get(), &m_currentLargeDataSize);
                m_helper.BindParamInt64(m_storeStatement, 3, &m_handler);
                m_helper.BindParamInt64(m_storeStatement, 4, &m_type);
                m_helper.BindParamInt64(m_storeStatement, 5, &m_instance);

                SetStmtTimeout(m_storeStatement);

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
                //If this is not an update we need to insert a row in the table for our entity
                //before storing it.
                Insert(entityId);
            }

            m_helper.Execute(m_storeStatement);

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
        catch(const OdbcException& e)
        {
            const std::wstring err = Safir::Dob::Typesystem::Utilities::ToWstring(e.what());
            m_debug << "Caught a ReconnectException in Store:\n" << err << std::endl;
            if (retries > REPORT_AFTER_RECONNECTS && !errorReported)
            {
                Safir::Logging::SendSystemLog(Safir::Logging::Error,
                                              L"Store: Failed to connect to the database, will keep trying. Exception info: " +
                                              err);
                errorReported = true;
            }

            DisconnectOdbcConnection();

            boost::this_thread::sleep_for(RECONNECT_EXCEPTION_DELAY);
        }
    }
}

//-------------------------------------------------------
void OdbcPersistor::Remove(const Safir::Dob::EntityProxy& entityProxy)
{
    Remove(entityProxy.GetEntityId());
}

//-------------------------------------------------------
void OdbcPersistor::Remove(const Safir::Dob::Typesystem::EntityId& entityId)
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
            ConnectIfNeeded(m_odbcConnection, m_isOdbcConnected, retries);

            if (!m_deleteIsValid)
            {
                m_helper.AllocStatement(&m_deleteStatement, m_odbcConnection);
                m_helper.Prepare(m_deleteStatement, "DELETE FROM PersistentEntity WHERE typeId=? AND instance=?");
                m_helper.BindParamInt64(m_deleteStatement, 1, &m_type);
                m_helper.BindParamInt64(m_deleteStatement, 2, &m_instance);
                SetStmtTimeout(m_deleteStatement);
                m_deleteIsValid = true;
                paramSet = false;
            }

            if (!paramSet)
            {
                m_type = entityId.GetTypeId();
                m_instance = entityId.GetInstanceId().GetRawValue();
                paramSet = true;
            }

            m_helper.Execute(m_deleteStatement);
            done = true;
            if (errorReported)
            {
                Safir::Logging::SendSystemLog(Safir::Logging::Informational,
                                                L"Successfully connected to the database");
                errorReported = false;
                retries = 0;
            }
        }
        catch(const OdbcException& e)
        {
            const std::wstring err = Safir::Dob::Typesystem::Utilities::ToWstring(e.what());
            m_debug << "Caught a ReconnectException in Delete:\n" << err << std::endl;
            if (retries > REPORT_AFTER_RECONNECTS && !errorReported)
            {
                Safir::Logging::SendSystemLog(Safir::Logging::Error,
                                              L"Remove: Failed to connect to the database, will keep trying. Exception info: " +
                                              err);
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
    m_debug << "Deleting all" <<std::endl;
    int retries = 0;
    bool errorReported = false;
    bool done = false;

    while (!done)
    {
        try
        {
            ConnectIfNeeded(m_odbcConnection, m_isOdbcConnected, retries);

            if (!m_deleteAllIsValid)
            {
                m_helper.AllocStatement(&m_deleteAllStatement, m_odbcConnection);
                m_deleteAllIsValid = true;

                m_helper.Prepare(m_deleteAllStatement, "DELETE FROM PersistentEntity");

                SetStmtTimeout(m_deleteAllStatement);
            }

            m_helper.Execute(m_deleteAllStatement);

            done = true;
            if (errorReported)
            {
                Safir::Logging::SendSystemLog(Safir::Logging::Informational,
                                                L"Successfully connected to the database");
                errorReported = false;
                retries = 0;
            }
        }
        catch(const OdbcException& e)
        {
            const std::wstring err = Safir::Dob::Typesystem::Utilities::ToWstring(e.what());
            m_debug << "Caught a ReconnectException in RemoveAll:\n" << err << std::endl;
            if (retries > REPORT_AFTER_RECONNECTS && !errorReported)
            {
                Safir::Logging::SendSystemLog(Safir::Logging::Error,
                                              L"DeleteAll: Failed to connect to the database, will keep trying. Exception info: " +
                                              err);
                errorReported = true;
            }

            DisconnectOdbcConnection();
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

    SQLHDBC                                     getAllConnection = SQL_NULL_HANDLE;
    bool                                        isConnected = false;
    bool                                        getAllIsValid = false;
    SQLHSTMT                                    getAllStatement = SQL_NULL_HANDLE;
    Safir::Dob::Typesystem::Int64               typeId = 0;
    Safir::Dob::Typesystem::Int64               instance = 0;
    Safir::Dob::Typesystem::Int64               handlerId = 0;
    boost::scoped_array<unsigned char>          storeBinarySmallData(new unsigned char[binarySmallSize]);
    SQLLEN                                      currentSmallDataSize = 0;
    boost::scoped_array<unsigned char>          storeBinaryLargeData(new unsigned char[binaryLargeSize]);
    SQLLEN                                      currentLargeDataSize = 0;
    boost::scoped_array<char>                   xmlBuffer(new char[xmlSize]);
    boost::scoped_array<wchar_t>                xmlBufferW(new wchar_t[xmlSize / sizeof(wchar_t)]);
    SQLLEN                                      currentXmlSize = 0;

    const boost::chrono::steady_clock::time_point startTime = boost::chrono::steady_clock::now();

    const SQLRETURN ret = ::SQLAllocHandle(SQL_HANDLE_DBC, m_environment, &getAllConnection);
    if (!SQL_SUCCEEDED(ret))
    {
        OdbcHelper::ThrowException(SQL_HANDLE_ENV, m_environment);
    }

    bool done = false;
    while (!done)
    {
        try
        {
            ConnectIfNeeded(getAllConnection, isConnected, connectionAttempts);

            for (;;)
            {
                //getAll statement execution (will loop here until we successfully execute)
                if (!getAllIsValid)
                {
                    m_helper.AllocStatement(&getAllStatement, getAllConnection);
                    m_helper.Prepare(
                        getAllStatement,
                        "SELECT typeId, instance, handlerid, xmlData, binaryData, binarySmallData "
                        "FROM PersistentEntity");
                    m_helper.BindColumnInt64(getAllStatement, 1, &typeId);
                    m_helper.BindColumnInt64(getAllStatement, 2, &instance);
                    m_helper.BindColumnInt64(getAllStatement, 3, &handlerId);

                    if (USE_CHAR_OPERATIONS_FOR_TEXT_COLUMNS)
                    {
                        BindColumnString(getAllStatement, 4, xmlSize, xmlBuffer.get(), &currentXmlSize);
                    }
                    else
                    {
                        BindColumnStringW(getAllStatement,
                                          4,
                                          xmlSize / sizeof(wchar_t),
                                          xmlBufferW.get(),
                                          &currentXmlSize);
                    }

                    m_helper.BindColumnBinary(getAllStatement, 5, binaryLargeSize, storeBinaryLargeData.get(), &currentLargeDataSize);
                    m_helper.BindColumnBinary(getAllStatement, 6, binarySmallSize, storeBinarySmallData.get(), &currentSmallDataSize);
                }

                try
                {
                    m_helper.Execute(getAllStatement);
                    break;
                }
                catch(const OdbcException& e)
                {
                    const std::wstring err = Safir::Dob::Typesystem::Utilities::ToWstring(e.what());
                    m_debug << "Caught a RetryException in GetAll:\n" << err << std::endl;
                    boost::this_thread::sleep_for(RETRY_EXCEPTION_DELAY);
                }
            }

            for (;;)
            {
                if (!m_helper.Fetch(getAllStatement))
                {//we've got all rows!
                    done = true;
                    break;
                }

                const Safir::Dob::Typesystem::EntityId entityId
                    (typeId,
                     Safir::Dob::Typesystem::InstanceId(instance));

                const Safir::Dob::Typesystem::HandlerId handler(handlerId);

                auto findIt = GetPersistentTypes().find(entityId.GetTypeId());
                if (findIt == GetPersistentTypes().end())
                { //not to be persisted!
                    Remove(entityId);
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
                        std::wstring xml;

                        if (USE_CHAR_OPERATIONS_FOR_TEXT_COLUMNS)
                        {
                           xml = Safir::Dob::Typesystem::Utilities::ToWstring(xmlBuffer.get());
                        }
                        else
                        {
                            xml = xmlBufferW.get();
                        }
                        m_debug
                            << "Restoring from xml"                 << entityId
                            << ", size = "                          << xml.size()
                            << ". First 100 chars of the data: "    << xml.substr(0,100)
                            << std::endl;

                        const Safir::Dob::Typesystem::ObjectPtr object =
                            Safir::Dob::Typesystem::Serialization::ToObject(xml);
                        const Safir::Dob::EntityPtr entity =
                            std::dynamic_pointer_cast<Safir::Dob::Entity>(object);
                        m_debug << "Successfully deserialized" <<std::endl;

                        injector.InitialSet(entity, entityId.GetInstanceId(), handler);
                        m_debug << "InitialSet successful"<<std::endl;

                        Safir::Dob::Typesystem::BinarySerialization bin;
                        Safir::Dob::Typesystem::Serialization::ToBinary(entity, bin);
                        Store(entityId, handler, bin, true);
                        m_debug << "Stored it as binary" << std::endl;
                    }
                    else if (currentSmallDataSize != SQL_NULL_DATA)
                    {
                        const char * const data = reinterpret_cast<char *>(storeBinarySmallData.get());
                        m_debug << "Restoring " << entityId << " from binary " <<std::endl;

                        Safir::Dob::EntityPtr entity =
                            std::dynamic_pointer_cast<Safir::Dob::Entity>
                            (Safir::Dob::Typesystem::ObjectFactory::Instance().CreateObject(data));
                        m_debug << "Successfully deserialized" <<std::endl;

                        injector.InitialSet(entity, entityId.GetInstanceId(), handler);
                        m_debug << "InitialSet successful"<<std::endl;
                    }
                    else
                    {
                        if (currentLargeDataSize != SQL_NULL_DATA)
                        { //some binarypersistent data set
                            const char * const data = reinterpret_cast<char *>(storeBinaryLargeData.get());
                            m_debug << "Restoring " << entityId << " from binary " <<std::endl;

                            Safir::Dob::EntityPtr entity =
                                std::dynamic_pointer_cast<Safir::Dob::Entity>
                                (Safir::Dob::Typesystem::ObjectFactory::Instance().CreateObject(data));
                            m_debug << "Successfully deserialized" <<std::endl;

                            injector.InitialSet(entity, entityId.GetInstanceId(), handler);
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
                catch(const Safir::Dob::Typesystem::IllegalValueException & e)
                {
                    m_debug << "Could not restore "
                            << entityId.ToString()
                            << ", removing it. (Got exception: " << e.what() << ")" << std::endl;

                    Safir::Logging::SendSystemLog(Safir::Logging::Error,
                                                    L"Failed to restore entity" +
                                                    entityId.ToString() +
                                                    L", will remove persisted data.");

                    //we don't want to try it again if the connection fails later.
                    restoredObjects.insert(entityId);
                    //remove the row from the db
                    Remove(entityId);
                    //since we did not remove the objectid from persistentObjects an empty row will be inserted below.
                }
                catch (const Safir::Dob::LowMemoryException&)
                {
                    Safir::Logging::SendSystemLog(Safir::Logging::Emergency,
                                                  L"Failed to inject persisted entities into system due to lack of shared memory. Exiting.");
                    m_ioService.stop();
                }
            }
        }
        catch(const OdbcException& e)
        {
            const std::wstring err = Safir::Dob::Typesystem::Utilities::ToWstring(e.what());
            m_debug << "Caught a ReconnectException in RestoreAll:\n" << err << std::endl;
            if (connectionAttempts > REPORT_AFTER_RECONNECTS && !errorReported)
            {
                Safir::Logging::SendSystemLog(Safir::Logging::Error,
                                              L"RestoreAll: Failed to connect to the database, will keep trying. Exception info: " +
                                              err);
                errorReported = true;
            }

            if (isConnected)
            {
                OdbcHelper::Disconnect(getAllConnection);
                isConnected = false;
            }

            getAllIsValid = false;

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

    try
    {
        if (isConnected)
        {
            OdbcHelper::Disconnect(getAllConnection);
        }
        FreeConnection(getAllConnection);
    }
    catch(const OdbcException& e)
    {
        const std::wstring err = Safir::Dob::Typesystem::Utilities::ToWstring(e.what());
        Safir::Logging::SendSystemLog(Safir::Logging::Error,
                                        L"Whoops. Error while disconnecting the getAllConnection. Ignoring this and moving on. Exception info: " +
                                        err);
    }

    m_debug << "RestoreAll completed: "
            << restoredObjects.size()
            << " objects restored in time "
            << boost::chrono::steady_clock::now() - startTime << std::endl;
}

//-------------------------------------------------------
void
OdbcPersistor::Insert(const Safir::Dob::Typesystem::EntityId& entityId)
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
            ConnectIfNeeded(m_odbcConnection, m_isOdbcConnected, retries);

            if (!m_insertIsValid)
            {
                m_helper.AllocStatement(&m_rowExistsStatement, m_odbcConnection);

                m_helper.AllocStatement(&m_insertStatement, m_odbcConnection);
                m_insertIsValid = true;

                m_helper.Prepare
                    (m_rowExistsStatement,
                     "SELECT count(*) as antal from PersistentEntity where typeId=? AND instance=? ");

                m_helper.Prepare
                    (m_insertStatement,
                     "INSERT INTO PersistentEntity (typeid, instance, typename) "
                     "values (?, ?, ?)");

                m_helper.BindParamInt64(m_rowExistsStatement, 1, &m_type);
                m_helper.BindParamInt64(m_rowExistsStatement, 2, &m_instance);

                m_helper.BindColumnInt64(m_rowExistsStatement,1, &m_rowCount);

                m_helper.BindParamInt64(m_insertStatement, 1, &m_type);
                m_helper.BindParamInt64(m_insertStatement, 2, &m_instance);

                if (USE_CHAR_OPERATIONS_FOR_TEXT_COLUMNS)
                {
                    m_helper.BindParamString(m_insertStatement,
                                             3,
                                             Safir::Dob::PersistenceParameters::TypeNameColumnSize(),
                                             m_typename.get(),
                                             &m_currentTypenameSize);
                }
                else
                {
                    m_helper.BindParamStringW(m_insertStatement,
                                              3,
                                              Safir::Dob::PersistenceParameters::TypeNameColumnSize() / sizeof(wchar_t),
                                              m_typenameW.get(),
                                              &m_currentTypenameSize);
                }

                SetStmtTimeout(m_insertStatement);
                SetStmtTimeout(m_rowExistsStatement);

                paramSet = false;
            }

            if (!paramSet)
            {
                m_type = entityId.GetTypeId();
                m_instance = entityId.GetInstanceId().GetRawValue();

                const std::wstring typeName = Safir::Dob::Typesystem::Operations::GetName(m_type);
                if (USE_CHAR_OPERATIONS_FOR_TEXT_COLUMNS)
                {
                    const std::string typeNameUtf8 = Safir::Dob::Typesystem::Utilities::ToUtf8(typeName);

                    const size_t size = (typeNameUtf8.size() + 1)* sizeof (char);
                    if (size > static_cast<size_t>(Safir::Dob::PersistenceParameters::TypeNameColumnSize()))
                    {
                        throw Safir::Dob::Typesystem::SoftwareViolationException
                            (L"The size in bytes of '" + typeName +
                             L"' exceeds Safir.Dob.PersistenceParameters.TypeNameColumnSize",
                             __WFILE__, __LINE__);
                    }
                    memcpy(m_typename.get(), typeNameUtf8.c_str(), size);
                }
                else
                {
                    const size_t size = (typeName.size() + 1)* sizeof (wchar_t);
                    if (size > static_cast<size_t>(Safir::Dob::PersistenceParameters::TypeNameColumnSize()))
                    {
                        throw Safir::Dob::Typesystem::SoftwareViolationException
                            (L"The size in bytes of '" + typeName +
                             L"' exceeds Safir.Dob.PersistenceParameters.TypeNameColumnSize",
                             __WFILE__, __LINE__);
                    }
                    memcpy(m_typenameW.get(), typeName.c_str(), size);
                }
                m_currentTypenameSize = SQL_NTS;

                paramSet = true;
            }

            m_helper.Execute(m_rowExistsStatement);

            bool bExecuteInsert = false;
            if (m_helper.Fetch(m_rowExistsStatement))
            {
                // Insert if no row exist
                if (m_rowCount <= 0)
                {
                    bExecuteInsert = true;
                }
            }

            CloseCursor(m_rowExistsStatement);

            if (bExecuteInsert)
            {
                m_helper.Execute(m_insertStatement);
            }

            done = true;
            if (errorReported)
            {
                Safir::Logging::SendSystemLog(Safir::Logging::Informational,
                                                L"Successfully connected to the database");
                errorReported = false;
                retries = 0;
            }

        }
        catch(const OdbcException& e)
        {
            const std::wstring err = Safir::Dob::Typesystem::Utilities::ToWstring(e.what());
            m_debug << "Caught a ReconnectException in Insert:\n" << err << std::endl;
            if (retries > REPORT_AFTER_RECONNECTS && !errorReported)
            {
                Safir::Logging::SendSystemLog(Safir::Logging::Error,
                                              L"Insert: Failed to connect to the database, will keep trying. Exception info: " +
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
    if (m_isOdbcConnected)
    {
        OdbcHelper::Disconnect(m_odbcConnection);
        m_isOdbcConnected = false;
    }

    m_storeIsValid = false;
    m_insertIsValid = false;
    m_deleteAllIsValid = false;
    m_deleteIsValid = false;
}


void
OdbcPersistor::ConnectIfNeeded(SQLHDBC connection,
                               bool& isConnected,
                               int& connectionAttempts)
{
    if(!isConnected)
    {
        ++connectionAttempts;
        m_debug << L"Connecting to database, attempt " << connectionAttempts << std::endl;

        const auto connectionString = Safir::Dob::Typesystem::Utilities::ToUtf8
        (Safir::Dob::PersistenceParameters::OdbcStorageConnectString());

        SQLRETURN ret = ::SQLDriverConnectA
            (connection,
             NULL,
             reinterpret_cast<SQLCHAR*>(const_cast<char *>(connectionString.c_str())),
             SQL_NTS,
             NULL,
             0,
             NULL,
             SQL_DRIVER_NOPROMPT);
        if (SQL_SUCCEEDED(ret))
        {
            isConnected = true;
        }
        else
        {
            OdbcHelper::ThrowException(SQL_HANDLE_DBC, connection);
        }

        // SQLSetConnectAttr(SQL_ATTR_AUTOCOMMIT, SQL_AUTOCOMMIT_ON);
        ret = ::SQLSetConnectAttr(connection,
                                    SQL_ATTR_AUTOCOMMIT,
                                    reinterpret_cast<SQLPOINTER>(SQL_AUTOCOMMIT_ON),
                                    SQL_IS_UINTEGER);
        if (!SQL_SUCCEEDED(ret))
        {
            OdbcHelper::ThrowException(SQL_HANDLE_DBC, connection);
        }
    }
}


void
OdbcPersistor::SetStmtTimeout(SQLHSTMT statement)
{
    const SQLRETURN ret = ::SQLSetStmtAttr(statement,
                                           SQL_ATTR_QUERY_TIMEOUT,
                                           reinterpret_cast<SQLPOINTER>(15),
                                           SQL_IS_UINTEGER);
    if (!SQL_SUCCEEDED(ret))
    {
        OdbcHelper::ThrowException(SQL_HANDLE_STMT,statement);
    }
}

void
OdbcPersistor::CloseCursor(SQLHSTMT statement)
{
    const SQLRETURN ret = ::SQLCloseCursor(statement);
    if (!SQL_SUCCEEDED(ret))
    {
        OdbcHelper::ThrowException(SQL_HANDLE_STMT,statement);
    }
}


void
OdbcPersistor::BindParamBinary(SQLHSTMT statement,
                               const SQLUSMALLINT paramNumber,
                               const SQLUINTEGER maxSize,
                               unsigned char* buffer,
                               SQLLEN* sizePtr)
{
    const SQLRETURN ret = ::SQLBindParameter(statement,
                                             paramNumber,
                                             SQL_PARAM_INPUT,
                                             SQL_C_BINARY,
                                             SQL_LONGVARBINARY,
                                             maxSize,
                                             0,
                                             buffer,
                                             maxSize,
                                             sizePtr );
    if (!SQL_SUCCEEDED(ret))
    {
        OdbcHelper::ThrowException(SQL_HANDLE_STMT,statement);
    }
}

void
OdbcPersistor::BindColumnString(SQLHSTMT statement,
                                unsigned short columnNumber,
                                const int maxSize,
                                char* string,
                                SQLLEN* sizePtr)
{
    const SQLRETURN ret = ::SQLBindCol(statement,
                                       columnNumber,
                                       SQL_C_CHAR,
                                       string,
                                       maxSize,
                                       sizePtr);
    if (!SQL_SUCCEEDED(ret))
    {
        OdbcHelper::ThrowException(SQL_HANDLE_STMT, statement);
    }
}


void
OdbcPersistor::BindColumnStringW(SQLHSTMT statement,
                                 unsigned short columnNumber,
                                 const int maxSize,
                                 wchar_t* string,
                                 SQLLEN* sizePtr)
{
    const SQLRETURN ret = ::SQLBindCol(statement,
                                       columnNumber,
                                       SQL_C_WCHAR,
                                       string,
                                       maxSize * sizeof(wchar_t),
                                       sizePtr);
    if (!SQL_SUCCEEDED(ret))
    {
        OdbcHelper::ThrowException(SQL_HANDLE_STMT, statement);
    }
}

void
OdbcPersistor::FreeConnection(SQLHDBC connection)
{
    const SQLRETURN ret = ::SQLFreeHandle(SQL_HANDLE_DBC, connection);
    if (!SQL_SUCCEEDED(ret))
    {
        OdbcHelper::ThrowException(SQL_HANDLE_DBC, connection);
    }
}

#endif
