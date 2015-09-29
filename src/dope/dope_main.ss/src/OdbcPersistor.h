/******************************************************************************
*
* Copyright Saab AB, 2006-2015 (http://safir.sourceforge.net)
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
#pragma once

#include "OdbcHelper.h"
#include "PersistenceHandler.h"
#include <Safir/Application/Tracer.h>
#include <boost/scoped_array.hpp>


/**
 * Storage backend for ODBC databases
 */
class OdbcPersistor : public PersistenceHandler
{
public:
    /**
     * Constructor
     */
    explicit OdbcPersistor(boost::asio::io_service& ioService);

    /**
     * Destructor
     */
    ~OdbcPersistor();

private:
    void Store(const Safir::Dob::Typesystem::EntityId& entityId,
               const Safir::Dob::Typesystem::HandlerId& handlerId,
               Safir::Dob::Typesystem::BinarySerialization& bin,
               const bool update) override;

    void RestoreAll() override;
    void Remove(const Safir::Dob::EntityProxy& entityProxy) override;
    void Remove(const Safir::Dob::Typesystem::EntityId& entityId);
    void RemoveAll() override;

    //Insert an empty row into the db
    void Insert(const Safir::Dob::Typesystem::EntityId& entityId);

    // Connect to the database if necessary.
    void ConnectIfNeeded(SQLHDBC connection,
                         bool& isConnected,
                         int&connectionAttempts);

    // Sets the timeout for a sql statement.
    static void SetStmtTimeout(SQLHSTMT statement);

    // Bind one binary parameter to a statement. The binary are nullable.
    static void BindParamBinary(SQLHSTMT statement,
                                const SQLUSMALLINT paramNumber,
                                const SQLUINTEGER maxSize,
                                unsigned char* buffer,
                                SQLLEN* sizePtr);

    // Bind one string column to a statement. The string can be null.
    static void BindColumnString(SQLHSTMT statement,
                                 unsigned short usColumnNumber,
                                 const int maxSize,
                                 char* string,
                                 SQLLEN* sizePtr);

    // Bind one wide string column to a statement. The string can be null.
    static void BindColumnStringW(SQLHSTMT statement,
                                 unsigned short usColumnNumber,
                                 const int maxSize,
                                 wchar_t* string,
                                 SQLLEN* sizePtr);

    // Free an previously allocated connection
    static void FreeConnection(SQLHDBC connection);

    // Disconnects a connection.
    static void Disconnect(SQLHDBC connection);


    // Disconnects the ODBC connection with all boolean correctly reset.
    void DisconnectOdbcConnection();

    // Closes a cursor previously opened with Fetch().
    static void CloseCursor(SQLHSTMT statement);


    /**
     * The database environment.
     */
    SQLHENV                             m_environment;

    /**
     * The main database connection.
     */
    SQLHDBC                             m_odbcConnection;
    bool                                m_isOdbcConnected;

    OdbcHelper                          m_helper;

    /**
     * Statement used for to update a row in db.
     */
    SQLHSTMT                                    m_storeStatement;
    bool                                        m_storeIsValid;
    SQLHSTMT                                    m_insertStatement;
    bool                                        m_insertIsValid;
    SQLHSTMT                                    m_rowExistsStatement;
    boost::scoped_array<unsigned char>          m_storeBinarySmallData;
    SQLLEN                                      m_currentSmallDataSize;
    boost::scoped_array<unsigned char>          m_storeBinaryLargeData;
    SQLLEN                                      m_currentLargeDataSize;
    Safir::Dob::Typesystem::Int64               m_handler;
    Safir::Dob::Typesystem::Int64               m_type;
    Safir::Dob::Typesystem::Int64               m_instance;
    Safir::Dob::Typesystem::Int64               m_rowCount;


    /**
     * Statement used to delete all rows in db
     */
    SQLHSTMT                                    m_deleteAllStatement;
    bool                                        m_deleteAllIsValid;

    /**
     * Statement used to delete a row in db
     */
    SQLHSTMT                                    m_deleteStatement;
    bool                                        m_deleteIsValid;

    Safir::Application::Tracer m_debug;
};
