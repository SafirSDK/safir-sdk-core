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
    void RemoveAll() override;

    //Insert an empty row into the db
    void Insert(const Safir::Dob::Typesystem::EntityId& entityId);

    //Delete a row into the db
    void Delete(SQLHDBC connectionToUse,
                bool& connectionIsValid,
                const Safir::Dob::Typesystem::EntityId& entityId);

    //Delete all rows from the db
    void DeleteAll();

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

    // Free an previously allocated connection
    static void Free(SQLHDBC connection);

    // Disconnects a connection.
    static void Disconnect(SQLHDBC connection);


    // Disconnects the ODBC connection with all boolean correctly reset.
    void DisconnectOdbcConnection();

    // Closes a cursor previously opened with Fetch().
    static void CloseCursor(SQLHSTMT statement);


    /**
     * The main database connection.
     */
    SQLHDBC                             m_hOdbcConnection;

    /**
     * The database environment.
     */
    SQLHENV                             m_hEnvironment;

    /**
     * The database connection used during RestoreAll
     */
    SQLHDBC                             m_hRestoreAllConnection;

    OdbcHelper                          m_helper;

    /**
     * Statement used for to update a row in db.
     */
    SQLHSTMT                                    m_hStoreStatement;
    SQLHSTMT                                    m_hInsertStatement;
    SQLHSTMT                                    m_hRowExistsStatement;
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
    SQLHSTMT                                    m_hDeleteAllStatement;

    /**
     * Statement used to delete a row in db
     */
    SQLHSTMT                                    m_hDeleteStatement;
    SQLHSTMT                                    m_hDeleteODBCStatement;

    // delete has its own connection since it can be done
    // during a fetch
    SQLHDBC                                     m_hDeleteConnection;

    /**
     * Keeps track of various Odbc actions.
     */
    bool                                        m_bIsOdbcConnected;
    bool                                        m_bIsRestoreAllConnected;
    bool                                        m_bStoreStatementIsValid;
    bool                                        m_bDeleteAllIsValid;
    bool                                        m_bDeleteIsConnected;
    bool                                        m_bInsertIsValid;
    bool                                        m_bDeleteIsValid;
    bool                                        m_bDeleteODBCIsValid;

    Safir::Application::Tracer m_debug;
};
