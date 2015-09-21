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
#ifndef __DOPE_ODBC_PERSITOR_H__
#define __DOPE_ODBC_PERSITOR_H__

#include <Safir/Application/Tracer.h>

#include "PersistenceHandler.h"

#include <boost/scoped_array.hpp>

#include <sqltypes.h>
#include <sql.h>
#include <sqlext.h>


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
    virtual ~OdbcPersistor();

private:
    //implementation of pure virtual from Backend
    virtual void Store(const Safir::Dob::Typesystem::EntityId entityId,
                       const Safir::Dob::Typesystem::HandlerId handlerId,
                       Safir::Dob::Typesystem::BinarySerialization & bin,
                       const bool update);

    //implementation of pure virtual from Backend
    virtual void RestoreAll();

    //implementation of pure virtual from Backend
    virtual void Remove(const Safir::Dob::EntityProxy & entityProxy);

    //implementation of pure virtual from Backend
    virtual void RemoveAll();

    //Insert an empty row into the db
    void Insert(const Safir::Dob::Typesystem::EntityId & entityId);

    //Delete a row into the db
    void Delete(SQLHDBC connectionToUse,
                bool & connectionIsValid,
                const Safir::Dob::Typesystem::EntityId & entityId);

    //Delete all rows from the db
    void DeleteAll();

    // Bind one Int64 parameter to a statement. The Int64 is not nullable.
    void BindParamInt64(SQLHSTMT hStmt,
                        const SQLUSMALLINT paramNumber,
                        Safir::Dob::Typesystem::Int64 * value);

    // Bind one binary parameter to a statement. The binary are nullable.
    void BindParamBinary(SQLHSTMT hStmt,
                         const SQLUSMALLINT paramNumber,
                         const SQLUINTEGER maxSize,
                         unsigned char * buffer,
                         SQLINTEGER * sizePtr);

    // Bind one string to a statement. The string is nullable.
    void BindParamString(SQLHSTMT hStmt,
                         const SQLUSMALLINT paramNumber,
                         const SQLUINTEGER maxSize,
                         wchar_t * string,
                         SQLINTEGER * sizePtr);

    // Bind one Int64 column to a statement. The Int64 cannot be null.
    void BindColumnInt64( SQLHSTMT hStmt,
                          unsigned short usColumnNumber,
                          Safir::Dob::Typesystem::Int64 * value );

    // Bind one binary column to a statement. The binary can be null.
    void BindColumnBinary( SQLHSTMT hStmt,
                           unsigned short usColumnNumber,
                           const int maxSize,
                           unsigned char * buffer,
                           SQLINTEGER * sizePtr );

    // Bind one string column to a statement. The string can be null.
    void BindColumnString( SQLHSTMT hStmt,
                           unsigned short usColumnNumber,
                           const int maxSize,
                           wchar_t * string,
                           SQLINTEGER * sizePtr );

    // Connect to the database if necessary.
    void ConnectIfNeeded(SQLHDBC hConnection, bool & isConnected, int & connectionAttempts);

    // Prepare a sql query.
    void Prepare(SQLHSTMT hStmt, const std::wstring & sql);

    // Sets the timeout for a sql statement.
    void SetStmtTimeout(SQLHSTMT hStmt);

    // Execute a sql statement
    void Execute(SQLHSTMT hStmt);

    // Allocates a handle for a sql statement.
    void AllocStatement(SQLHSTMT * hStmt, SQLHDBC hConnection);

    // Fetches data from an executed statement into the columns bound to the statement.
    bool Fetch( SQLHSTMT hStmt );

    // Free an previously allocated connection
    void Free( SQLHDBC hConnection );

    // Disconnects a connection.
    void Disconnect( SQLHDBC hConnection );

    // Reads the error message and throws an std::exception
    void ThrowException(SQLSMALLINT   HandleType,
                        SQLHANDLE     Handle);

    // Disconnects the ODBC connection with all boolean correctly reset.
    void DisconnectOdbcConnection();

    // Closes a cursor previously opened with Fetch().
    void CloseCursor(SQLHSTMT hStmt);


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

    /**
     * Statement used for to update a row in db.
     */
    SQLHSTMT                                    m_hStoreStatement;
    SQLHSTMT                                    m_hInsertStatement;
    SQLHSTMT                                    m_hRowExistsStatement;
    boost::scoped_array<unsigned char>          m_storeBinarySmallData;
    SQLINTEGER                                  m_currentSmallDataSize;
    boost::scoped_array<unsigned char>          m_storeBinaryLargeData;
    SQLINTEGER                                  m_currentLargeDataSize;
    Safir::Dob::Typesystem::Int64               m_handler;
    SQLINTEGER                                  m_currentInt64Size;
    Safir::Dob::Typesystem::Int64               m_type;
    Safir::Dob::Typesystem::Int64               m_instance;
    boost::scoped_array<wchar_t>                m_typeName;
    SQLINTEGER                                  m_typeNameSize;
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

#endif
