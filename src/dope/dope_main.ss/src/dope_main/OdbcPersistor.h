/******************************************************************************
*
* Copyright Saab AB, 2006-2008 (http://www.safirsdk.com)
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


#include <Safir/Databases/Odbc/Connection.h>
#include <Safir/Databases/Odbc/Environment.h>
#include <Safir/Databases/Odbc/InputParameter.h>
#include <Safir/Databases/Odbc/Statement.h>

#include <Safir/Application/Tracer.h>

#include "PersistenceHandler.h"

//const long DATA_COLUMNS_SIZE = 10485760; //10M



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
    void Delete(Safir::Databases::Odbc::Connection & connectionToUse,
                const Safir::Dob::Typesystem::EntityId & entityId);

    //Delete all rows from the db
    void DeleteAll(Safir::Databases::Odbc::Connection & connectionToUse);

    /**
     * The main database connection.
     */
    Safir::Databases::Odbc::Connection  m_odbcConnection;

    /**
     * The database environment.
     */
    Safir::Databases::Odbc::Environment m_environment;


    /**
     * Statement used for to update a row in db.
     */
    Safir::Databases::Odbc::Statement      m_storeStatement;
    Safir::Databases::Odbc::Int64Parameter m_storeTypeIdParam;
    Safir::Databases::Odbc::Int64Parameter m_storeInstanceParam;
    Safir::Databases::Odbc::Int64Parameter m_storeHandlerParam;
    Safir::Databases::Odbc::LongBinaryParameter m_storeBinaryDataParam; //10M

    /**
     * Statement used to insert a null row in db
     */
    Safir::Databases::Odbc::Statement      m_insertStatement;
    Safir::Databases::Odbc::Int64Parameter m_insertTypeIdInsertParam;
    Safir::Databases::Odbc::Int64Parameter m_insertInstanceInsertParam;
    Safir::Databases::Odbc::WideStringParameter m_insertTypeNameParam; //236

    /**
     * Statement used to delete all rows in db
     */
    Safir::Databases::Odbc::Statement      m_deleteAllStatement;

    /**
     * Statement used to delete a row in db
     */
    Safir::Databases::Odbc::Statement      m_deleteStatement;
    Safir::Databases::Odbc::Int64Parameter m_deleteTypeIdParam;
    Safir::Databases::Odbc::Int64Parameter m_deleteInstanceParam;
    //delete has its own connection since it can be done
    // during a fetch
    Safir::Databases::Odbc::Connection     m_deleteConnection;


    Safir::Application::Tracer m_debug;
};

#endif
