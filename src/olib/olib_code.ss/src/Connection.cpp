/******************************************************************************
*
* Copyright Saab AB, 2005-2013 (http://safirsdkcore.com)
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
#include "Safir/Databases/Odbc/Connection.h"
#include "Safir/Databases/Odbc/Environment.h"
#include "Safir/Databases/Odbc/Statement.h"

#include <Safir/Databases/Odbc/ReconnectException.h>
#include <Safir/Databases/Odbc/TimeoutException.h>
#include <Safir/Logging/Log.h>
#include "StringConversion.h"
#include "Diagnostics.h"
    

namespace Safir
{

namespace Databases
{

namespace Odbc
{

void Connection::CheckSQLWCHARSize(const size_t size)
{
    if (size != sizeof(SQLWCHAR))
    {
        throw Safir::Dob::Typesystem::SoftwareViolationException(L"Olib was compiled with a different size of SQLWCHAR than the code that uses it. Are you trying to mix iODBC and unixODBC?", __WFILE__, __LINE__);
    }
}

Connection::~Connection()
{
}

const StateMessagePair Connection::GetDiagRec() const
{
    return Safir::Databases::Odbc::GetDiagRec(SQL_HANDLE_DBC,
                                              m_hConnection);
}

    
void Connection::Alloc(const Environment & environment)
{
    SQLRETURN ret;

    // SQL_HANDLE_DBC is the handle we want to allocate.
    ret = ::SQLAllocHandle(SQL_HANDLE_DBC, environment.Handle(), &m_hConnection);
    if (!SQL_SUCCEEDED(ret))
    {
        // SQL_HANDLE_ENV is the handle sent into ThrowReconnectException and shouldn't
        // be the same as in SQLAllocHandle()
        ThrowReconnectException(SQL_HANDLE_ENV, environment.Handle(),__WFILE__,__LINE__ );
    }
    if (ret == SQL_SUCCESS_WITH_INFO) // Can only be 01000 - non odbc error.
    {
        const StateMessagePair rec = GetDiagRec();

        Safir::Logging::SendSystemLog(Safir::Logging::Warning,
                                      L"Olib: Non Odbc Error in Connection::Alloc " + rec.second);
    }
}

void Connection::Free()
{
    SQLRETURN ret;

    if (!IsValid())
        throw Safir::Dob::Typesystem::SoftwareViolationException(L"Freeing an invalid connection",__WFILE__,__LINE__);

    if (IsConnected())
        throw Safir::Dob::Typesystem::SoftwareViolationException(L"Freeing an connected connection",__WFILE__,__LINE__);

    ret = ::SQLFreeHandle(SQL_HANDLE_DBC, m_hConnection);
    if (!SQL_SUCCEEDED(ret))
    {
        const StateMessagePair rec = GetDiagRec();

        if (rec.first == L"HY010") // Function sequence error
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"Freeing an connected connection",__WFILE__,__LINE__);
        }
        else if (rec.first == L"HY017")    // Stmt already freed.
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"Freeing an invalid connection",__WFILE__,__LINE__);
        }
        else if (rec.first == L"IM001")    // Driver not implemented this function
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"Driver not implemented this function",__WFILE__,__LINE__);
        }
        else if (rec.first == L"HY013" ||
                 rec.first == L"HY001")
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"Memory management error",__WFILE__,__LINE__);
        }
        else if (rec.first == L"HY000")    // General error
        {
            Safir::Logging::SendSystemLog(Safir::Logging::Warning,
                                          L"Olib: Error in Connection::Free()" + rec.second);

        }
        else if (rec.first == L"HYT01")    // Connection timeout expired.
        {
            Safir::Logging::SendSystemLog(Safir::Logging::Warning,
                                          L"Olib: Error in Connection::Free() " + rec.second);
        }
    }

    // Cant return SQL_SUCCESS_WITH_INFO

    m_hConnection = SQL_NULL_HDBC;
}

void Connection::SetConnectAttr(long lAttribute, long lValue)
{
    SQLRETURN ret;

    if (!IsValid())
        throw Safir::Dob::Typesystem::SoftwareViolationException(L"Using an invalid connection",__WFILE__,__LINE__);

    ret = ::SQLSetConnectAttr(  m_hConnection,
                                lAttribute,
                                reinterpret_cast<SQLPOINTER>(lValue),
                                SQL_IS_UINTEGER );
    if (!SQL_SUCCEEDED(ret))
    {
        ThrowReconnectException(__WFILE__,__LINE__);
    }
    if (ret == SQL_SUCCESS_WITH_INFO) // Can only be 01000 or 01S02
    {
        const StateMessagePair rec = GetDiagRec();
        if (rec.first == L"01000")
        {
            Safir::Logging::SendSystemLog(Safir::Logging::Error,
                                          L"Olib: Non Odbc Error in Connection::SetConnectAttr " + rec.second);
        }
        if (rec.first == L"01S02")
        {
            Safir::Logging::SendSystemLog(Safir::Logging::Warning,
                                          L"Olib: Connection attribute not supported. A similiar attribute used instead");
        }
    }
}

void Connection::SetConnectAttr(long lAttribute, const std::wstring & wszValue)
{
    SQLRETURN ret;

    if (!IsValid())
        throw Safir::Dob::Typesystem::SoftwareViolationException(L"Using an invalid connection",__WFILE__,__LINE__);

    ret = ::SQLSetConnectAttr(  m_hConnection,
                                lAttribute,
                                ToSqlWchars(wszValue),
                                SQL_NTS );
    if (!SQL_SUCCEEDED(ret))
    {
        ThrowReconnectException(__WFILE__,__LINE__);
    }
    if (ret == SQL_SUCCESS_WITH_INFO) // Can only be 01000 or 01S02
    {
        const StateMessagePair rec = GetDiagRec();

        if (rec.first == L"01000")
        {
            Safir::Logging::SendSystemLog(Safir::Logging::Error,
                                          L"Olib: Non Odbc Error in Connection::SetConnectAttr " + rec.second);
        }
        if (rec.first == L"01S02")
        {
            Safir::Logging::SendSystemLog(Safir::Logging::Warning,
                                          L"Olib: Connection attribute not supported. A similiar attribute used instead");
        }
    }
}

void Connection::GetConnectAttr(long lAttribute, long & lValue) const
{
    SQLRETURN ret;

    if (!IsValid())
        throw Safir::Dob::Typesystem::SoftwareViolationException(L"Using an invalid connection",__WFILE__,__LINE__);

    ret = ::SQLGetConnectAttr(  m_hConnection,
                                lAttribute,
                                &lValue,
                                SQL_IS_UINTEGER,
                                NULL);
    if (!SQL_SUCCEEDED(ret))
    {
        ThrowReconnectException(__WFILE__,__LINE__);
    }
    if (ret == SQL_SUCCESS_WITH_INFO)
    {
        const StateMessagePair rec = GetDiagRec();
        if (rec.first == L"01000")
        {
            Safir::Logging::SendSystemLog(Safir::Logging::Error,
                                          L"Olib: Non Odbc Error in Connection::GetConnectAttr " + rec.second);
        }
        if (rec.first == L"01004")
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(rec.second, __WFILE__,__LINE__);
        }
    }
}

void Connection::GetConnectAttr(long lAttribute, wchar_t * wszValue, unsigned long ulLength) const
{
    SQLRETURN ret;

    if (!IsValid())
        throw Safir::Dob::Typesystem::SoftwareViolationException(L"Using an invalid connection",__WFILE__,__LINE__);

    ret = ::SQLGetConnectAttr(  m_hConnection,
                                lAttribute,
                                wszValue,
                                ulLength,
                                NULL );
    if (!SQL_SUCCEEDED(ret))
    {
        ThrowReconnectException(__WFILE__,__LINE__);
    }
    if (ret == SQL_SUCCESS_WITH_INFO)
    {
        const StateMessagePair rec = GetDiagRec();

        if (rec.first == L"01000")
        {
            Safir::Logging::SendSystemLog(Safir::Logging::Error,
                                          L"Olib: Non Odbc Error in Connection::GetConnectAttr " + rec.second);
        }
        if (rec.first == L"01004")
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(rec.second, __WFILE__,__LINE__);
        }
    }
}

void Connection::Connect(const std::wstring & wszConnectionString)
{
    SQLRETURN ret;

    if (!IsValid())
        throw Safir::Dob::Typesystem::SoftwareViolationException(L"Using an invalid connection",__WFILE__,__LINE__);

    if (IsConnected())
        throw Safir::Dob::Typesystem::SoftwareViolationException(L"Already connected",__WFILE__,__LINE__);    

    ret = ::SQLDriverConnectW(  m_hConnection,          // ConnectionHandle
                                NULL,                   // WindowHandle
                                ToSqlWchars(wszConnectionString), // InConnectionString
                                SQL_NTS,                // StringLength1
                                NULL,                   // OutConnectionString
                                0,                      // BufferLength
                                NULL,                   // StringLength2Ptr
                                SQL_DRIVER_NOPROMPT );  // DriverCompletion
    if (!SQL_SUCCEEDED(ret))
    {
        ThrowReconnectException(__WFILE__,__LINE__);
    }
    if (ret == SQL_SUCCESS_WITH_INFO)
    {
        const StateMessagePair rec = GetDiagRec();

        if (rec.first == L"01004" ||
            rec.first == L"01S00" ||
            rec.first == L"01S09")
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(rec.second, __WFILE__,__LINE__);
        }
        else if (rec.first == L"01000")
        {
            Safir::Logging::SendSystemLog(Safir::Logging::Error,
                                          L"Olib: Non Odbc Error in Connection::Connect " + rec.second);
        }
        else if (rec.first == L"01S02")
        {
            Safir::Logging::SendSystemLog(Safir::Logging::Warning,
                                          L"Olib: Connection attribute not supported. A similiar attribute used instead");
        }
        else if (rec.first == L"01S08")
        {
            Safir::Logging::SendSystemLog(Safir::Logging::Warning,
                                          L"Olib: Error saving file dsn.");
        }
    }
    m_bIsConnected = true;
}

void Connection::Connect(char * cszConnectionString)
{
    SQLRETURN ret;

    if (!IsValid())
        throw Safir::Dob::Typesystem::SoftwareViolationException(L"Using an invalid connection",__WFILE__,__LINE__);

    if (IsConnected())
        throw Safir::Dob::Typesystem::SoftwareViolationException(L"Already connected",__WFILE__,__LINE__);

    ret = ::SQLDriverConnectA(  m_hConnection,          // ConnectionHandle
                                NULL,                   // WindowHandle
                                reinterpret_cast<SQLCHAR *>(cszConnectionString),    // InConnectionString
                                SQL_NTS,                // StringLength1
                                NULL,                   // OutConnectionString
                                0,                      // BufferLength
                                NULL,                   // StringLength2Ptr
                                SQL_DRIVER_NOPROMPT );  // DriverCompletion
    if (!SQL_SUCCEEDED(ret))
    {
        ThrowReconnectException(__WFILE__,__LINE__);
    }
    if (ret == SQL_SUCCESS_WITH_INFO)
    {
        const StateMessagePair rec = GetDiagRec();

        if (rec.first == L"01004" ||
            rec.first == L"01S00" ||
            rec.first == L"01S09")
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(rec.second, __WFILE__,__LINE__);
        }
        else if (rec.first == L"01000")
        {
            Safir::Logging::SendSystemLog(Safir::Logging::Error,
                                          L"Olib: Non Odbc Error in Connection::Connect: " + rec.second);
        }
        else if (rec.first == L"01S02")
        {
            Safir::Logging::SendSystemLog(Safir::Logging::Warning,
                                          L"Olib: Connection attribute not supported. A similiar attribute used instead");
        }
        else if (rec.first == L"01S08")
        {
            Safir::Logging::SendSystemLog(Safir::Logging::Warning,
                                          L"Olib: Error saving file dsn.");
        }
    }
    m_bIsConnected = true;
}

void Connection::AddStatement(Statement * pStatement)
{
    m_statements.push_back( pStatement );
}

void Connection::RemoveStatement(Statement * pStatement)
{
    std::vector<Statement *>::iterator it;
    it = std::remove( m_statements.begin(), m_statements.end(), pStatement);
    m_statements.erase(it, m_statements.end());
}

void Connection::Disconnect()
{
    Statement * pStatement;
    SQLRETURN ret;

    if (!IsValid())
        throw Safir::Dob::Typesystem::SoftwareViolationException(L"Using an invalid connection",__WFILE__,__LINE__);

    if (!IsConnected())
        return;

    ret = ::SQLDisconnect( m_hConnection );
    if (!SQL_SUCCEEDED(ret))
    {
        const StateMessagePair rec = GetDiagRec();

        if (rec.first == L"HY010") // Function sequence error
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"Query in progress",__WFILE__,__LINE__);
        }
        else if (rec.first == L"08003")    // Connection does not exist.
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"Using an invalid connection",__WFILE__,__LINE__);
        }
        else if (rec.first == L"25000")    // Transaction in progress
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"Transaction in progress",__WFILE__,__LINE__);
        }
        else if (rec.first == L"HY013" ||
                 rec.first == L"HY001")
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"Memory management error",__WFILE__,__LINE__);
        }
        else if (rec.first == L"HY000")   // General error
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(rec.second,__WFILE__,__LINE__);
        }
        else if (rec.first == L"IM001")    // Driver not implemented function
        {
            Safir::Logging::SendSystemLog(Safir::Logging::Warning,
                                          L"Olib: Driver not implemented function");
        }
        else if (rec.first == L"HYT01")      // Connection timeout
        {
            throw Safir::Databases::Odbc::TimeoutException(rec.second, __WFILE__,__LINE__);
        }
    }
    else if (ret == SQL_SUCCESS_WITH_INFO) // Can only be 01000 or 01S02
    {
        const StateMessagePair rec = GetDiagRec();

        if (rec.first == L"01000")
        {
            Safir::Logging::SendSystemLog(Safir::Logging::Error,
                                          L"Olib: Non Odbc Error in Connection::Disconnect " + rec.second);
        }
        if (rec.first == L"01S02")
        {
            Safir::Logging::SendSystemLog(Safir::Logging::Warning,
                                          L"Olib: An error occurred during the disconnect. However, the disconnect succeeded..");
        }
    }

    while (!m_statements.empty())
    {
        pStatement = m_statements.back();
        pStatement->SetNotValid();
        m_statements.pop_back();
    }
    m_bIsConnected = false;
}

bool Connection::GetDiagRec(short sRecNumber,
                            std::wstring & SqlState,
                            boost::int32_t & NativeError,
                            std::wstring & MessageText,
                            bool & bDataRead) const
{
    SQLWCHAR wszSqlState[6];
    SQLWCHAR wszMessageText[SQL_MAX_MESSAGE_LENGTH];

    SQLINTEGER tmpNativeError;
    SQLRETURN ret;
    ret = ::SQLGetDiagRecW( SQL_HANDLE_DBC,
                            m_hConnection,
                            sRecNumber,
                            wszSqlState,
                            &tmpNativeError,
                            wszMessageText,
                            SQL_MAX_MESSAGE_LENGTH,
                            NULL);
    NativeError = static_cast<boost::int32_t>(tmpNativeError);
    bDataRead = ((ret == SQL_SUCCESS) ||(ret == SQL_SUCCESS_WITH_INFO));
    SqlState = ToWstring(wszSqlState);
    MessageText = ToWstring(wszMessageText);
    return ((ret != SQL_NO_DATA) && (ret != SQL_INVALID_HANDLE) && (ret != SQL_ERROR));
}

void Connection::EndTran(short sCompletionType)
{
    SQLRETURN ret;

    if (!IsValid())
        throw Safir::Dob::Typesystem::SoftwareViolationException(L"Using an invalid connection",__WFILE__,__LINE__);

    if (!IsConnected())
        throw Safir::Dob::Typesystem::SoftwareViolationException(L"Using an unconnected connection",__WFILE__,__LINE__);

    ret = ::SQLEndTran(SQL_HANDLE_DBC, m_hConnection, sCompletionType);
    if (!SQL_SUCCEEDED(ret))
    {
        ThrowReconnectException(__WFILE__,__LINE__);
    }
    if (ret == SQL_SUCCESS_WITH_INFO) // Can only be 01000 - non odbc error.
    {
        const StateMessagePair rec = GetDiagRec();

        Safir::Logging::SendSystemLog(Safir::Logging::Error,
                                      L"Olib: Non Odbc Error in Connection::EndTran " + rec.second);
    }
}

void Connection::ThrowReconnectException(const std::wstring & fileName,
                                         const Safir::Dob::Typesystem::Int64 lineNumber) const
{
    ThrowReconnectException(SQL_HANDLE_DBC, m_hConnection,fileName,lineNumber);
}

void Connection::ThrowReconnectException(SQLSMALLINT handleType,
                                         SQLHANDLE handle,
                                         const std::wstring & fileName,
                                         const Safir::Dob::Typesystem::Int64 lineNumber) const
{
    const StateMessagePair rec = Safir::Databases::Odbc::GetDiagRec(handleType, handle);

    throw ReconnectException(rec.first + L":" + rec.second, fileName,lineNumber);
}

} // End namespace Odbc

} // End namespace Databases

} // End namespace Safir
