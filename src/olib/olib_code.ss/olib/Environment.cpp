/******************************************************************************
*
* Copyright Saab AB, 2005-2008 (http://www.safirsdk.com)
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
#include "Safir/Databases/Odbc/Environment.h"

#include <Safir/Databases/Odbc/ReconnectException.h>
#include <Safir/Logging/Log.h>
#include "StringConversion.h"
#include "Diagnostics.h"

namespace Safir
{

namespace Databases
{

namespace Odbc
{

Environment::Environment(void) : m_hEnv(SQL_NULL_HENV), bSetOdbcVersion( false )
{
}

Environment::~Environment(void)
{
}

void Environment::Alloc()
{
    SQLRETURN ret = SQL_SUCCESS;

    if (IsValid() && bSetOdbcVersion)
        return; // Environment already allocated

    if (!IsValid())
    {
        ret = ::SQLAllocHandle(SQL_HANDLE_ENV, SQL_NULL_HANDLE, &m_hEnv);
        if (!SQL_SUCCEEDED(ret)) 
        {
            const StateMessagePair rec = Safir::Databases::Odbc::GetDiagRec(SQL_HANDLE_ENV,
                                                                            SQL_NULL_HANDLE);

            if (rec.first == L"HY000")     // General error
            {
                ThrowReconnectException(SQL_NULL_HANDLE,__WFILE__,__LINE__);
            }
            else if (rec.first == L"HY013" ||    
                     rec.first == L"HY001")
            {
                throw Safir::Dob::Typesystem::SoftwareViolationException(L"Memory management error",__WFILE__,__LINE__);
            }
            else if (rec.first == L"HY009")    
            {
                throw Safir::Dob::Typesystem::SoftwareViolationException(L"Invalid use of null pointer",__WFILE__,__LINE__);
            }
            else if (rec.first == L"HY010")    
            {
                throw Safir::Dob::Typesystem::SoftwareViolationException(L"Function sequence error",__WFILE__,__LINE__);
            }
            else if (rec.first == L"HY014")    // Limit on the number of handles exceeded
            {
                throw Safir::Dob::Typesystem::SoftwareViolationException(L"Limit on the number of handles exceeded",__WFILE__,__LINE__);
            }
            else if (rec.first == L"HY092")    
            {
                throw Safir::Dob::Typesystem::SoftwareViolationException(L"Invalid attribute/option identifier",__WFILE__,__LINE__);
            }
            else if (rec.first == L"HYC00" ||    // Optional feature not implemented
                     rec.first == L"IM001")      // Driver does not support this function
            {
                throw Safir::Dob::Typesystem::SoftwareViolationException(L"Feature not implemented",__WFILE__,__LINE__);
            }
            else if (rec.first == L"HYT01")    // Connection timeout expired
            {
                ThrowReconnectException(SQL_NULL_HANDLE,__WFILE__,__LINE__);
            }
        }
        if (ret == SQL_SUCCESS_WITH_INFO) // Can only be 01000 - non odbc error.
        {
            const StateMessagePair rec = Safir::Databases::Odbc::GetDiagRec(SQL_HANDLE_ENV,
                                                                            m_hEnv);
            Safir::Logging::SendSystemLog(Safir::Logging::Error,
                                          L"Olib: Non Odbc Error in Environment::Alloc " + rec.second);
        }
    }

    if (!bSetOdbcVersion)
    {
        SetEnvAttr(SQL_ATTR_ODBC_VERSION, SQL_OV_ODBC3);    //SQL_OV_ODBC2
        //SetEnvAttr(SQL_ATTR_ODBC_VERSION, SQL_OV_ODBC2);    //SQL_OV_ODBC3
        bSetOdbcVersion = true;
    }
}

void Environment::Free()
{
    SQLRETURN ret;

    if (!IsValid())
        throw Safir::Dob::Typesystem::SoftwareViolationException(L"Freeing an invalid environment",__WFILE__,__LINE__);

    ret = ::SQLFreeHandle(SQL_HANDLE_ENV, m_hEnv);
    if (!SQL_SUCCEEDED(ret)) 
    {
        const StateMessagePair rec = Safir::Databases::Odbc::GetDiagRec(SQL_HANDLE_ENV,
                                                                        m_hEnv);

        if (rec.first == L"HY010") // Function sequence error
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"Cannot free environment due to existing connections",__WFILE__,__LINE__);
        }
        else if (rec.first == L"HY017")    // Stmt already freed.
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"Freeing an invalid environment",__WFILE__,__LINE__);
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
                                          L"Olib: Error in Environment::Free()" + rec.second);
        }
        else if (rec.first == L"HYT01")    // Connection timeout expired.
        {
            Safir::Logging::SendSystemLog(Safir::Logging::Warning,
                                          L"Olib: Error in Environment::Free()" + rec.second);
        }
    }
    // Cant return SQL_SUCCESS_WITH_INFO

    m_hEnv = SQL_NULL_HENV;
    bSetOdbcVersion = false;
}

void Environment::SetEnvAttr(long lAttribute, long lValue)
{
    SQLRETURN ret;

    ret = ::SQLSetEnvAttr(  m_hEnv, 
                            lAttribute, 
                            reinterpret_cast<SQLPOINTER>(lValue),
                            SQL_IS_UINTEGER );
    if (!SQL_SUCCEEDED(ret)) 
    {
        const StateMessagePair rec = Safir::Databases::Odbc::GetDiagRec(SQL_HANDLE_ENV,
                                                                        m_hEnv);

        if (rec.first == L"HY000") // General error
        {
            ThrowReconnectException(__WFILE__,__LINE__);
        }
        else if (rec.first == L"HY009")    // Invalid use of null pointer
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"Invalid use of null pointer",__WFILE__,__LINE__);
        }
        else if (rec.first == L"HY010")    // Function sequence error
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"Invalid handle",__WFILE__,__LINE__);
        }
        else if (rec.first == L"HY024" ||    // Illegal value
                 rec.first == L"HY001")      // Optional value not supported.    
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"Illegal value",__WFILE__,__LINE__);
        }
        else if (rec.first == L"HY090")    // Invalid string or buffer length
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"Invalid string or buffer length",__WFILE__,__LINE__);
        }
        else if (rec.first == L"HYC00")    // Driver not implemented this function
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"Driver not implemented this function",__WFILE__,__LINE__);
        }
        else if (rec.first == L"HY013" ||    
                 rec.first == L"HY001")
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"Memory management error",__WFILE__,__LINE__);
        }
    }
    if (ret == SQL_SUCCESS_WITH_INFO) // Can only be 01000 or 01S02
    {
        const StateMessagePair rec = Safir::Databases::Odbc::GetDiagRec(SQL_HANDLE_ENV,
                                                                        m_hEnv);
        if (rec.first == L"01000")
        {
            const StateMessagePair rec = Safir::Databases::Odbc::GetDiagRec(SQL_HANDLE_ENV,
                                                                            m_hEnv);
            Safir::Logging::SendSystemLog(Safir::Logging::Error,
                                          L"Olib: Non Odbc Error in Environment::SetEnvAttr" + rec.second);
        }
        if (rec.first == L"01S02")
        {
            Safir::Logging::SendSystemLog(Safir::Logging::Warning,
                                          L"Olib: Environment attribute not supported. A similiar attribute used instead");
        }
    }
}

void Environment::SetEnvAttr(long lAttribute, const std::wstring & wszValue, unsigned long ulLength)
{
    SQLRETURN ret;

    ret = ::SQLSetEnvAttr(  m_hEnv, 
                            lAttribute, 
                            ToSqlWchars(wszValue),
                            ulLength );
    if (!SQL_SUCCEEDED(ret)) 
    {
        const StateMessagePair rec = Safir::Databases::Odbc::GetDiagRec(SQL_HANDLE_ENV,
                                                                        m_hEnv);

        if (rec.first == L"HY000") // General error
        {
            ThrowReconnectException(__WFILE__,__LINE__);
        }
        else if (rec.first == L"HY009")    // Invalid use of null pointer
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"Invalid use of null pointer",__WFILE__,__LINE__);
        }
        else if (rec.first == L"HY010")    // Function sequence error
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"Invalid handle",__WFILE__,__LINE__);
        }
        else if (rec.first == L"HY024" ||    // Illegal value
                 rec.first == L"HY001")      // Optional value not supported.    
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"Illegal value",__WFILE__,__LINE__);
        }
        else if (rec.first == L"HY090")    // Invalid string or buffer length
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"Invalid string or buffer length",__WFILE__,__LINE__);
        }
        else if (rec.first == L"HYC00")    // Driver not implemented this function
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"Driver not implemented this function",__WFILE__,__LINE__);
        }
        else if (rec.first == L"HY013" ||    
                 rec.first == L"HY001")
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"Memory management error",__WFILE__,__LINE__);
        }
    }
    if (ret == SQL_SUCCESS_WITH_INFO) // Can only be 01000 or 01S02
    {
        const StateMessagePair rec = Safir::Databases::Odbc::GetDiagRec(SQL_HANDLE_ENV,
                                                                        m_hEnv);

        if (rec.first == L"01000")
        {
            const StateMessagePair rec = Safir::Databases::Odbc::GetDiagRec(SQL_HANDLE_ENV,
                                                                            m_hEnv);
            Safir::Logging::SendSystemLog(Safir::Logging::Error,
                                          L"Olib: Non Odbc Error in Environment::SetEnvAttr" + rec.second);
        }
        if (rec.first == L"01S02")
        {
            Safir::Logging::SendSystemLog(Safir::Logging::Warning,
                                          L"Olib: Environment attribute not supported. A similiar attribute used instead");
        }
    }
}

void Environment::GetEnvAttr(long lAttribute, long & lValue) const
{
    SQLRETURN ret;

    ret = ::SQLGetEnvAttr(  m_hEnv, 
                            lAttribute, 
                            &lValue,
                            SQL_IS_UINTEGER,
                            NULL);
    if (!SQL_SUCCEEDED(ret)) 
    {
        const StateMessagePair rec = Safir::Databases::Odbc::GetDiagRec(SQL_HANDLE_ENV,
                                                                        m_hEnv);
        if (rec.first == L"HY000") // General error
        {
            ThrowReconnectException(__WFILE__,__LINE__);
        }
        else if (rec.first == L"HY013" ||    
                 rec.first == L"HY001")
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"Memory management error",__WFILE__,__LINE__);
        }
        else if (rec.first == L"HYC00")
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"Driver not implemented this function",__WFILE__,__LINE__);
        }
        else if (rec.first == L"HY092")
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"Optional feature not implemented",__WFILE__,__LINE__);
        }
        else if (rec.first == L"IM001")    
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"Driver does not support this function",__WFILE__,__LINE__);
        }
    }
    if (ret == SQL_SUCCESS_WITH_INFO) 
    {
        const StateMessagePair rec = Safir::Databases::Odbc::GetDiagRec(SQL_HANDLE_ENV,
                                                                        m_hEnv);
        if (rec.first == L"01000")
        {
            const StateMessagePair rec = Safir::Databases::Odbc::GetDiagRec(SQL_HANDLE_ENV,
                                                                            m_hEnv);
            Safir::Logging::SendSystemLog(Safir::Logging::Error,
                                          L"Olib: Non Odbc Error in Environment::GetEnvAttr" + rec.second);
        }
        if (rec.first == L"01004")
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(rec.second, __WFILE__,__LINE__);
        }
    }
}

void Environment::GetEnvAttr(long lAttribute, wchar_t * wszValue, unsigned long ulLength) const
{
    SQLRETURN ret;

    ret = ::SQLGetEnvAttr(  m_hEnv, 
                            lAttribute, 
                            wszValue,
                            ulLength,
                            NULL );
    if (!SQL_SUCCEEDED(ret)) 
    {
        const StateMessagePair rec = Safir::Databases::Odbc::GetDiagRec(SQL_HANDLE_ENV,
                                                                        m_hEnv);

        if (rec.first == L"HY000") // General error
        {
            ThrowReconnectException(__WFILE__,__LINE__);
        }
        else if (rec.first == L"HY013" ||    
                 rec.first == L"HY001")
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"Memory management error",__WFILE__,__LINE__);
        }
        else if (rec.first == L"HYC00")
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"Driver not implemented this function",__WFILE__,__LINE__);
        }
        else if (rec.first == L"HY092")
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"Optional feature not implemented",__WFILE__,__LINE__);
        }
        else if (rec.first == L"IM001")    
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"Driver does not support this function",__WFILE__,__LINE__);
        }
    }
    if (ret == SQL_SUCCESS_WITH_INFO) 
    {
        const StateMessagePair rec = Safir::Databases::Odbc::GetDiagRec(SQL_HANDLE_ENV,
                                                                        m_hEnv);
        if (rec.first == L"01000")
        {
            const StateMessagePair rec = Safir::Databases::Odbc::GetDiagRec(SQL_HANDLE_ENV,
                                                                            m_hEnv);
            Safir::Logging::SendSystemLog(Safir::Logging::Error,
                                          L"Olib: Non Odbc Error in Environment::GetEnvAttr" + rec.second);
        }
        if (rec.first == L"01004")
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(rec.second, __WFILE__,__LINE__);
        }
    }
}

bool Environment::GetDiagRec(short sRecNumber,
                           std::wstring & SqlState,
                           boost::int32_t & NativeError,
                           std::wstring & MessageText,
                           bool & bDataRead) const
{
    SQLWCHAR wszSqlState[6];
    SQLWCHAR wszMessageText[SQL_MAX_MESSAGE_LENGTH];

    SQLRETURN ret;
    SQLINTEGER tmpNativeError;
    ret = ::SQLGetDiagRecW( SQL_HANDLE_ENV,
                            m_hEnv,
                            sRecNumber,
                            wszSqlState,
                            &tmpNativeError,
                            wszMessageText,
                            SQL_MAX_MESSAGE_LENGTH,
                            0 );
    NativeError = static_cast<boost::int32_t>(tmpNativeError);
    bDataRead = ((ret == SQL_SUCCESS) ||(ret == SQL_SUCCESS_WITH_INFO));
    SqlState = ToWstring(wszSqlState);
    MessageText = ToWstring(wszMessageText);
    return ((ret != SQL_NO_DATA) && (ret != SQL_INVALID_HANDLE) && (ret != SQL_ERROR));
}

void Environment::ThrowReconnectException(  const std::wstring & fileName,   
                                            const Safir::Dob::Typesystem::Int64 lineNumber) const
{
    ThrowReconnectException( m_hEnv, fileName, lineNumber );
}

void Environment::ThrowReconnectException(SQLHENV hEnv,
                                          const std::wstring & fileName,   
                                          const Safir::Dob::Typesystem::Int64 lineNumber) const
{
    const StateMessagePair rec = Safir::Databases::Odbc::GetDiagRec(SQL_HANDLE_ENV,
                                                                    hEnv);

    throw ReconnectException(rec.first + L":" + rec.second, fileName, lineNumber);
}

} // End namespace Odbc

} // End namespace Databases

} // End namespace Safir
