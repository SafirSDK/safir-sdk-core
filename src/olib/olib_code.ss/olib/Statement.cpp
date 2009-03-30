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
#include <Safir/Databases/Odbc/Statement.h>
#include <Safir/Databases/Odbc/Connection.h>
#include <Safir/Databases/Odbc/IntegrityConstraintException.h>
#include <Safir/Databases/Odbc/RetryException.h>
#include <Safir/Databases/Odbc/TimeoutException.h>
#include <Safir/Databases/Odbc/ReconnectException.h>
#include <Safir/SwReports/SwReport.h>

namespace Safir
{

namespace Databases
{

namespace Odbc
{

Statement::Statement() : m_hStatement(SQL_NULL_HSTMT), m_pConnection( NULL )
{
}

Statement::~Statement()
{
    if (IsValid() && (m_pConnection != NULL))
        m_pConnection->RemoveStatement(this);
}

void Statement::Alloc(Connection & pConnection)
{
    SQLRETURN ret;

    // Allocating a valid statement
    if (IsValid())
        return;

    // SQL_HANDLE_STMT refers to the handle we are allocating and should not be mixed up
    // with the handle type sent into ThrowException.
    ret = ::SQLAllocHandle(SQL_HANDLE_STMT, pConnection.Handle(), &m_hStatement);
    if (!SQL_SUCCEEDED(ret))
    {
        ThrowException(SQL_HANDLE_DBC, pConnection.Handle(), __WFILE__,__LINE__, ret);
    }
    if (ret == SQL_SUCCESS_WITH_INFO)
    {
        ThrowException(__WFILE__,__LINE__, ret);
    }
    m_pConnection = &pConnection;
    m_pConnection->AddStatement( this );
}

void Statement::Free()
{
    SQLRETURN ret;

    if (!IsValid())
        throw Safir::Dob::Typesystem::SoftwareViolationException(L"Freeing an invalid statement",__WFILE__,__LINE__);

    ret = ::SQLFreeHandle(SQL_HANDLE_STMT, m_hStatement);
    if (!SQL_SUCCEEDED(ret))
    {
        ThrowException(__WFILE__,__LINE__, ret);
    }
    m_hStatement = SQL_NULL_HSTMT;
    m_pConnection->RemoveStatement(this);
}

void Statement::CloseCursor()
{
    SQLRETURN ret;

    if (!IsValid())
        throw Safir::Dob::Typesystem::SoftwareViolationException(L"Using an invalid statement",__WFILE__,__LINE__);

    ret = ::SQLCloseCursor(m_hStatement);
    if (!SQL_SUCCEEDED(ret))
    {
        ThrowException(__WFILE__,__LINE__, ret);
    }
}

void Statement::SetStmtAttr(long lAttribute, long lValue)
{
    SQLRETURN ret;

    if (!IsValid())
        throw Safir::Dob::Typesystem::SoftwareViolationException(L"Using an invalid statement",__WFILE__,__LINE__);

    ret = ::SQLSetStmtAttr( m_hStatement,
                            lAttribute,
                            reinterpret_cast<SQLPOINTER>(lValue),
                            SQL_IS_UINTEGER );
    if (!SQL_SUCCEEDED(ret))
    {
        ThrowException(__WFILE__,__LINE__, ret);
    }
}

void Statement::SetStmtAttr(long lAttribute, const std::wstring & wszValue)
{
    SQLRETURN ret;

    if (!IsValid())
        throw Safir::Dob::Typesystem::SoftwareViolationException(L"Using an invalid statement",__WFILE__,__LINE__);

    // const_cast is used because Value is declared as input in the ODBC
    // specification and should be a const wchar_t *.
    ret = ::SQLSetStmtAttrW( m_hStatement,
                             lAttribute,
                             const_cast<wchar_t *>(wszValue.c_str()),
                             SQL_NTS );
    if (!SQL_SUCCEEDED(ret))
    {
        ThrowException(__WFILE__,__LINE__, ret);
    }
}

void Statement::GetStmtAttr(long lAttribute, long & lValue) const
{
    SQLRETURN ret;

    if (!IsValid())
        throw Safir::Dob::Typesystem::SoftwareViolationException(L"Using an invalid statement",__WFILE__,__LINE__);

    ret = ::SQLGetStmtAttr( m_hStatement,
                            lAttribute,
                            &lValue,
                            SQL_IS_UINTEGER,
                            NULL);
    if (!SQL_SUCCEEDED(ret))
    {
        ThrowException(__WFILE__,__LINE__, ret);
    }
}

void Statement::GetStmtAttr(long lAttribute, wchar_t * wszValue, unsigned long ulLength) const
{
    SQLRETURN ret;

    if (!IsValid())
        throw Safir::Dob::Typesystem::SoftwareViolationException(L"Using an invalid statement",__WFILE__,__LINE__);

    ret = ::SQLGetStmtAttrW(m_hStatement,
                            lAttribute,
                            wszValue,
                            ulLength,
                            NULL );
    if (!SQL_SUCCEEDED(ret))
    {
        ThrowException(__WFILE__,__LINE__, ret);
    }
}

void Statement::Execute()
{
    SQLRETURN ret;

    if (!IsValid())
        throw Safir::Dob::Typesystem::SoftwareViolationException(L"Using an invalid statement",__WFILE__,__LINE__);

    ret = ::SQLExecute( m_hStatement );
    if (ret == SQL_NEED_DATA)
    {
        ;
    }
    else if (!SQL_SUCCEEDED(ret))
    {
        ThrowException(__WFILE__,__LINE__, ret);
    }
}

void Statement::Prepare(const std::wstring & wszSqlCommand)
{
    SQLRETURN ret;

    if (!IsValid())
        throw Safir::Dob::Typesystem::SoftwareViolationException(L"Using an invalid statement",__WFILE__,__LINE__);

    // const_cast is used because StatementText is declared as input in the ODBC
    // specification and should be a const wchar_t *.
    ret = ::SQLPrepareW(m_hStatement,
                        const_cast<wchar_t *>(wszSqlCommand.c_str()),
                        SQL_NTS );
    if (!SQL_SUCCEEDED(ret))
    {
        ThrowException(__WFILE__,__LINE__, ret);
    }
}

void Statement::ExecDirect(const std::wstring & wszSqlCommand)
{
    SQLRETURN ret;

    if (!IsValid())
        throw Safir::Dob::Typesystem::SoftwareViolationException(L"Using an invalid statement",__WFILE__,__LINE__);

    // const_cast is used because StatementText is declared as input in the ODBC
    // specification and should be a const wchar_t *.
    ret = ::SQLExecDirectW( m_hStatement,
                            const_cast<wchar_t *>(wszSqlCommand.c_str()),
                            SQL_NTS );
    if (!SQL_SUCCEEDED(ret))
    {
        ThrowException(__WFILE__,__LINE__, ret);
    }
}

bool Statement::Fetch()
{
    SQLRETURN ret;
    bool bDataFound = true;

    if (!IsValid())
        throw Safir::Dob::Typesystem::SoftwareViolationException(L"Using an invalid statement",__WFILE__,__LINE__);

    ret = ::SQLFetch( m_hStatement );
    if (ret==SQL_NO_DATA_FOUND)
    {
        bDataFound = false;
    }
    else if (!SQL_SUCCEEDED(ret))
    {
        ThrowException(__WFILE__,__LINE__, ret);
    }
    return bDataFound;
}

bool Statement::MoreResults()
{
    SQLRETURN ret;
    bool bDataFound = true;

    if (!IsValid())
        throw Safir::Dob::Typesystem::SoftwareViolationException(L"Using an invalid statement",__WFILE__,__LINE__);

    ret = ::SQLMoreResults( m_hStatement );
    if (ret==SQL_NO_DATA_FOUND)
    {
        bDataFound = false;
    }
    else if (!SQL_SUCCEEDED(ret))
    {
        ThrowException(__WFILE__,__LINE__, ret);
    }
    return bDataFound;
}

int Statement::GetNumberOfColumns() const
{
    SQLRETURN ret;
    SQLSMALLINT nColumns = 0;

    if (!IsValid())
        throw Safir::Dob::Typesystem::SoftwareViolationException(L"Using an invalid statement",__WFILE__,__LINE__);

    ret = ::SQLNumResultCols( m_hStatement, &nColumns );
    if (!SQL_SUCCEEDED(ret))
    {
        ThrowException(__WFILE__,__LINE__, ret);
    }
    return nColumns;
}

bool Statement::ParamData(unsigned short & lParameter) const
{
    bool bNeedData = false;
    SQLRETURN ret;
    SQLPOINTER pBuffer;

    if (!IsValid())
        throw Safir::Dob::Typesystem::SoftwareViolationException(L"Using an invalid statement",__WFILE__,__LINE__);

    ret = ::SQLParamData( m_hStatement, &pBuffer );
    if (ret==SQL_NEED_DATA)
    {
        bNeedData = true;
    }
    else if (!SQL_SUCCEEDED(ret))
    {
        ThrowException(__WFILE__,__LINE__, ret);
    }

    long temp = (long) pBuffer;
    lParameter = (unsigned short) (temp & 0xffff);
    //lParameter = (unsigned short)(int)pBuffer;
    //lParameter = reinterpret_cast<unsigned short>(pBuffer);

    return bNeedData;
}

bool Statement::GetDiagRec(short sRecNumber,
                           std::wstring & SqlState,
                           boost::int32_t & NativeError,
                           std::wstring & MessageText,
                           bool & bDataRead) const
{
    wchar_t wszSqlState[6];
    wchar_t wszMessageText[SQL_MAX_MESSAGE_LENGTH];

    SQLINTEGER tmpNativeError;
    SQLRETURN ret;
    ret = ::SQLGetDiagRecW( SQL_HANDLE_STMT,
                            m_hStatement,
                            sRecNumber,
                            wszSqlState,
                            &tmpNativeError,
                            wszMessageText,
                            SQL_MAX_MESSAGE_LENGTH,
                            0 );
    NativeError = static_cast<boost::int32_t>(tmpNativeError);
    bDataRead = ((ret == SQL_SUCCESS) ||(ret == SQL_SUCCESS_WITH_INFO));
    SqlState = wszSqlState;
    MessageText = wszMessageText;
    return ((ret != SQL_NO_DATA) && (ret != SQL_INVALID_HANDLE) && (ret != SQL_ERROR));
}

void Statement::ThrowException(const std::wstring & fileName,
                               const Safir::Dob::Typesystem::Int64 lineNumber,
                               SQLRETURN original_returncode) const
{
    ThrowException(SQL_HANDLE_STMT, m_hStatement, fileName, lineNumber,original_returncode);
}

void Statement::ThrowException(SQLSMALLINT HandleType,
                               SQLHANDLE Handle,
                               const std::wstring & fileName,
                               const Safir::Dob::Typesystem::Int64 lineNumber,
                               SQLRETURN original_returncode) const
{
    wchar_t wszSqlState[6];
    SQLINTEGER lpNativeErrorPtr;
    wchar_t wszMessageText[512];
    SQLRETURN ret;

    ret = ::SQLGetDiagRecW( HandleType,
                            Handle,
                            1,
                            wszSqlState,
                            &lpNativeErrorPtr,
                            wszMessageText,
                            256,
                            0 );
    if (wcscmp(wszSqlState, L"01S07") == 0) // Fractional truncation
    {
        if (original_returncode == SQL_ERROR)   // Only report if its an SQL_ERROR
        {
            std::wstring string = wszSqlState;
            string += L":";
            string += wszMessageText;
            throw Safir::Dob::Typesystem::SoftwareViolationException(string.c_str(), fileName,lineNumber);
        }
    }
    else if (wcscmp(wszSqlState, L"23000") == 0)        // Integrity constraint violation
    {
        std::wstring string = wszSqlState;
        string += L":";
        string += wszMessageText;
        throw IntegrityConstraintException(string.c_str(),fileName,lineNumber);
    }
    else if ((wcscmp(wszSqlState, L"07002") == 0) ||
             (wcscmp(wszSqlState, L"07006") == 0) ||
             (wcscmp(wszSqlState, L"07009") == 0) ||
             (wcscmp(wszSqlState, L"07S01") == 0) ||
             (wcscmp(wszSqlState, L"21S01") == 0) ||    // Insert value list does not match column list
             (wcscmp(wszSqlState, L"21S02") == 0) ||
             (wcscmp(wszSqlState, L"22001") == 0) ||    // truncation of non null string or binary data.
             (wcscmp(wszSqlState, L"22002") == 0) ||
             (wcscmp(wszSqlState, L"22007") == 0) ||
             (wcscmp(wszSqlState, L"22008") == 0) ||
             (wcscmp(wszSqlState, L"22012") == 0) ||
             (wcscmp(wszSqlState, L"22018") == 0) ||
             (wcscmp(wszSqlState, L"22019") == 0) ||
             (wcscmp(wszSqlState, L"22025") == 0) ||
             (wcscmp(wszSqlState, L"24000") == 0) ||
             (wcscmp(wszSqlState, L"34000") == 0) ||
             (wcscmp(wszSqlState, L"3D000") == 0) ||
             (wcscmp(wszSqlState, L"3F000") == 0) ||
             (wcscmp(wszSqlState, L"42000") == 0) ||
             (wcscmp(wszSqlState, L"42S01") == 0) ||
             (wcscmp(wszSqlState, L"42S02") == 0) ||
             (wcscmp(wszSqlState, L"42S22") == 0) ||
             (wcscmp(wszSqlState, L"HY003") == 0) ||
             (wcscmp(wszSqlState, L"HY009") == 0) ||
             (wcscmp(wszSqlState, L"HY010") == 0) ||    // Function sequence error
             (wcscmp(wszSqlState, L"HY104") == 0) ||
             (wcscmp(wszSqlState, L"HY105") == 0) ||
             (wcscmp(wszSqlState, L"HYC00") == 0) ||
             (wcscmp(wszSqlState, L"IM001") == 0) ||
             (wcscmp(wszSqlState, L"01001") == 0) ||
             (wcscmp(wszSqlState, L"01006") == 0) ||
             (wcscmp(wszSqlState, L"01007") == 0) ||
             (wcscmp(wszSqlState, L"01S00") == 0) ||
             (wcscmp(wszSqlState, L"01S06") == 0) ||    // Used in ExtendedFetch or FetchScroll.
             (wcscmp(wszSqlState, L"01S09") == 0))
    {
        std::wstring string = wszSqlState;
        string += L":";
        string += wszMessageText;
        throw Safir::Dob::Typesystem::SoftwareViolationException(string.c_str(), fileName,lineNumber);
    }
    else if ((wcscmp(wszSqlState, L"HYT00") == 0) ||    // Statement timeout
             (wcscmp(wszSqlState, L"HYT01") == 0))      // Connection timeout
    {
        std::wstring string = wszSqlState;
        string += L":";
        string += wszMessageText;
        throw TimeoutException(string.c_str(),fileName,lineNumber);
    }
    else if ((wcscmp(wszSqlState, L"08S01") == 0) ||    // Communication link failure
             (wcscmp(wszSqlState, L"HY000") == 0))      // General error
    {
        std::wstring string = wszSqlState;
        string += L":";
        string += wszMessageText;
        throw ReconnectException(string.c_str(),fileName,lineNumber);
    }
    else if ((wcscmp(wszSqlState, L"40001") == 0) ||    // Serialization failure. Transaction rollback.
             (wcscmp(wszSqlState, L"HY001") == 0) ||    // Memory allocation error
             (wcscmp(wszSqlState, L"HY013") == 0))      // Memory management error
    {
        std::wstring string = wszSqlState;
        string += L":";
        string += wszMessageText;
        throw RetryException(string.c_str(),fileName,lineNumber);
    }
    else if (wcscmp(wszSqlState, L"01000") == 0)
    {
        // This is a warning from the rdbms driver and its not specified as an error.
        Safir::SwReports::SendProgramInfoReport(std::wstring(L"Non Odbc Error caught:: ") +
                                                std::wstring(wszMessageText) );
    }
    else if (wcscmp(wszSqlState, L"01004") == 0)
    {
        // This is a warning from the rdbms driver and its not specified as an error.
        Safir::SwReports::SendProgramInfoReport(L"String data, right truncated");
    }
    else if (wcscmp(wszSqlState, L"01002") == 0)
    {
        Safir::SwReports::SendProgramInfoReport(L"Cursor operation conflict");
    }
    else if (wcscmp(wszSqlState, L"01003") == 0)
    {
        Safir::SwReports::SendProgramInfoReport(L"NULL value eliminated in set function");
    }
    else if (wcscmp(wszSqlState, L"01S01") == 0)
    {
        Safir::SwReports::SendProgramInfoReport(L"Error in row");
    }
    else if (wcscmp(wszSqlState, L"01S02") == 0)
    {
        Safir::SwReports::SendProgramInfoReport(
            L"Statement attribute not supported. A similiar attribute used instead");
    }
    else if (wcscmp(wszSqlState, L"01S08") == 0)
    {
        Safir::SwReports::SendProgramInfoReport(L"Error saving file dsn.");
    }
    else
    {
        std::wstring string = wszSqlState;
        string += L":";
        string += wszMessageText;
        throw ReconnectException(string.c_str(),fileName,lineNumber);
    }
}

} // End namespace Odbc

} // End namespace Databases

} // End namespace Safir
