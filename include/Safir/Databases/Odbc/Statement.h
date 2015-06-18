/******************************************************************************
*
* Copyright Saab AB, 2005-2013 (http://safir.sourceforge.net)
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
#ifndef Safir_Databases_Statement_h
#define Safir_Databases_Statement_h

#ifndef SAFIR_NO_DEPRECATED

#include "Safir/Databases/Odbc/Defs.h"
#include "Safir/Databases/Odbc/Internal/InternalDefs.h"
#include "Safir/Databases/Odbc/Internal/Parameter.h"
#include "Safir/Databases/Odbc/Internal/Column.h"
#include "Safir/Databases/Odbc/Internal/BufferedWideStringParameter.h"
#include "Safir/Databases/Odbc/Internal/NonBufferedWideStringParameter.h"
#include "Safir/Databases/Odbc/Internal/BufferedBinaryParameter.h"
#include "Safir/Databases/Odbc/Internal/NonBufferedBinaryParameter.h"
#include "Safir/Databases/Odbc/Internal/BinaryColumn.h"
#include "Safir/Databases/Odbc/Internal/WideStringColumn.h"

#include <boost/noncopyable.hpp>
#include <Safir/Dob/Typesystem/Exceptions.h>

namespace Safir
{
namespace Databases
{
namespace Odbc
{
    class Connection;

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable: 4275)
#endif

/**
 * The Statement class models a statement made to the database engine
 * and one statement object should be made for each statement or query
 * to the database. All statements needs to be Allocated and Deallocated
 * at connect and disconnect.
 *
 * @deprecated All functionality in the Safir::Databases::Odbc namespace is deprecated.
*/
class OLIB_API Statement : private boost::noncopyable
{
public:
    /** @brief Constructor
    */
    Statement();

    /** @brief Destructor
    */
    ~Statement();

    /** @brief Allocates a new statement in a connection.
    *
    * See SQLAllocHandle in ODBC documentation for more info.
    *
    * @param [in] pConnection - the connection the statement is made through.
    * @exception ReconnectException - SQLAllocHandle failed. Check GetDiagRec for info.
    */
    void Alloc(Connection & pConnection);

    /** @brief Frees the statement previously allocated to the database.
    *
    * Statements are freed automatically when disconnected.
    * See SQLFreeHandle in ODBC documentation for more info.
    *
    * @exception ReconnectException - SQLFreeHandle failed. Check GetDiagRec for info.
    */
    void Free();

    /** @brief Closes a recordset returned by an Execute() or ExecDirect().
    *
    * See SQLCloseCursor in ODBC documentation for more info.
    *
    * @exception ReconnectException - SQLCloseCursor failed. Check GetDiagRec for info.
    */
    void CloseCursor();

    /** @brief Executes an already prepared statement.
    *
    * See SQLExecute in ODBC documentation for more info.
    *
    * @exception IntegrityConstraintException - The query violated the integrity constraints
    * of the database.
    * @exception ReconnectException - SQLExecute failed. Check GetDiagRec for info.
    * @exception RetryException - Retry the operation.
    * @exception TimeoutException - Timeout has occurred. Retry the operation.
    */
    void Execute();

    /** @brief Prepares a statement for later execution.
    *
    * See SQLPrepare in ODBC documentation for more info.
    *
    * @param [in] wszSqlCommand - the sql query this statement shall execute.
    * @exception ReconnectException - SQLPrepare failed. Check GetDiagRec for info.
    * @exception RetryException - Retry the operation.
    */
    void Prepare(const std::wstring & wszSqlCommand);

    /** @brief Prepares and executes an statement.
    *
    * See SQLExecDirect in ODBC documentation for more info.
    *
    * @param [in] wszSqlCommand - the sql query this statement shall execute.
    * @exception ReconnectException - SQLExecDirect failed. Check GetDiagRec for info.
    * @exception RetryException - Retry the operation.
    * @exception TimeoutException - Timeout has occurred. Retry the operation.
   */
    void ExecDirect(const std::wstring & wszSqlCommand);

    /** @brief Reads one row in an recordset into the columns bound to this query.
    *
    * See SQLFetch in ODBC documentation for more info.
    *
    * @return True if data was returned. False if at end of the recordset.
    * @exception ReconnectException - SQLFetch failed. Check GetDiagRec for info.
    * @exception RetryException - Retry the operation.
    * @exception TimeoutException - Timeout has occurred. Retry the operation.
    */
    bool Fetch();

    /** @brief Retrieves more results (resultsets or parameters) from the database.
    *
    * See SQLMoreResults in ODBC documentation for more info.
    *
    * @return True if more result sets exists. False if all result sets have been processed.
    * @exception ReconnectException - SQLMoreResults failed. Check GetDiagRec for info.
    * @exception RetryException - Retry the operation.
    */
    bool MoreResults();

    /** @brief Returns the number of columns generated from a query.
    *
    * Call this method after a successful call to execute.
    * See SQLNumResultCols in ODBC documentation for more info.
    *
    * @return Returns the number of columns generated by last statement.
    * Returns 0 if no columns were generated.
    * @exception ReconnectException - SQLNumResultCols failed. Check GetDiagRec for info.
    */
   int GetNumberOfColumns() const;

    /** @brief Sets a value for a ODBC statement attribute.
    *
    * See SQLSetStmtAttr in ODBC documentation for more info.
    *
    * @param [in] lAttribute - An integer representing the attribute
    * @param [in] lValue - The value of the attribute to be set
    * @exception ReconnectException - SQLSetStmtAttr failed. Check GetDiagRec for info.
    */
    void SetStmtAttr(long lAttribute, long lValue);

    /** @brief Sets a value for a ODBC statement attribute.
    *
    * See SQLSetStmtAttr in ODBC documentation for more info.
    *
    * @param [in] lAttribute - An integer representing the attribute
    * @param [in] wszValue - The value of the attribute to be set
    * @exception ReconnectException - SQLSetStmtAttr failed. Check GetDiagRec for info.
    */
    void SetStmtAttr(long lAttribute, const std::wstring & wszValue);

    /** @brief Gets a value for a ODBC statement attribute.
    *
    * See SQLGetStmtAttr in ODBC documentation for more info.
    *
    * @param [in] lAttribute - An integer representing the attribute
    * @param [out] lValue - The value of the attribute
    * @exception ReconnectException - SQLGetStmtAttr failed. Check GetDiagRec for info.
    */
    void GetStmtAttr(long lAttribute, long & lValue) const;

    /** @brief Gets a value for a ODBC statement attribute.
    *
    * See SQLGetStmtAttr in ODBC documentation for more info.
    *
    * @param [in] lAttribute - An integer representing the attribute
    * @param [out] wszValue - The value of the attribute
    * @param [in] ulLength - Size in bytes of the szValue buffer.
    * @exception ReconnectException - SQLGetStmtAttr failed. Check GetDiagRec for info.
    */
    void GetStmtAttr(long lAttribute, wchar_t * wszValue, unsigned long ulLength) const;

    /** @brief Changes PutData() to add data to next parameter.
    *
    * See SQLParamData in ODBC documentation for more info.
    *
    * @param [out] lParameter - The next parameter that need data.
    * @return True if more parameters needed data otherwise False;
    * @exception ReconnectException - SQLParamData failed. Check GetDiagRec for info.
    * @exception RetryException - Retry the operation.
   */
    bool ParamData(unsigned short & lParameter) const;

    /** @brief Binds a parameter to a statement.
    *
    * Parameters are unbound when connection is disconnected.
    * See SQLBindParameter in ODBC documentation for more info.
    *
    * @param [in] usParameterNumber - the position of this parameter
    * in the parameter list. Starts at 1.
    * @param [in] param - an instance of the parameter class.
    * @exception ReconnectException - The operation cannot succeed without reconnecting to the RDBMS
    * @exception RetryException - Retry the operation.
    */
    template<short sCType, short sSqlType, class Type, short sInputOutputType, unsigned long lColumnSize>
    void BindParameter( unsigned short usParameterNumber,
                        Internal::Parameter<sCType, sSqlType, Type, sInputOutputType,lColumnSize> & param );
    template<short sSqlType, short sInputOutputType>
    void BindParameter( unsigned short usParameterNumber,
                        Internal::NonBufferedWideStringParameter<sSqlType, sInputOutputType> & param );


    /** @brief Binds a long parameter to a statement.
    *
    * Parameters are unbound when connection is disconnected.
    * See SQLBindParameter in ODBC documentation for more info.
    *
    * @param [in] usParameterNumber - the position of this parameter
    * in the parameter list. Starts at 1.
    * @param [in] param - an instance of the parameter class.
    * @exception ReconnectException - The operation cannot succeed without reconnecting to the RDBMS
    * @exception RetryException - Retry the operation.
    */
    template<short sCType, short sSqlType, class Type, short sInputOutputType, unsigned long lColumnSize>
    void BindLongParameter( unsigned short usParameterNumber,
                            Internal::Parameter<sCType, sSqlType, Type, sInputOutputType,lColumnSize> & param );

    /** @brief Binds a column to a statement.
    *
    * Columns are unbound when connection is disconnected.
    * See SQLBindColumn in ODBC documentation for more info.
    *
    * @param [in] usColumnNumber - the position of this column as returned by the query. Starts at 1.
    * @param [in] column - an instance of the column class.
    * @exception ReconnectException - The operation cannot succeed without reconnecting to the RDBMS
    * @exception RetryException - Retry the operation.
    */
    template<short sValueType, class Type>
    void BindColumn(unsigned short usColumnNumber,
                    Internal::Column<sValueType, Type> & column );

    /** @brief Get data from a column.
    *
    * See SQLGetData in ODBC documentation for more info.
    *
    * @param [in] usColumnNumber - the position of this column as returned by the query. Starts at 1.
    * @param [in] column - an instance of the column class.
    * @return True if data was returned. False if at end of the recordset.
    * @exception ReconnectException - The operation cannot succeed without reconnecting to the RDBMS
    * @exception RetryException - Retry the operation.
    */
    template<short sValueType, class Type>
    bool GetData(   unsigned short usColumnNumber,
                    Internal::Column<sValueType, Type> & column );

    /** @brief Put data into a parameter.
    *
    * See SQLPutData in ODBC documentation for more info.
    *
    * @param [in] param - an instance of the parameter class.
    * @exception ReconnectException - The operation cannot succeed without reconnecting to the RDBMS
    * @exception RetryException - Retry the operation.
    */
    template<short sCType, short sSqlType, class Type, short sInputOutputType, unsigned long lColumnSize>
    void PutData(Internal::Parameter<sCType, sSqlType, Type, sInputOutputType,lColumnSize> & param );

    /** @brief Get a diagnostics record.
    *
    * See SQLGetDiagRec in ODBC documentation for more info.
    *
    * @param [in] sRecNumber -  The sql error record. Starts at 1.
    * @param [out] SqlState - The five char sql state error code.
    * @param [out] NativeError - Driver specific error code.
    * @param [out] MessageText - The diagnostic message text string.
    * @param [out] bDataRead - true if data has been placed in the buffers.
    */
    bool GetDiagRec(short sRecNumber,
                    std::wstring & SqlState,
                    boost::int32_t & NativeError,
                    std::wstring & MessageText,
                    bool & bDataRead) const;

    /** @brief Checks if this statement is a valid allocated statement.
    *
    * @return True if the statement is ok to use.
    */
    bool IsValid() const;

    /** @brief Returns the ODBC Handle of the statement
    *
    * @return An ODBC statement handle.
    */
    SQLHDBC Handle() const;
private:
    SQLHSTMT m_hStatement;
    Connection * m_pConnection;

    void SetNotValid();
    void ThrowException(const std::wstring & fileName,
                        const Safir::Dob::Typesystem::Int64 lineNumber,
                        SQLRETURN original_returncode) const;
    void ThrowException(SQLSMALLINT HandleType,
                        SQLHANDLE Handle,
                        const std::wstring & fileName,
                        const Safir::Dob::Typesystem::Int64 lineNumber,
                        SQLRETURN original_returncode) const;

    friend class Safir::Databases::Odbc::Connection;
};

#ifdef _MSC_VER
#pragma warning(pop)
#endif

inline
SQLHDBC Statement::Handle() const
{
    return m_hStatement;
}

inline
bool Statement::IsValid() const
{
    return m_hStatement != SQL_NULL_HSTMT;
}

inline
void Statement::SetNotValid()
{
    m_hStatement = SQL_NULL_HSTMT;
}

template<short sValueType, class Type>
bool Statement::GetData(unsigned short usColumnNumber,
                        Internal::Column<sValueType, Type> & column )
{
    SQLRETURN ret;
    bool bDataFound = true;

    if (!IsValid())
        throw Safir::Dob::Typesystem::SoftwareViolationException(L"Using an invalid statement",__WFILE__,__LINE__);

    ret = ::SQLGetData( m_hStatement,                   // StatementHandle
                        usColumnNumber,                 // ColumnNumber,
                        column.m_csValueType,           // TargetType,
                        column.GetValuePtr(),           // TargetValuePtr,
                        column.GetSize(),               // BufferLength,
                        column.GetLengthOrIndPtr());    // StrLen_or_Ind
    if (ret==SQL_NO_DATA_FOUND)
        bDataFound = false;
    else if (!SQL_SUCCEEDED(ret))
    {
        ThrowException(__WFILE__,__LINE__, ret);
    }

    return bDataFound;
}

template<short sValueType, class Type>
void Statement::BindColumn( unsigned short usColumnNumber,
                            Internal::Column<sValueType, Type> & column )
{
    SQLRETURN ret;

    if (!IsValid())
        throw Safir::Dob::Typesystem::SoftwareViolationException(L"Using an invalid statement",__WFILE__,__LINE__);

    ret = ::SQLBindCol( m_hStatement,                   // StatementHandle
                        usColumnNumber,                 // ColumnNumber,
                        column.m_csValueType,           // TargetType,
                        column.GetValuePtr(),           // TargetValuePtr,
                        column.GetSize(),               // BufferLength,
                        column.GetLengthOrIndPtr());    // StrLen_or_Ind
    if (!SQL_SUCCEEDED(ret))
    {
        ThrowException(__WFILE__,__LINE__, ret);
    }
}

template<short sCType, short sSqlType, class Type, short sInputOutputType, unsigned long lColumnSize>
void Statement::BindParameter(  unsigned short usParameterNumber,
                                Internal::Parameter<sCType, sSqlType, Type, sInputOutputType,lColumnSize> & param )
{
    SQLRETURN ret;

    if (!IsValid())
        throw Safir::Dob::Typesystem::SoftwareViolationException(L"Using an invalid statement",__WFILE__,__LINE__);

    ret = ::SQLBindParameter(   m_hStatement,                   // StatementHandle
                                usParameterNumber,              // ParameterNumber,
                                param.m_csInputOutputType,      // InputOutputType
                                param.m_csCType,                // ValueType
                                param.m_csSqlType,              // ParameterType
                                param.GetColumnSize(),          // ColumnSize
                                param.GetDecimal(),             // DecimalDigits
                                param.GetValuePtr(),            // ParameterValuePtr
                                param.GetSize(),                // BufferLength
                                param.GetLengthOrIndPtr() );    // StrLen_or_Ind
    if (!SQL_SUCCEEDED(ret))
    {
        ThrowException(__WFILE__,__LINE__, ret);
    }
}

template<short sSqlType, short sInputOutputType>
void Statement::BindParameter(  unsigned short /*usParameterNumber*/,
                                Internal::NonBufferedWideStringParameter<sSqlType, sInputOutputType> & /*param*/ )
{
    throw Safir::Dob::Typesystem::SoftwareViolationException(L"This parameter can only be used with PutData",__WFILE__,__LINE__);
}


template<short sCType, short sSqlType, class Type, short sInputOutputType, unsigned long lColumnSize>
void Statement::BindLongParameter(  unsigned short usParameterNumber,
                                    Internal::Parameter<sCType, sSqlType, Type, sInputOutputType,lColumnSize> & param )
{
    SQLRETURN ret;

    if (!IsValid())
        throw Safir::Dob::Typesystem::SoftwareViolationException(L"Using an invalid statement",__WFILE__,__LINE__);

    ret = ::SQLBindParameter(   m_hStatement,                   // StatementHandle
                                usParameterNumber,              // ParameterNumber,
                                param.m_csInputOutputType,      // InputOutputType
                                param.m_csCType,                // ValueType
                                param.m_csSqlType,              // ParameterType
                                param.GetColumnSize(),          // ColumnSize
                                param.GetDecimal(),             // DecimalDigits
                                reinterpret_cast<SQLPOINTER>(usParameterNumber),    // ParameterValuePtr
                                param.GetSize(),                // BufferLength
                                param.GetLengthOrIndPtr() );    // StrLen_or_Ind
    if (!SQL_SUCCEEDED(ret))
    {
        ThrowException(__WFILE__,__LINE__, ret);
    }
}

template<short sCType, short sSqlType, class Type, short sInputOutputType, unsigned long lColumnSize>
void Statement::PutData(Internal::Parameter<sCType, sSqlType, Type, sInputOutputType,lColumnSize> & param )
{
    SQLRETURN ret;

    if (!IsValid())
        throw Safir::Dob::Typesystem::SoftwareViolationException(L"Using an invalid statement",__WFILE__,__LINE__);

    ret = ::SQLPutData( m_hStatement,                   // StatementHandle
                        param.GetValuePtr(),            // ParameterValuePtr
                        param.GetSize() );       // StrLen_or_Ind

    if (!SQL_SUCCEEDED(ret))
    {
        ThrowException(__WFILE__,__LINE__, ret);
    }
}

} // End namespace Odbc

} // End namespace Databases

} // End namespace Safir

#endif
#endif // Safir_Databases_Statement_h
