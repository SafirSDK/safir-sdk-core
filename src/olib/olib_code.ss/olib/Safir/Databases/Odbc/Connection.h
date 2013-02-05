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
#ifndef Safir_Databases_Connection_h
#define Safir_Databases_Connection_h

#include "Safir/Databases/Odbc/Defs.h"
#include "Safir/Databases/Odbc/Internal/InternalDefs.h"
#include <Safir/Dob/Typesystem/Defs.h>
#include <vector>
#include <boost/noncopyable.hpp>

namespace Safir
{
namespace Databases
{
namespace Odbc
{
class Environment;
class Statement;

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable: 4275)
#pragma warning(disable: 4251)
#endif

/** The Connection class models a connection made to the database engine.
    An application should have one connection object per connection and in
    ODBC one connection can only process one statement at the time. All
    Connections needs to be allocated and deallocated before use.
*/
class OLIB_API Connection : private boost::noncopyable
{
public:
    /** @brief Constructor
    */
    Connection():
        m_hConnection(SQL_NULL_HDBC),
        m_bIsConnected(false)
    {
        // This bit of code will make sure that the client is using the 
        // same size for SQLWCHAR as the library.
        CheckSQLWCHARSize(sizeof(SQLWCHAR));
    }

    /** @brief Destructor
    */
    ~Connection();

    /** @brief Allocates a new connection to the database.
    *
    * See SQLAllocHandle in ODBC documentation for more info.
    *
    * @param [in] environment - the environment the connection is made through.
    * @exception ReconnectException - SQLAllocHandle failed. Check GetDiagRec for info.
    */
    void Alloc(const Environment & environment);

    /** @brief Frees the connection previously allocated to the database.
    *
    * See SQLFreeHandle in ODBC documentation for more info.
    */
    void Free();

    /** @brief establishes a connection to the database.
    *
    * See SQLDriverConnect in ODBC documentation for more info.
    *
    * @param [in] wszConnectionString - an odbc connection string in which
    *               attributes for the connection to the RDBMS is contained.
    * @exception ReconnectException - SQLDriverConnect failed. Check GetDiagRec for info.
    */
    void Connect(const std::wstring & wszConnectionString);

    /** @brief establishes a connection to the database.
    *
    * See SQLDriverConnect in ODBC documentation for more info.
    *
    * @param [in] cszConnectionString - an odbc connection string in which
    *               attributes for the connection to the RDBMS is contained.
    * @exception ReconnectException - SQLDriverConnect failed. Check GetDiagRec for info.
    */
    void Connect(char * cszConnectionString);

    /** @brief disconnects from the database.
    *
    * All statements are automatically freed by ODBC when disconnecting.
    * After a disconnect statements have to be reallocated and prepared.
    * Columns and parameters have to be rebound.
    *
    * See SQLDisconnect in ODBC documentation for more info.
    *
    * @exception TimeoutException - Timeout has occurred.
    */
    void Disconnect();

    /** @brief Set odbc to automatically commit each transaction after a successful query.
    *
    * This is the default choice.
    * See SQLSetConnectAttr and the attribute SQL_ATTR_AUTOCOMMIT
    * in ODBC documentation for more info.
    *
    * @exception ReconnectException - SQLSetConnectAttr failed. Check GetDiagRec for info.
    */
    void UseAutoTransactions();

    /** @brief Set odbc to require manual commits to end transactions.
    *
    * Call this to be able to use Commit() and Rollback() to end transactions.
    * See SQLSetConnectAttr and the attribute SQL_ATTR_AUTOCOMMIT
    * in ODBC documentation for more info.
    *
    * @exception ReconnectException - SQLSetConnectAttr failed. Check GetDiagRec for info.
    */
    void UseManualTransactions();

    /** @brief Commits the changes permanently made in this transaction.
    *
    * Call UseManualTransactions() to be able to use Commit() and Rollback() to
    * end transactions. See SQLSetConnectAttr and the attribute SQL_ATTR_AUTOCOMMIT
    * in ODBC documentation for more info.
    *
    * @exception ReconnectException - SQLEndTran failed. Check GetDiagRec for info.
    */
    void Commit();

    /** @brief Remove all changes made in this transaction.
    *
    * Call UseManualTransactions() to be able to use Commit() and Rollback() to
    * end transactions. See SQLSetConnectAttr and the attribute SQL_ATTR_AUTOCOMMIT
    * in ODBC documentation for more info.
    *
    * @exception ReconnectException - SQLEndTran failed. Check GetDiagRec for info.
    */
    void Rollback();

    /** @brief Sets a value for a ODBC connection attribute.
    *
    * See SQLSetConnectAttr in ODBC documentation for more info.
    *
    * @param [in] lAttribute - An integer representing the attribute
    * @param [in] lValue - The value of the attribute to be set
    * @exception ReconnectException - SQLSetConnectAttr failed. Check GetDiagRec for info.
    */
    void SetConnectAttr(long lAttribute, long lValue);

    /** @brief Sets a value for a ODBC connection attribute.
    *
    * See SQLSetConnectAttr in ODBC documentation for more info.
    *
    * @param [in] lAttribute - An integer representing the attribute
    * @param [in] wszValue - The value of the attribute to be set
    * @exception ReconnectException - SQLSetConnectAttr failed. Check GetDiagRec for info.
    */
    void SetConnectAttr(long lAttribute, const std::wstring & wszValue);

    /** @brief Gets a value for a ODBC connection attribute.
    *
    * See SQLSetConnectAttr in ODBC documentation for more info.
    *
    * @param [in] lAttribute - An integer representing the attribute
    * @param [out] lValue - The value of the attribute
    * @exception ReconnectException - SQLGetConnectAttr failed. Check GetDiagRec for info.
    */
    void GetConnectAttr(long lAttribute, long & lValue) const;

    /** @brief Gets a value for a ODBC connection attribute.
    *
    * See SQLSetConnectAttr in ODBC documentation for more info.
    *
    * @param [in] lAttribute - An integer representing the attribute
    * @param [out] wszValue - The value of the attribute
    * @param [in] ulLength - Size in bytes of the szValue buffer.
    * @exception ReconnectException - SQLGetConnectAttr failed. Check GetDiagRec for info.
    */
    void GetConnectAttr(long lAttribute, wchar_t * wszValue, unsigned long ulLength) const;

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

    /** @brief Checks if this connection is a valid allocated connection.
    *
    * @return True if the connection is ok to use.
    */
    bool IsValid() const;

    /** @brief Checks if a connection has been established
    *
    * @return An ODBC Environment handle.
    */
    bool IsConnected() const;

    /** @brief Returns the ODBC Handle of the environment
    *
    * @return An ODBC Environment handle.
    */
    SQLHDBC Handle() const;
private:
    SQLHDBC m_hConnection;
    bool m_bIsConnected;

    typedef std::pair<std::wstring,std::wstring> StateMessagePair;
    //returns pair of SQLState and MessageText
    const StateMessagePair GetDiagRec() const;

    void AddStatement(Statement * pStatement);
    void RemoveStatement(Statement * pStatement);
    void EndTran(short sCompletionType);
    void ThrowReconnectException(   const std::wstring & fileName,
                                    const Safir::Dob::Typesystem::Int64 lineNumber) const;
    void ThrowReconnectException(   SQLSMALLINT HandleType,
                                    SQLHANDLE Handle,
                                    const std::wstring & fileName,
                                    const Safir::Dob::Typesystem::Int64 lineNumber) const;

    std::vector<Statement *> m_statements;

    friend class Safir::Databases::Odbc::Statement;

    static void CheckSQLWCHARSize(const size_t size);
};

#ifdef _MSC_VER
#pragma warning(pop)
#endif

inline
void Connection::UseAutoTransactions()
{
    SetConnectAttr(SQL_ATTR_AUTOCOMMIT, static_cast<long>(SQL_AUTOCOMMIT_ON));
}

inline
void Connection::UseManualTransactions()
{
    SetConnectAttr(SQL_ATTR_AUTOCOMMIT, static_cast<long>(SQL_AUTOCOMMIT_OFF));
}

inline
void Connection::Commit()
{
    EndTran(SQL_COMMIT);
}

inline
void Connection::Rollback()
{
    EndTran(SQL_ROLLBACK);
}

inline
bool Connection::IsValid() const
{
    return m_hConnection != SQL_NULL_HDBC;
}

inline
bool Connection::IsConnected() const
{
    return m_bIsConnected;
}

inline
SQLHDBC Connection::Handle() const
{
    return m_hConnection;
}

} // End namespace Odbc

} // End namespace Databases

} // End namespace Safir

#endif //Safir_Databases_Connection_h
