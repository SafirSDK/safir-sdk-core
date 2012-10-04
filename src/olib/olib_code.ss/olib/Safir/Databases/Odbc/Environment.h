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
#ifndef Safir_Databases_Environment_h
#define Safir_Databases_Environment_h

#include "Safir/Databases/Odbc/Defs.h"
#include "Safir/Databases/Odbc/Internal/InternalDefs.h"
#include <Safir/Dob/Typesystem/Defs.h>
#include <boost/noncopyable.hpp>

namespace Safir
{
namespace Databases
{
namespace Odbc
{

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable: 4275)
#endif

/** The Environment class models the necessary setup each application neeeds
    in order to access the database. Only one Environment is necessary in a
    application and all environments needs to be Allocated and Deallocated
    before use.
*/
class OLIB_API Environment : private boost::noncopyable
{
public:
    /** @brief Constructor
    */
    Environment(void);

    /** @brief Destructor
    */
    ~Environment(void);

    /** @brief Allocates a new environment for the database.
    *
    * See SQLAllocHandle in ODBC documentation for more info.
    *
    * @exception ReconnectException - SQLAllocHandle failed. Check GetDiagRec for info.
    */
    void Alloc();

    /** @brief Frees the environment previously allocated for the database.
    *
    * See SQLFreeHandle in ODBC documentation for more info.
    */
    void Free();

    /** @brief Sets an integer Environment attribute.
    *
    * Some attributes can be set without allocating the environment first.
    * See SQLSetEnvAttr in ODBC documentation for more info.
    *
    * @param [in] lAttribute - An integer representing the attribute
    * @param [in] lValue - The value of the attribute to be set
    * @exception ReconnectException - SQLSetEnvAttr failed. Check GetDiagRec for info.
    */
    void SetEnvAttr(long lAttribute, long lValue);

    /** @brief Sets an string Environment attribute.
    *
    * Some attributes can be set without allocating the environment first.
    * See SQLSetEnvAttr in ODBC documentation for more info.
    *
    * @param [in] lAttribute - An integer representing the attribute
    * @param [in] wszValue - The value of the attribute to be set
    * @param [in] ulLength - The length of string.
    * @exception ReconnectException - SQLSetEnvAttr failed. Check GetDiagRec for info.
    */
    void SetEnvAttr(long lAttribute, const std::wstring & wszValue, unsigned long ulLength);

    /** @brief Gets the value of an integer Environment attribute.
    *
    * Some attributes can be retrieved without allocating the environment first.
    * See SQLGetEnvAttr in ODBC documentation for more info.
    *
    * @param [in] lAttribute - An integer representing the attribute
    * @param [out] lValue - The value of the attribute to be set
    * @exception ReconnectException - SQLGetEnvAttr failed. Check GetDiagRec for info.
    */
    void GetEnvAttr(long lAttribute, long & lValue) const;

    /** @brief Gets the value of a string Environment attribute.
    *
    * Some attributes can be retrieved without allocating the environment first.
    * See SQLGetEnvAttr in ODBC documentation for more info.
    *
    * @param [in] lAttribute - An integer representing the attribute
    * @param [out] wszValue - The value of the attribute to be set
    * @param [in] ulLength - The length of string.
    * @exception ReconnectException - SQLGetEnvAttr failed. Check GetDiagRec for info.
    */
    void GetEnvAttr(long lAttribute, wchar_t * wszValue, unsigned long ulLength) const;

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

    /** @brief Checks if this environment is a valid allocated environment.
    *
    * @return True if the environment is ok to use.
    */
    bool IsValid() const;

    /** @brief Returns the ODBC Handle of the environment
    *
    * @return An ODBC Environment handle.
    */
    SQLHENV Handle() const;
private:
    SQLHENV m_hEnv;
    bool bSetOdbcVersion;

    void ThrowReconnectException(   const std::wstring & fileName,
                                    const Safir::Dob::Typesystem::Int64 lineNumber) const;
    void ThrowReconnectException(   SQLHENV hEnv,
                                    const std::wstring & fileName,
                                    const Safir::Dob::Typesystem::Int64 lineNumber) const;

};

#ifdef _MSC_VER
#pragma warning(pop)
#endif

inline
bool Environment::IsValid() const
{
    return m_hEnv != SQL_NULL_HENV;
}

inline
SQLHENV Environment::Handle() const
{
    return m_hEnv;
}

} // End namespace Odbc

} // End namespace Databases

} // End namespace Safir

#endif // Safir_Databases_Environment_h
