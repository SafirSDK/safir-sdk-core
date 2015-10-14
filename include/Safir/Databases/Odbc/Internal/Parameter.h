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
#ifndef Safir_Databases_Odbc_Internal_Parameter_h
#define Safir_Databases_Odbc_Internal_Parameter_h

#include "Safir/Databases/Odbc/Defs.h"
#include "Safir/Databases/Odbc/Internal/InternalDefs.h"
#include <Safir/Dob/Typesystem/Exceptions.h>
#include <Safir/Time/TimeProvider.h>
#include <boost/noncopyable.hpp>
#include <boost/scoped_array.hpp>

namespace Safir
{
namespace Databases
{
namespace Odbc
{
class Statement;

namespace Internal
{
/** The Parameter class models a parameter in a query to the database.
    One parameter object should be made for each parameter in a query
    and all parameters needs to be bound to a prepared statement before
    used.
*/
template<short sCType, short sSqlType, class Type, short sInputOutputType, unsigned long lColumnSize>
class Parameter : private boost::noncopyable
{
public:
    /** @brief Constructor for simple types.
    */
    Parameter();

    /** @brief Constructor for complex types with a unknown size at compile time.
    */
    Parameter(SQLLEN nSize);

    /** @brief Destructor
    */
    virtual ~Parameter();

    /** @brief Returns true if size is available.
    *
    * Size is used for input or input-output parameters of a specific type.
    *
    * @return True if size is available.
    */
    bool IsRetrievedSizeAvailable();

    /** @brief Returns the available size.
    *
    * Size is used for input or input-output parameters of a specific type.
    *
    * @return True if size is available.
    */
    SQLLEN GetRetrievedSize();

    /** @brief Sets the value for a parameter.
    *
    *  @param [in] value - The value to set.
    */
    void SetValue(const Type & value);

    /** @brief Sets the value to NULL for a parameter.
    */
    void SetNull();

    /** @brief Makes the stored procedure to use the default value for a parameter.
    */
    void UseDefaultValue();

    /** @brief Used by LongParameters to notify that this amount of data is sent after Execute()
    *
    * SetValueAtExecution must be sent for LongParameters before each Execute() to notify the
    * database engine that a certain amount of data is sent by the PutData() method.
    *
    *  @param [in] nSize - The size sent in bytes.
    */
    void SetValueAtExecution(int nSize);

    /** @brief Returns the value of the parameter.
    *
    * If the value is NULL then the returned value is unknown.
    *
    * @return the value of the parameter.
    */
    Type & GetValue();
    const Type & GetValue() const;

    /** @brief Returns true if the parameter is NULL.
    *
    * @return True if the parameter is NULL.
    */
    bool IsNull() const;

protected:
    Type m_value;
    SQLLEN m_lpLengthOrInd;
    SQLLEN m_nSize;
    short m_nDecimal;

    static const short m_csCType = sCType;
    static const short m_csSqlType = sSqlType;
    static const short m_csInputOutputType = sInputOutputType;

    virtual unsigned long GetColumnSize();
    virtual void * GetValuePtr();
    SQLLEN GetLengthOrInd();
    SQLLEN * GetLengthOrIndPtr();
    void SetLengthOrInd(SQLLEN lValue);
    SQLLEN GetSize();
    short GetDecimal();

    friend class Safir::Databases::Odbc::Statement;
};

/** The BooleanParameter is a parameter for boolean values entered into the database as the
*   ODBC type SQL_BIT.
*/
template<short sInputOutputType>
class BooleanParameter : public Parameter<SQL_C_BIT, SQL_BIT, unsigned char,sInputOutputType,1>
{
public:
    void SetValue(bool bValue);
    bool GetValue();

private:
    typedef Parameter<SQL_C_BIT, SQL_BIT, unsigned char,sInputOutputType,1> InstantiatedParameter;
};

/** The TimeParameter is a parameter for datetime values entered into the database as the
*   ODBC type SQL_TYPE_TIMESTAMP. Please note that the fraction part is noted in nanoseconds
*   and since the database only uses 6 digits the first lowest 3 has to be zero's.
*
*   Example: Fraction in database 000 001 uses 000 001 000 in timestamp struct.
*/
template<short sInputOutputType>
class TimeParameter : public Parameter<SQL_C_TYPE_TIMESTAMP, SQL_TYPE_TIMESTAMP, TIMESTAMP_STRUCT,sInputOutputType,26> //Was 19
{
public:
    TimeParameter();
    void SetTimeStamp(const TIMESTAMP_STRUCT & tsTime);
    void SetValue(Safir::Dob::Typesystem::Si64::Second time);

    const TIMESTAMP_STRUCT & GetTimeStamp() const;
    TIMESTAMP_STRUCT GetTimeStamp();
    void GetValue(Safir::Dob::Typesystem::Si64::Second & time) const;

private:
    typedef Parameter<SQL_C_TYPE_TIMESTAMP, SQL_TYPE_TIMESTAMP, TIMESTAMP_STRUCT,sInputOutputType,26> InstantiatedParameter;
};

/////////////////////////////////////////////////////////////////////////////////////////
// Method implementations - Parameter
/////////////////////////////////////////////////////////////////////////////////////////

template<short sCType, short sSqlType, class Type, short sInputOutputType, unsigned long lColumnSize>
inline Parameter<sCType, sSqlType, Type, sInputOutputType,lColumnSize>::Parameter()
    : m_lpLengthOrInd( SQL_NULL_DATA ), m_nSize(sizeof(Type)), m_nDecimal(0)
{
}

template<short sCType, short sSqlType, class Type, short sInputOutputType, unsigned long lColumnSize>
inline Parameter<sCType, sSqlType, Type, sInputOutputType,lColumnSize>::Parameter(SQLLEN nSize)
    : m_lpLengthOrInd( SQL_NULL_DATA ), m_nSize(nSize)
{
}

template<short sCType, short sSqlType, class Type, short sInputOutputType, unsigned long lColumnSize>
inline Parameter<sCType, sSqlType, Type, sInputOutputType,lColumnSize>::~Parameter()
{
}

template<short sCType, short sSqlType, class Type, short sInputOutputType, unsigned long lColumnSize>
unsigned long Parameter<sCType, sSqlType, Type, sInputOutputType,lColumnSize>::GetColumnSize()
{
    return lColumnSize;
}

template<short sCType, short sSqlType, class Type, short sInputOutputType, unsigned long lColumnSize>
inline SQLLEN Parameter<sCType, sSqlType, Type, sInputOutputType,lColumnSize>::GetSize()
{
    return m_nSize;
}

template<short sCType, short sSqlType, class Type, short sInputOutputType, unsigned long lColumnSize>
inline void * Parameter<sCType, sSqlType, Type, sInputOutputType,lColumnSize>::GetValuePtr()
{
    return &m_value;
}

template<short sCType, short sSqlType, class Type, short sInputOutputType, unsigned long lColumnSize>
inline SQLLEN  Parameter<sCType, sSqlType, Type, sInputOutputType,lColumnSize>::GetLengthOrInd()
{
    return m_lpLengthOrInd;
}

template<short sCType, short sSqlType, class Type, short sInputOutputType, unsigned long lColumnSize>
inline SQLLEN * Parameter<sCType, sSqlType, Type, sInputOutputType,lColumnSize>::GetLengthOrIndPtr()
{
    return &m_lpLengthOrInd;
}

template<short sCType, short sSqlType, class Type, short sInputOutputType, unsigned long lColumnSize>
inline void Parameter<sCType, sSqlType, Type, sInputOutputType,lColumnSize>::SetLengthOrInd(SQLLEN lValue)
{
    m_lpLengthOrInd = lValue;
}

template<short sCType, short sSqlType, class Type, short sInputOutputType, unsigned long lColumnSize>
inline void Parameter<sCType, sSqlType, Type, sInputOutputType,lColumnSize>::SetValue(const Type & value)
{
    m_value = value;
    m_lpLengthOrInd = sizeof(Type);
}

template<short sCType, short sSqlType, class Type, short sInputOutputType, unsigned long lColumnSize>
inline void Parameter<sCType, sSqlType, Type, sInputOutputType,lColumnSize>::SetNull()
{
    m_lpLengthOrInd = SQL_NULL_DATA;
}

template<short sCType, short sSqlType, class Type, short sInputOutputType, unsigned long lColumnSize>
inline void Parameter<sCType, sSqlType, Type, sInputOutputType,lColumnSize>::UseDefaultValue()
{
    m_lpLengthOrInd = SQL_DEFAULT_PARAM;
}

template<short sCType, short sSqlType, class Type, short sInputOutputType, unsigned long lColumnSize>
inline Type & Parameter<sCType, sSqlType, Type, sInputOutputType,lColumnSize>::GetValue()
{
    return m_value;
}

template<short sCType, short sSqlType, class Type, short sInputOutputType, unsigned long lColumnSize>
inline const Type & Parameter<sCType, sSqlType, Type, sInputOutputType,lColumnSize>::GetValue() const
{
    return m_value;
}

template<short sCType, short sSqlType, class Type, short sInputOutputType, unsigned long lColumnSize>
inline bool  Parameter<sCType, sSqlType, Type, sInputOutputType,lColumnSize>::IsNull() const
{
    return m_lpLengthOrInd == SQL_NULL_DATA;
}

template<short sCType, short sSqlType, class Type, short sInputOutputType, unsigned long lColumnSize>
inline bool Parameter<sCType, sSqlType, Type, sInputOutputType,lColumnSize>::IsRetrievedSizeAvailable()
{
    return ((m_lpLengthOrInd != SQL_NULL_DATA) && (m_lpLengthOrInd != SQL_NO_TOTAL));
}

template<short sCType, short sSqlType, class Type, short sInputOutputType, unsigned long lColumnSize>
inline SQLLEN Parameter<sCType, sSqlType, Type, sInputOutputType,lColumnSize>::GetRetrievedSize()
{
    return m_lpLengthOrInd;
}

template<short sCType, short sSqlType, class Type, short sInputOutputType, unsigned long lColumnSize>
inline void Parameter<sCType, sSqlType, Type, sInputOutputType,lColumnSize>::SetValueAtExecution(int nSize)
{
    SetLengthOrInd( SQL_LEN_DATA_AT_EXEC( nSize ) );
}

template<short sCType, short sSqlType, class Type, short sInputOutputType, unsigned long lColumnSize>
inline short Parameter<sCType, sSqlType, Type, sInputOutputType,lColumnSize>::GetDecimal()
{
    return m_nDecimal;
}

/////////////////////////////////////////////////////////////////////////////////////////
// Method implementations - BooleanParameter
/////////////////////////////////////////////////////////////////////////////////////////

template<short sInputOutputType>
inline void BooleanParameter<sInputOutputType>::SetValue(bool bValue)
{
    InstantiatedParameter::SetValue(bValue ? 1 : 0);
}

template<short sInputOutputType>
inline bool BooleanParameter<sInputOutputType>::GetValue()
{
    return InstantiatedParameter::GetValue() == 1;
}

/////////////////////////////////////////////////////////////////////////////////////////
// Method implementations - TimeParameter
/////////////////////////////////////////////////////////////////////////////////////////
template<short sInputOutputType>
inline TimeParameter<sInputOutputType>::TimeParameter() : InstantiatedParameter()
{
    InstantiatedParameter::m_nDecimal = 6;
}

template<short sInputOutputType>
inline void TimeParameter<sInputOutputType>::SetTimeStamp(const TIMESTAMP_STRUCT & tsTime)
{
    InstantiatedParameter::SetValue( tsTime );
    //m_value.fraction = 0; // Fraction should always be 0 because of Datetime field overflow
}

template<short sInputOutputType>
inline void TimeParameter<sInputOutputType>::SetValue(Safir::Dob::Typesystem::Si64::Second time)
{
    boost::posix_time::ptime pTime;
    TIMESTAMP_STRUCT value;
    SQLUINTEGER NanoSecs;

    pTime = Safir::Time::TimeProvider::ToPtime( time );
    value.year = static_cast<SQLSMALLINT>(pTime.date().year());
    value.month = static_cast<SQLUSMALLINT>(pTime.date().month());
    value.day = static_cast<SQLUSMALLINT>(pTime.date().day());
    value.hour = static_cast<SQLUSMALLINT>(pTime.time_of_day().hours());
    value.minute = static_cast<SQLUSMALLINT>(pTime.time_of_day().minutes());
    value.second = static_cast<SQLUSMALLINT>(pTime.time_of_day().seconds());

    // The fraction part of the TIMESTAMP_STRUCT has to be nanoseconds with
    // 3 zeros at the beginning.
    NanoSecs = static_cast<SQLUINTEGER>(pTime.time_of_day().fractional_seconds());
    NanoSecs = (NanoSecs / 1000) * 1000;
    value.fraction = NanoSecs;

    InstantiatedParameter::SetValue( value );
}

template<short sInputOutputType>
inline const TIMESTAMP_STRUCT & TimeParameter<sInputOutputType>::GetTimeStamp() const
{
    return InstantiatedParameter::GetValue();
}

template<short sInputOutputType>
inline TIMESTAMP_STRUCT TimeParameter<sInputOutputType>::GetTimeStamp()
{
    return InstantiatedParameter::GetValue();
}

template<short sInputOutputType>
inline void TimeParameter<sInputOutputType>::GetValue(Safir::Dob::Typesystem::Si64::Second & time) const
{
    boost::gregorian::date pDate(InstantiatedParameter::m_value.year,
                                 InstantiatedParameter::m_value.month,
                                 InstantiatedParameter::m_value.day);
    boost::posix_time::time_duration pTimeDuration(InstantiatedParameter::m_value.hour,
                                                   InstantiatedParameter::m_value.minute,
                                                   InstantiatedParameter::m_value.second,
                                                   InstantiatedParameter::m_value.fraction);
    boost::posix_time::ptime pTime(pDate, pTimeDuration);

    time = Safir::Time::TimeProvider::ToDouble( pTime );
}

} // End namespace Internal

} // End namespace Odbc

} // End namespace Databases

} // End namespace Safir

#endif // Safir_Databases_Odbc_Internal_Parameter_h
