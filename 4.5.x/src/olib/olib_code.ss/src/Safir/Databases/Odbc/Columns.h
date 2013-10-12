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
#if !defined(Safir_Databases_Odbc_Columns_h)
#define Safir_Databases_Odbc_Columns_h

#include "Safir/Databases/Odbc/Defs.h"
#include "Safir/Databases/Odbc/Internal/InternalDefs.h"
#include "Safir/Databases/Odbc/Internal/Column.h"
#include "Safir/Databases/Odbc/Internal/BinaryColumn.h"
#include "Safir/Databases/Odbc/Internal/WideStringColumn.h"

#include <Safir/Time/TimeProvider.h>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <Safir/Dob/Typesystem/Defs.h>

namespace Safir
{
namespace Databases
{
namespace Odbc
{

///////////////////////////////////////////////////
// Columns
///////////////////////////////////////////////////

class BooleanColumn : public Internal::Column<SQL_C_BIT, unsigned char>
{
public:
    void SetValue(bool bValue);
    bool GetValue() const;
};

class TimeColumn : public Internal::Column<SQL_C_TYPE_TIMESTAMP, TIMESTAMP_STRUCT>
{
public:
    void SetTimeStamp(const TIMESTAMP_STRUCT & tsTime);
    void SetValue(Safir::Dob::Typesystem::Si64::Second time);

    const TIMESTAMP_STRUCT & GetTimeStamp() const;
    TIMESTAMP_STRUCT GetTimeStamp();
    void GetValue(Safir::Dob::Typesystem::Si64::Second & time) const;
private:
    typedef Internal::Column<SQL_C_TYPE_TIMESTAMP, TIMESTAMP_STRUCT> InstantiatedColumn;
};

typedef Internal::BinaryColumn BinaryColumn;

typedef Internal::WideStringColumn WideStringColumn;

typedef Internal::Column<SQL_C_FLOAT, Safir::Dob::Typesystem::Float32> Float32Column;

typedef Internal::Column<SQL_C_DOUBLE, Safir::Dob::Typesystem::Float64> Float64Column;

typedef Internal::Column<SQL_C_SLONG, Safir::Dob::Typesystem::Int32> Int32Column;

typedef Internal::Column<SQL_C_SBIGINT, Safir::Dob::Typesystem::Int64> Int64Column;

/////////////////////////////////////////////////////////////////////////////////////////
// Method implementations - BooleanColumn
/////////////////////////////////////////////////////////////////////////////////////////

inline void BooleanColumn::SetValue(bool bValue)
{
    Internal::Column<SQL_C_BIT, unsigned char>::SetValue(bValue ? 1 : 0);
}

inline bool BooleanColumn::GetValue() const
{
    return Internal::Column<SQL_C_BIT, unsigned char>::GetValue() == 1;
}

/////////////////////////////////////////////////////////////////////////////////////////
// Method implementations - TimeColumn
/////////////////////////////////////////////////////////////////////////////////////////

inline void TimeColumn::SetTimeStamp(const TIMESTAMP_STRUCT & tsTime)
{
    InstantiatedColumn::SetValue( tsTime );
}

inline void TimeColumn::SetValue(Safir::Dob::Typesystem::Si64::Second time)
{
    boost::posix_time::ptime pTime;
    TIMESTAMP_STRUCT value;

    pTime = Safir::Time::TimeProvider::ToPtime( time );
    value.year = static_cast<SQLSMALLINT>(pTime.date().year());
    value.month = static_cast<SQLUSMALLINT>(pTime.date().month());
    value.day = static_cast<SQLUSMALLINT>(pTime.date().day());
    value.hour = static_cast<SQLUSMALLINT>(pTime.time_of_day().hours());
    value.minute = static_cast<SQLUSMALLINT>(pTime.time_of_day().minutes());
    value.second = static_cast<SQLUSMALLINT>(pTime.time_of_day().seconds());
    value.fraction = static_cast<SQLUINTEGER>(pTime.time_of_day().fractional_seconds());
    InstantiatedColumn::SetValue( value );
}

inline const TIMESTAMP_STRUCT & TimeColumn::GetTimeStamp() const
{
    return InstantiatedColumn::GetValue();
}

inline TIMESTAMP_STRUCT TimeColumn::GetTimeStamp()
{
    return InstantiatedColumn::GetValue();
}

inline void TimeColumn::GetValue(Safir::Dob::Typesystem::Si64::Second & time) const
{
    boost::gregorian::date pDate(m_value.year, m_value.month, m_value.day);
    boost::posix_time::time_duration pTimeDuration(m_value.hour, m_value.minute, m_value.second, m_value.fraction);
    boost::posix_time::ptime pTime(pDate, pTimeDuration);

    time = Safir::Time::TimeProvider::ToDouble( pTime );
}

};  // Odbc

};  // Databases

};  // Safir

#endif // Safir_Databases_Odbc_Columns_h
