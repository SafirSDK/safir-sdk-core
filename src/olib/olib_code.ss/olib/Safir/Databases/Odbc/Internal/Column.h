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
#ifndef Safir_Databases_Odbc_Internal_Column_h
#define Safir_Databases_Odbc_Internal_Column_h

#include "Safir/Databases/Odbc/Defs.h"
#include "Safir/Databases/Odbc/Internal/InternalDefs.h"

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
template<short sValueType, class Type>
class Column : private boost::noncopyable
{
public:
    Column();
    Column(SQLLEN nSize);
    virtual ~Column();

    bool IsRetrievedSizeAvailable();
    SQLLEN GetRetrievedSize();

    void SetValue(const Type & value);
    void SetNull();

    Type & GetValue();
    const Type & GetValue() const;
    bool IsNull() const;

protected:
    Type m_value;
    SQLLEN m_lpLengthOrInd;
    SQLLEN m_nSize;   // in bytes

    static const short m_csValueType = sValueType;
    virtual void * GetValuePtr();
    SQLLEN * GetLengthOrIndPtr();
    SQLLEN GetSize();

    friend class Safir::Databases::Odbc::Statement;
};

/////////////////////////////////////////////////////////////////////////////////////////
// Method implementations - Column
/////////////////////////////////////////////////////////////////////////////////////////

template<short sValueType, class Type>
inline Column<sValueType, Type>::Column() : m_lpLengthOrInd(SQL_NULL_DATA), m_nSize(sizeof(Type))
{
}

template<short sValueType, class Type>
inline Column<sValueType, Type>::Column(SQLLEN nSize)
    : m_lpLengthOrInd(SQL_NULL_DATA),
      m_nSize(nSize)

{
}

template<short sValueType, class Type>
inline Column<sValueType, Type>::~Column()
{
}

template<short sValueType, class Type>
inline SQLLEN Column<sValueType, Type>::GetSize()
{
    return m_nSize;
}

template<short sValueType, class Type>
inline void * Column<sValueType, Type>::GetValuePtr()
{
    return &m_value;
}

template<short sValueType, class Type>
inline SQLLEN * Column<sValueType, Type>::GetLengthOrIndPtr()
{
    return &m_lpLengthOrInd;
}

template<short sValueType, class Type>
inline void Column<sValueType, Type>::SetValue(const Type & value)
{
    m_value = value;
    m_lpLengthOrInd = sizeof(Type);
}

template<short sValueType, class Type>
inline void Column<sValueType, Type>::SetNull()
{
    m_lpLengthOrInd = SQL_NULL_DATA;
}

template<short sValueType, class Type>
inline Type & Column<sValueType, Type>::GetValue()
{
    return m_value;
}

template<short sValueType, class Type>
inline const Type & Column<sValueType, Type>::GetValue() const
{
    return m_value;
}

template<short sValueType, class Type>
inline bool Column<sValueType, Type>::IsNull() const
{
    return m_lpLengthOrInd == SQL_NULL_DATA;
}

template<short sValueType, class Type>
bool Column<sValueType, Type>::IsRetrievedSizeAvailable()
{
    return ((m_lpLengthOrInd != SQL_NULL_DATA) && (m_lpLengthOrInd != SQL_NO_TOTAL));
}

template<short sValueType, class Type>
SQLLEN Column<sValueType, Type>::GetRetrievedSize()
{
    return m_lpLengthOrInd;
}


} // End namespace Internal

} // End namespace Odbc

} // End namespace Databases

} // End namespace Safir

#endif // Safir_Databases_Odbc_Internal_Column_h
