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
#ifndef Safir_Databases_Odbc_Internal_BinaryColumn_h
#define Safir_Databases_Odbc_Internal_BinaryColumn_h

#include "Safir/Databases/Odbc/Defs.h"
#include "Safir/Databases/Odbc/Internal/InternalDefs.h"
#include "Safir/Databases/Odbc/Internal/Column.h"

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
class BinaryColumn : public Internal::Column<SQL_C_BINARY, byte *>
{
public:
    inline BinaryColumn(unsigned int nDbSize);
    virtual ~BinaryColumn();

    inline void SetValue(const byte * byteString, unsigned int nSize);

protected:
    virtual void * GetValuePtr();

private:
    typedef Internal::Column<SQL_C_BINARY, byte *> InstantiatedColumn;
    boost::scoped_array<byte> m_wszValue;

    friend class Safir::Databases::Odbc::Statement;
};

/////////////////////////////////////////////////////////////////////////////////////////
// Method implementations - BinaryColumn
/////////////////////////////////////////////////////////////////////////////////////////

BinaryColumn::BinaryColumn(unsigned int nDbSize)
    : InstantiatedColumn((nDbSize) * sizeof(byte)),
      m_wszValue( new byte[nDbSize] )
{
    m_value = m_wszValue.get();
}

inline BinaryColumn::~BinaryColumn()
{
    m_wszValue.reset();
}

inline void * BinaryColumn::GetValuePtr()
{
    return m_wszValue.get();
}

inline void BinaryColumn::SetValue(const byte * byteString, unsigned int nSize)
{
    memcpy(m_wszValue.get(), byteString, nSize);
    m_lpLengthOrInd = nSize;
}

} // End namespace Internal

} // End namespace Odbc

} // End namespace Databases

} // End namespace Safir

#endif // Safir_Databases_Odbc_Internal_BinaryColumn_h
