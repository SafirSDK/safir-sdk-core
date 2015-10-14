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
#ifndef Safir_Databases_Odbc_Internal_WideStringColumn_h
#define Safir_Databases_Odbc_Internal_WideStringColumn_h

#include "Safir/Databases/Odbc/Defs.h"
#include "Safir/Databases/Odbc/Internal/InternalDefs.h"
#include "Safir/Databases/Odbc/Internal/Column.h"

#include <boost/noncopyable.hpp>
#include <boost/scoped_array.hpp>
#include <string>

namespace Safir
{
namespace Databases
{
namespace Odbc
{
class Statement;

namespace Internal
{

class WideStringColumn : public Internal::Column<SQL_C_WCHAR, wchar_t *>
{
public:
    inline WideStringColumn(unsigned int nDbSize);
    virtual ~WideStringColumn();

    inline void SetValue(const wchar_t *wszvalue, unsigned int nCharacters);
    inline void SetValue(const std::wstring & strValue);

protected:
    virtual void * GetValuePtr();

private:
    typedef Internal::Column<SQL_C_WCHAR, wchar_t *> InstantiatedColumn;
    boost::scoped_array<wchar_t> m_wszValue;
    unsigned int m_nCharacters;

    friend class Safir::Databases::Odbc::Statement;
};
/////////////////////////////////////////////////////////////////////////////////////////
// Method implementations - WideStringColumn
/////////////////////////////////////////////////////////////////////////////////////////

WideStringColumn::WideStringColumn(unsigned int nDbSize)
    : InstantiatedColumn((nDbSize+1) * sizeof(wchar_t)),
      m_wszValue( new wchar_t[nDbSize+1] ), m_nCharacters (nDbSize+1)
{
    m_value =  m_wszValue.get();
}

inline WideStringColumn::~WideStringColumn()
{
    m_wszValue.reset();
}

inline void * WideStringColumn::GetValuePtr()
{
    return m_wszValue.get();
}

inline void WideStringColumn::SetValue(const wchar_t *wszValue, unsigned int nCharacters)
{
    if (nCharacters >= m_nCharacters)
    {
        wmemcpy(m_wszValue.get(), wszValue, m_nCharacters);
        m_value[m_nCharacters-1] = '\0';
    }
    else
    {
        wmemcpy(m_wszValue.get(), wszValue, nCharacters );
        m_value[nCharacters] = '\0';
    }
    m_lpLengthOrInd = SQL_NTS;
}

inline void WideStringColumn::SetValue(const std::wstring & strValue)
{
    if (strValue.size() >= m_nCharacters)
    {
        wmemcpy(m_wszValue.get(),strValue.c_str(), m_nCharacters);
        m_value[m_nCharacters-1] = L'\0';
    }
    else
    {
        wmemcpy(m_wszValue.get(), strValue.c_str(), strValue.size() );
        m_value[strValue.size()] = L'\0';
    }
    m_lpLengthOrInd = SQL_NTS;
}


} // End namespace Internal

} // End namespace Odbc

} // End namespace Databases

} // End namespace Safir

#endif // Safir_Databases_Odbc_Internal_WideStringColumn_h
