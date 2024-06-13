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
#ifndef Safir_Databases_Odbc_Internal_BufferedWideStringParameter_h
#define Safir_Databases_Odbc_Internal_BufferedWideStringParameter_h

#include "Safir/Databases/Odbc/Defs.h"
#include "Safir/Dob/Typesystem/Defs.h"
#include "Safir/Databases/Odbc/Internal/InternalDefs.h"
#include "Safir/Databases/Odbc/Internal/Parameter.h"

namespace Safir
{
namespace Databases
{
namespace Odbc
{
class Statement;

namespace Internal
{

template<short sSqlType, short sInputOutputType>
class BufferedWideStringParameter : public Parameter<SQL_C_WCHAR, sSqlType, wchar_t *, sInputOutputType, 0>
{
public:
    BufferedWideStringParameter(unsigned int nDbSize);
    virtual ~BufferedWideStringParameter();

    void SetValue(const wchar_t *wszvalue, unsigned int nCharacters);
    void SetValue(const std::wstring & strValue);

protected:
    virtual void * GetValuePtr();
    virtual unsigned long GetColumnSize();

private:
    typedef Parameter<SQL_C_WCHAR, sSqlType, wchar_t *, sInputOutputType, 0> InstantiatedParameter;
    boost::scoped_array<wchar_t> m_wszValue;
    unsigned int m_nCharacters;
    friend class Safir::Databases::Odbc::Statement;
};

/////////////////////////////////////////////////////////////////////////////////////////
// Method implementations - BufferedWideStringParameter
/////////////////////////////////////////////////////////////////////////////////////////

template<short sSqlType, short sInputOutputType>
BufferedWideStringParameter<sSqlType,sInputOutputType>::BufferedWideStringParameter(unsigned int nDbSize)
    : InstantiatedParameter((nDbSize+1) * sizeof(wchar_t)),
      m_wszValue( new wchar_t[nDbSize+1] ), m_nCharacters(nDbSize+1)

{
    InstantiatedParameter::m_value = m_wszValue.get();
}

template<short sSqlType, short sInputOutputType>
inline BufferedWideStringParameter<sSqlType,sInputOutputType>::~BufferedWideStringParameter()
{
    m_wszValue.reset();
}

template<short sSqlType, short sInputOutputType>
inline void * BufferedWideStringParameter<sSqlType,sInputOutputType>::GetValuePtr()
{
    return m_wszValue.get();
}

template<short sSqlType, short sInputOutputType>
inline void BufferedWideStringParameter<sSqlType,sInputOutputType>::SetValue(const wchar_t *wszValue,
                                                                             unsigned int nCharacters)
{
    if (nCharacters >= m_nCharacters)
    {
        wmemcpy(m_wszValue.get(),wszValue, m_nCharacters);
        m_wszValue[m_nCharacters-1] = L'\0';
    }
    else
    {
        wmemcpy(m_wszValue.get(), wszValue, nCharacters );
        m_wszValue[nCharacters] = L'\0';
    }
    InstantiatedParameter::m_lpLengthOrInd = SQL_NTS;
}

template<short sSqlType, short sInputOutputType>
inline void BufferedWideStringParameter<sSqlType,sInputOutputType>::SetValue(const std::wstring & strValue)
{
    if (strValue.size() >= m_nCharacters)
    {
        wmemcpy(m_wszValue.get(),strValue.c_str(), m_nCharacters);
        m_wszValue[m_nCharacters-1] = L'\0';
    }
    else
    {
        wmemcpy(m_wszValue.get(), strValue.c_str(), strValue.size() );
        m_wszValue[strValue.size()] = L'\0';
    }
    InstantiatedParameter::m_lpLengthOrInd = SQL_NTS;
}

template<short sSqlType, short sInputOutputType>
inline unsigned long BufferedWideStringParameter<sSqlType,sInputOutputType>::GetColumnSize()
{
    return static_cast<unsigned long>(InstantiatedParameter::m_nSize - sizeof(wchar_t)); // Do not count end-of-string char in column size
}

} // End namespace Internal

} // End namespace Odbc

} // End namespace Databases

} // End namespace Safir

#endif // Safir_Databases_Odbc_Internal_BufferedWideStringParameter_h
