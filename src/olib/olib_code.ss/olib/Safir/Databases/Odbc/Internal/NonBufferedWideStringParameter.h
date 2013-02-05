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
#ifndef Safir_Databases_Odbc_Internal_NonBufferedWideStringParameter_h
#define Safir_Databases_Odbc_Internal_NonBufferedWideStringParameter_h

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
class NonBufferedWideStringParameter : public Parameter<SQL_C_WCHAR, sSqlType, std::wstring *, sInputOutputType, 0>
{
public:
    NonBufferedWideStringParameter(unsigned int nDbSize);
    void SetValue(std::wstring * strValue);

protected:
    virtual void * GetValuePtr();

private:
    typedef Parameter<SQL_C_WCHAR, sSqlType, std::wstring *, sInputOutputType, 0> InstantiatedParameter;
    friend class Safir::Databases::Odbc::Statement;
};

/////////////////////////////////////////////////////////////////////////////////////////
// Method implementations - NonBufferedWideStringParameter
/////////////////////////////////////////////////////////////////////////////////////////

template<short sSqlType, short sInputOutputType>
NonBufferedWideStringParameter<sSqlType,sInputOutputType>::NonBufferedWideStringParameter(unsigned int nDbSize)
    : InstantiatedParameter((nDbSize+1) * sizeof(wchar_t))
{
}

template<short sSqlType, short sInputOutputType>
inline void * NonBufferedWideStringParameter<sSqlType,sInputOutputType>::GetValuePtr()
{
    // The call to this function is BindParameter and there it is used only as Input.
    return const_cast<wchar_t *>(InstantiatedParameter::m_value->c_str());
}

template<short sSqlType, short sInputOutputType>
inline void NonBufferedWideStringParameter<sSqlType,sInputOutputType>::SetValue(std::wstring * strValue)
{
    InstantiatedParameter::m_value = strValue;
    InstantiatedParameter::m_nSize = static_cast<int>(strValue->size() * sizeof (wchar_t));
}

} // End namespace Internal

} // End namespace Odbc

} // End namespace Databases

} // End namespace Safir

#endif // Safir_Databases_Odbc_Internal_NonBufferedWideStringParameter_h
