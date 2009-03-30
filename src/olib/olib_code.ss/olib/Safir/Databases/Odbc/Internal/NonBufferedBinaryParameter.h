/******************************************************************************
*
* Copyright Saab AB, 2005-2008 (http://www.safirsdk.com)
*
* Created by: J�rgen Johansson / stjrjo
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
#ifndef Safir_Databases_Odbc_Internal_NonBufferedBinaryParameter_h
#define Safir_Databases_Odbc_Internal_NonBufferedBinaryParameter_h

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
class NonBufferedBinaryParameter : public Parameter<SQL_C_BINARY, sSqlType, byte *, sInputOutputType, 0>
{
public:

    NonBufferedBinaryParameter(unsigned int nDbSize);
    void SetValue(byte *byteString, unsigned int nSize);
protected:

    virtual void * GetValuePtr();
    virtual unsigned long GetColumnSize();

private:
    typedef Parameter<SQL_C_BINARY, sSqlType, byte *, sInputOutputType, 0> InstantiatedParameter;
    friend class Safir::Databases::Odbc::Statement;
    unsigned int m_nCurrentSize;
};

/////////////////////////////////////////////////////////////////////////////////////////
// Method implementations - NonBufferedBinaryParameter
/////////////////////////////////////////////////////////////////////////////////////////

template<short sSqlType, short sInputOutputType>
NonBufferedBinaryParameter<sSqlType,sInputOutputType>::NonBufferedBinaryParameter(unsigned int nDbSize)
    : InstantiatedParameter((nDbSize+1) * sizeof(byte)), m_nCurrentSize(0)
{
}

template<short sSqlType, short sInputOutputType>
inline void * NonBufferedBinaryParameter<sSqlType,sInputOutputType>::GetValuePtr()
{
    // The call to this function is BindParameter and there it is used only as Input.
    return const_cast<byte *>(InstantiatedParameter::m_value);
}

template<short sSqlType, short sInputOutputType>
inline void NonBufferedBinaryParameter<sSqlType,sInputOutputType>::SetValue(byte *byteString,
                                                                            unsigned int nSize)
{
    InstantiatedParameter::SetValue( byteString );
    m_nCurrentSize = nSize;
    InstantiatedParameter::m_lpLengthOrInd = nSize;
}

template<short sSqlType, short sInputOutputType>
inline unsigned long NonBufferedBinaryParameter<sSqlType,sInputOutputType>::GetColumnSize()
{
    return m_nCurrentSize;
}

} // End namespace Internal

} // End namespace Odbc

} // End namespace Databases

} // End namespace Safir

#endif // Safir_Databases_Odbc_Internal_NonBufferedBinaryParameter_h
