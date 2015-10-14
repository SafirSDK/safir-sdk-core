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
#ifndef Safir_Databases_Odbc_Internal_BufferedBinaryParameter_h
#define Safir_Databases_Odbc_Internal_BufferedBinaryParameter_h

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
class BufferedBinaryParameter : public Parameter<SQL_C_BINARY, sSqlType, byte *, sInputOutputType, 0>
{
public:
    BufferedBinaryParameter(unsigned int nDbSize);
    virtual ~BufferedBinaryParameter();

    void SetValue(const byte *byteString, unsigned int nSize);
protected:
    virtual void * GetValuePtr();
    virtual unsigned long GetColumnSize();

private:
    typedef Parameter<SQL_C_BINARY, sSqlType, byte *, sInputOutputType, 0> InstantiatedParameter;
    boost::scoped_array<byte> m_wszValue;
    unsigned int m_nCurrentSize;
    friend class Safir::Databases::Odbc::Statement;
};

/////////////////////////////////////////////////////////////////////////////////////////
// Method implementations - BufferedBinaryParameter
/////////////////////////////////////////////////////////////////////////////////////////

template<short sSqlType, short sInputOutputType>
BufferedBinaryParameter<sSqlType,sInputOutputType>::BufferedBinaryParameter(unsigned int nDbSize)
    :InstantiatedParameter(nDbSize * sizeof(byte)), 
     m_wszValue( new byte[nDbSize] ), m_nCurrentSize(0)
    
{
    InstantiatedParameter::m_value = m_wszValue.get();
}

template<short sSqlType, short sInputOutputType>
inline BufferedBinaryParameter<sSqlType,sInputOutputType>::~BufferedBinaryParameter()
{
    m_wszValue.reset();
}

template<short sSqlType, short sInputOutputType>
inline void * BufferedBinaryParameter<sSqlType,sInputOutputType>::GetValuePtr()
{
    return m_wszValue.get();
}

template<short sSqlType, short sInputOutputType>
inline void BufferedBinaryParameter<sSqlType,sInputOutputType>::SetValue(const byte *byteString,
                                                                         unsigned int nSize)
{
    memcpy(m_wszValue.get(),byteString, nSize);
    InstantiatedParameter::m_lpLengthOrInd = nSize;
    m_nCurrentSize = nSize;
}

template<short sSqlType, short sInputOutputType>
inline unsigned long BufferedBinaryParameter<sSqlType,sInputOutputType>::GetColumnSize()
{
    return m_nCurrentSize; // Do not count end-of-string char in column size
}

} // End namespace Internal

} // End namespace Odbc

} // End namespace Databases

} // End namespace Safir

#endif // Safir_Databases_Odbc_Internal_BufferedBinaryParameter_h
