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
#ifndef Safir_Databases_Odbc_Input_Parameter_h
#define Safir_Databases_Odbc_Input_Parameter_h

#ifndef SAFIR_NO_DEPRECATED

#include "Safir/Databases/Odbc/Defs.h"
#include "Safir/Dob/Typesystem/Defs.h"
#include "Safir/Databases/Odbc/Internal/InternalDefs.h"
#include "Safir/Databases/Odbc/Internal/Parameter.h"
#include "Safir/Databases/Odbc/Internal/BufferedWideStringParameter.h"
#include "Safir/Databases/Odbc/Internal/NonBufferedWideStringParameter.h"
#include "Safir/Databases/Odbc/Internal/BufferedBinaryParameter.h"
#include "Safir/Databases/Odbc/Internal/NonBufferedBinaryParameter.h"

namespace Safir
{
namespace Databases
{
namespace Odbc
{

///////////////////////////////////////////////////
// Input parameters.
///////////////////////////////////////////////////

typedef Internal::BufferedBinaryParameter<SQL_VARBINARY, SQL_PARAM_INPUT> BinaryParameter;

typedef Internal::NonBufferedBinaryParameter<SQL_LONGVARBINARY, SQL_PARAM_INPUT> LongBinaryParameter;

typedef Internal::BufferedWideStringParameter<SQL_WVARCHAR, SQL_PARAM_INPUT> WideStringParameter;

typedef Internal::NonBufferedWideStringParameter<SQL_WLONGVARCHAR, SQL_PARAM_INPUT> LongWideStringParameter;

typedef Internal::BooleanParameter<SQL_PARAM_INPUT> BooleanParameter;

typedef Internal::TimeParameter<SQL_PARAM_INPUT> TimeParameter;

typedef Internal::Parameter<SQL_C_FLOAT, SQL_FLOAT, Safir::Dob::Typesystem::Float32,SQL_PARAM_INPUT,15> Float32Parameter;

typedef Internal::Parameter<SQL_C_DOUBLE, SQL_DOUBLE, Safir::Dob::Typesystem::Float64,SQL_PARAM_INPUT,15> Float64Parameter;

typedef Internal::Parameter<SQL_C_SLONG, SQL_INTEGER, Safir::Dob::Typesystem::Int32,SQL_PARAM_INPUT,10> Int32Parameter;

typedef Internal::Parameter<SQL_C_SBIGINT, SQL_BIGINT, Safir::Dob::Typesystem::Int64,SQL_PARAM_INPUT,20> Int64Parameter;

}  // Odbc

}  // Databases

}  // Safir

#endif
#endif //Safir_Databases_Odbc_Input_Parameter_h
