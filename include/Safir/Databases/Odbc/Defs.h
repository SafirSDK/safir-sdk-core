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
#ifndef Safir_Databases_Odbc_Defs_h
#define Safir_Databases_Odbc_Defs_h

#ifndef SAFIR_NO_DEPRECATED


#ifndef SAFIR_DISABLE_OLIB_DEPRECATION_WARNING

//There is no standard way to give a warning on all compilers.

#if defined (_MSC_VER)
#  define STRINGIZE_HELPER(x) #x
#  define STRINGIZE(x) STRINGIZE_HELPER(x)
#  define WARNING(desc) message(__FILE__ "(" STRINGIZE(__LINE__) ") : warning: " #desc)
#  pragma WARNING(Olib, i.e. everything in the Safir::Databases::Odbc namespace, is deprecated! It will disappear soon!)

#elif defined (__GNUC__)
#  warning "Olib, i.e. everything in the Safir::Databases::Odbc namespace, is deprecated! It will disappear soon!"
#else
#  error "Olib, i.e. everything in the Safir::Databases::Odbc namespace, is deprecated! It will disappear soon!"
#endif

#endif

namespace Safir
{
namespace Databases
{
namespace Odbc
{

typedef char byte;

}

}

}

#endif

#endif // Safir_Databases_Odbc_Defs_h
