/******************************************************************************
*
* Copyright Saab AB, 2004-2013 (http://www.safirsdk.com)
*
* Created by: Joel Ottosson / joot
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
#ifndef __DOTS_INTERNAL_TYPEDEFINITIONPARSER_H__
#define __DOTS_INTERNAL_TYPEDEFINITIONPARSER_H__

#include <boost/filesystem.hpp>
#include <boost/shared_ptr.hpp>
#include <Safir/Dob/Typesystem/Internal/ParseError.h>
#include <Safir/Dob/Typesystem/Internal/TypeRepository.h>

#if defined _MSC_VER
    #if defined DOTS_INTERNAL_EXPORTS
        #define DOTS_API __declspec(dllexport)
    #else
        #define DOTS_API __declspec(dllimport)
        #define SAFIR_LIBRARY_NAME "dots_internal"
        #include <Safir/Utilities/Internal/AutoLink.h>
    #endif
#elif defined __GNUC__
    #define DOTS_API
    #define __cdecl
#endif

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4275)
#pragma warning (disable: 4251)
#endif

    /**
     * Will validate and parse a complete set of dou- and dom-files. If no error occurs, the
     * result is returned.
     *
     * @param definitions [in] - Root directory path to location of dou- and dom-files that shall be parsed.
     * @return TypeRepository containing all types, i.e classes, exceptions, enums, properties and property mappings.
     * @throws Safir::Dob::Typesystem::Parser:ParseError The dou- or dom- files at the specified path contains errors.
     */
    DOTS_API boost::shared_ptr<const TypeRepository> ParseTypeDefinitions(const boost::filesystem::path& definitions);

#ifdef _MSC_VER
#pragma warning (pop)
#endif

}
}
}
} //end namespace Safir::Dob::Typesystem::Internal

#endif
