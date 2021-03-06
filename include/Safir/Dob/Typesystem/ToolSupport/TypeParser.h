/******************************************************************************
*
* Copyright Saab AB, 2004-2015 (http://safirsdkcore.com)
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
* GNU General Public License for more Internals.
*
* You should have received a copy of the GNU General Public License
* along with Safir SDK Core.  If not, see <http://www.gnu.org/licenses/>.
*
******************************************************************************/
#ifndef __DOTS_INTERNAL_TYPE_PARSER_H__
#define __DOTS_INTERNAL_TYPE_PARSER_H__

#include <boost/filesystem.hpp>
#include <boost/shared_ptr.hpp>
#include <Safir/Dob/Typesystem/ToolSupport/Internal/InternalExportDefs.h>
#include <Safir/Dob/Typesystem/ToolSupport/ParseError.h>
#include <Safir/Dob/Typesystem/ToolSupport/TypeRepository.h>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace ToolSupport
{

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4275)
#pragma warning (disable: 4251)
#endif

    /**
     * Will validate and parse a complete set of dou- and dom-files from If no error occurs, the
     * result is returned. The root path will be parsed recursively, i.e any subfolders will be parsed too.
     *
     * @param root [in] - Root directory path to location of dou- and dom-files that shall be parsed.
     * @return TypeRepository containing all types, i.e classes, exceptions, enums, properties and property mappings.
     * @throws Safir::Dob::Typesystem::Parser:ParseError The dou- or dom- files at the specified path contains errors.
     */
    DOTS_INTERNAL_API boost::shared_ptr<const TypeRepository> ParseTypeDefinitions(const boost::filesystem::path& root);

    /**
     * Will validate and parse a complete set of dou- and dom-files. If no error occurs, the
     * result is returned.
     * This version takes a vector of paths and each path is traversed recursively includeing subfolders. If the same
     * dou-/dom- file exists at many paths the latest found will override the previous ones. I.e files found at paths
     * closer to the end of the vector will override files closer to the beginning of the vector.
     *
     * @param roots [in] - A vector of root directories containing dou- and dom-files that shall be parsed.
     * @return TypeRepository containing all types, i.e classes, exceptions, enums, properties and property mappings.
     * @throws Safir::Dob::Typesystem::Parser:ParseError The dou- or dom- files at the specified path contains errors.
     */
    DOTS_INTERNAL_API boost::shared_ptr<const TypeRepository> ParseTypeDefinitions(const std::vector<boost::filesystem::path>& roots);


#ifdef _MSC_VER
#pragma warning (pop)
#endif

}
}
}
} //end namespace Safir::Dob::Typesystem::ToolSupport

#endif
