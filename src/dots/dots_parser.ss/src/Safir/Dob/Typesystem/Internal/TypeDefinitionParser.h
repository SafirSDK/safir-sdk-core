/******************************************************************************
*
* Copyright Saab AB, 2004-2012 (http://www.safirsdk.com)
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
#ifndef __DOTS_TYPE_DEFINITION_PARSER_H__
#define __DOTS_TYPE_DEFINITION_PARSER_H__

#if defined _MSC_VER
    #if defined DOTS_PARSER_EXPORTS
        #define DOTS_API __declspec(dllexport)
    #else
        #define DOTS_API __declspec(dllimport)
        #define SAFIR_LIBRARY_NAME "dots_parser"
        #include <Safir/Utilities/Internal/AutoLink.h>
    #endif
#elif defined __GNUC__
    #define DOTS_API
    #define __cdecl
#endif

#include <boost/filesystem.hpp>
#include <boost/shared_ptr.hpp>
#include <Safir/Dob/Typesystem/Internal/ParseResult.h>
#include <Safir/Dob/Typesystem/Internal/TypeRepository.h>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{    
    /**
     * Parse and validate dou- and dom- files.
     * This class is immutable. All parsing and validation is made during construction and
     * after construction all operations are read-operations that will not modify the object.
     */
    class DOTS_API TypeDefinitionParser
    {
    public:
        /**
         * Constructor - Creates a TypeDefinitionParser object.
         * Will validate and parse a complete set of dou- and dom-files. If no error occurs, the
         * result can be retrieved by calling GetResult.
         *
         * @param definitions [in] - Root directory path to location of dou- and dom-files that shall be parsed.
         * @throws Safir::Dob::Typesystem::Parser:ParseError The dou- or dom- files at the specified path contains errors.
         */
        TypeDefinitionParser(const boost::filesystem::path& definitions);
       
        /**
         * Get the path that was parsed during construction of this object.
         *
         * @return Path to the dou- and dom- files.
         */
        boost::filesystem::path GetPath() const {return m_path;}

        /**
         * Get the result after a successfull call to Parse. Will get the raw parse result. In most cases its more convenient
         * to use the GetRepository-method.
         *         
         * @return All types, i.e classes, exceptions, enums, properties and property mappings.
         */
        boost::shared_ptr<const RawParseResult> GetRawResult() const {return m_result;}

        /**
         * Get the result after a successfull call to Parse. Will get the complete type system as a TypeRepository
         * that can be used in conjunction with the serializers and blob-builder stuff.
         *         
         * @return All types, i.e classes, exceptions, enums, properties and property mappings.
         */
        boost::shared_ptr<const TypeRepository> GetRepository() const;

        /**
         * Return string representation of the entire content of a RawParseResult.
         *
         * @param result [in] - RawParseResult.
         */
        static std::string ToString(boost::shared_ptr<const RawParseResult> rawResult);

        /**
         * Return string representation of the entire content of a TypeRepository.
         *
         * @param result [in] - TypeRepository.
         */
        static std::string ToString(boost::shared_ptr<const TypeRepository> repository);

    private:
        boost::filesystem::path m_path;
        boost::shared_ptr<RawParseResult> m_result;

        void Parse();
    };
}
}
}
} //end namespace Safir::Dob::Typesystem

#endif
