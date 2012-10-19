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
/* Not used? Removed by LAHA
#if defined _MSC_VER
   #if (_MSC_VER >= 1500)
       #include <unordered_map>
       using std::tr1::unordered_map;
    #else
       #include <hash_map>
       #define unordered_map stdext::hash_map
    #endif
#elif defined __GNUC__
   #include <tr1/unordered_map>
   using std::tr1::unordered_map;

//The int64 hash is not predefined on g++ 4.1, we assume it is in > 4.2
#if (__GNUC_MINOR__ < 2)
namespace std {namespace tr1 {

    template<>
    struct hash<long long int>
    {
        size_t
        operator()(const long long int v) const
        { return static_cast<size_t>(v);}
    };

}}
#endif

#endif
*/
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
     */
    class DOTS_API TypeDefinitionParser
    {
    public:
        /**
         * Default constructor.
         *
         * Creates a TypeDefinitionParser object.
         */
        TypeDefinitionParser(void);
       
        /**
         * Validate and parse a complete set of dou- and dom-files. If the function returns without errors, the 
         * result can be retrieved by calling GetResult.
         *
         * @param definitions [in] - Root directory path to location of dou- and dom-files that shall be parsed.
         * @return True if no error occured, else false.
         * @throws Safir::Dob::Typesystem::Parser:ParseError The dou- or dom- files att specified path contains errors.
         */
        void Parse(const boost::filesystem::path& definitions);

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
        boost::shared_ptr<RawParseResult> m_result;        
    };
}
}
}
} //end namespace Safir::Dob::Typesystem

#endif
