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
#ifndef __DOTS_PARSER_BASIC_TYPES_H__
#define __DOTS_PARSER_BASIC_TYPES_H__

#include <string>
#include <map>
#include <boost/noncopyable.hpp>
#include <boost/function.hpp>
#include "ParseResult.h"
#include "InternalDefs.h"

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    class BasicTypes : public boost::noncopyable
    {
    public:

        static const BasicTypes& Instance();
        
        const std::string& StringOf(DotsC_MemberType type) const;
        Size SizeOfType(DotsC_MemberType type) const;        

        bool MemberTypeOf(const std::string& typeName, RawParseResultConstPtr res, DotsC_MemberType& memberType) const;
        bool CanParseValue(const std::string& typeName, const std::string& value, RawParseResultConstPtr res) const;
        //bool ParseValue(const std::string& typeName, RawParseResultConstPtr rawResult, ValueDefinition& val) const;


        //Built in object names
        static const std::string ObjectName;
        static const std::string ExceptionName;
        static const std::string FundamentalExceptionName;
        /*IllegalValueException
        IncompatibleTypesException
        ConfigurationErrorException
        SoftwareViolationException
        NullException
        ReadOnlyException*/

    private:
        BasicTypes(void);
        
        typedef boost::function<bool(const std::string&, RawParseResultConstPtr res)> ValueCheckerFunction;
        struct TypeInfo
        {
            DotsC_MemberType type;
            std::string name;
            ValueCheckerFunction valCheck;

            TypeInfo(DotsC_MemberType mt, const char* name, ValueCheckerFunction f) : type(mt), name(name), valCheck(f) {}
        };

        typedef std::vector<TypeInfo> TypeInfoVector;
        TypeInfoVector m_typeInfo;
        const TypeInfo* GetTypeInfo(DotsC_MemberType mt) const;
        const TypeInfo* GetTypeInfo(const std::string& typeName) const;
    };
}
}
}
} //end namespace Safir::Dob::Typesystem

#endif


