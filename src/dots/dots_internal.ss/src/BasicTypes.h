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
#ifndef __DOTS_INTERNAL_BASIC_TYPES_H__
#define __DOTS_INTERNAL_BASIC_TYPES_H__

#include <string>
#include <map>
#include <vector>
#include <boost/noncopyable.hpp>
#include <boost/unordered_map.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/function.hpp>
#include <boost/bind.hpp>
#include <Safir/Dob/Typesystem/Internal/KernelDefs.h>
#include <Safir/Dob/Typesystem/Internal/ParseError.h>
#include "InternalDefs.h"
#include "classic_string_cast.h"

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

        bool IsBasicType(const std::string& typeName, DotsC_MemberType& memberType) const;                
        bool ParseValue(DotsC_MemberType memberType, const std::string& val, ValueDefinition& result) const;

        static std::string ToBase64(const std::string& bin);
        static bool FromBase64(std::string base64, std::string& bin);

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

//        typedef boost::function<bool(const std::string& str, ValueDefinition& val)> FromStringConversion;
//        typedef boost::function<bool(const ValueDefinition& val, std::string& str)> ToStringConversion;

//        struct TypeInfo
//        {
//            std::string name;
//            DotsC_MemberType memberType;
//            size_t size;
//            FromStringConversion fromString;
//            ToStringConversion toString;

//            TypeInfo(const std::string& name_, DotsC_MemberType mt_, size_t size_, FromStringConversion fs_, ToStringConversion ts_)
//                :name(name_)
//                ,memberType(mt_)
//                ,size(size_)
//                ,fromString(fs_)
//                ,toString(ts_)
//            {
//            }
//        };

        boost::unordered_map<std::string, DotsC_MemberType> m_nameToMt;
        boost::unordered_map<DotsC_MemberType, std::string> m_mtToName;

        const std::string* MemberTypeToString(DotsC_MemberType mt) const;
        const DotsC_MemberType* StringToMemberType(const std::string& typeName) const;
    };
}
}
}
} //end namespace Safir::Dob::Typesystem

#endif


