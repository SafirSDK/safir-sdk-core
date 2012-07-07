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
#include <Safir/Dob/Typesystem/ParseResult.h>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Parser
{
    class TypeChecker : public boost::noncopyable
    {
    public:
        static const TypeChecker& Instance();
        bool IsType(const std::string& typeName, const ParseResult& res) const;
        bool CanParseValue(const std::string& typeName, const std::string value, const ParseResult& res) const;

    private:
        TypeChecker(void);
        typedef boost::function<bool(const std::string&, const ParseResult& res)> ValueCheckerFunction;
        typedef std::map<std::string, ValueCheckerFunction> BasicTypeMap;
        BasicTypeMap m_basicTypes;
    };
}
}
}
} //end namespace Safir::Dob::Typesystem

#endif


