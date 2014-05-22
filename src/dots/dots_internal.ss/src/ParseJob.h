/******************************************************************************
*
* Copyright Saab AB, 2004-2013 (http://safir.sourceforge.net)
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
#ifndef __DOTS_INTERNAL_PARSE_JOB_H__
#define __DOTS_INTERNAL_PARSE_JOB_H__

#include <boost/filesystem.hpp>
#include <boost/property_tree/ptree.hpp>
#include <Safir/Dob/Typesystem/ToolSupport/ParseError.h>
#include "ParseState.h"

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace ToolSupport
{
    //-----------------------------------------------------------------------------------------
    //Parses a location with dou/dom-files.
    //Constructor will do all the work, throws ParseError on failure. Of success
    //result is collected by calling GetResult.
    //-----------------------------------------------------------------------------------------
    class ParseJob : public boost::noncopyable
    {
    public:        
        ParseJob(const std::vector<boost::filesystem::path>& roots);
        boost::shared_ptr<TypeRepository> GetResult() {return m_result;}

    private:
        boost::shared_ptr<RepositoryBasic> m_result;
    };
}
}
}
} //end namespace Safir::Dob::Typesystem

#endif
