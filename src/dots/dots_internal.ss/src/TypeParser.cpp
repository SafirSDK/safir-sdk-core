/******************************************************************************
*
* Copyright Saab AB, 2004-2015 (http://safir.sourceforge.net)
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
#include <Safir/Dob/Typesystem/ToolSupport/TypeParser.h>
#include "ParseJob.h"

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace ToolSupport
{
    boost::shared_ptr<const TypeRepository> ParseTypeDefinitions(const boost::filesystem::path& root)
    {
        std::vector<boost::filesystem::path> roots;
        roots.push_back(root);
        return ParseTypeDefinitions(roots);
    }

    boost::shared_ptr<const TypeRepository> ParseTypeDefinitions(const std::vector<boost::filesystem::path>& roots)
    {
        for (size_t i=0; i<roots.size(); ++i)
        {
            //Check paths are valid directories. We dont care about duplicates, that will only result in overriding with same file
            if (!boost::filesystem::exists(roots[i]) || !boost::filesystem::is_directory(roots[i]))
            {
                throw ParseError("Invalid directory path", "The specified root directory does not exist.", roots[i].string(), 8);
            }
        }

        ParseJob job(roots);
        return job.GetResult();
    }
}
}
}
} //end namespace Safir::Dob::Typesystem::ToolSupport
