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
#include <iostream>
#include <algorithm>
#include <Safir/Dob/Typesystem/Internal/TypeParser.h>
#include <Safir/Dob/Typesystem/Internal/detail/BlobToJsonSerializer.h>
#include "ParseJob.h"
#include "ElementParserDefs.h"

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    boost::shared_ptr<const TypeRepository> ParseTypeDefinitions(const boost::filesystem::path& definitions)
    {
        if (!boost::filesystem::is_directory(definitions))
        {
            throw ParseError("Invalid directory path", "The specified root directory does not exist.", definitions.string(), 8);
        }

        int cores=static_cast<int>(boost::thread::hardware_concurrency());
        ParseJob job(definitions, cores);
        return job.GetResult();
    }
}
}
}
} //end namespace Safir::Dob::Typesystem::Parser
