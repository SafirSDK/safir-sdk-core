/******************************************************************************
*
* Copyright Saab AB, 2004-2008 (http://www.safirsdk.com)
* 
* Created by: Joel Ottosson / stjoot
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

#ifndef _dots_error_handler_h
#define _dots_error_handler_h

#include "dots_internal_defs.h"
#include <string>
#include <boost/filesystem.hpp>
namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    /**
     * Class for internal error reporting
     */
    class ErrorHandler
    {
    public:
        static void Information(const std::string & msg);

        static void Error(const std::string & label,
                          const std::string & description,
                          const boost::filesystem::path & location);

        static void Error(const std::string & label,
                          const std::string & description,
                          const boost::filesystem::path & filename,
                          const int linenumber,
                          const std::string & location);
    };
}
}
}
}
#endif
