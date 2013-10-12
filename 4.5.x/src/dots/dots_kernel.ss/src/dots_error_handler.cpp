/******************************************************************************
*
* Copyright Saab AB, 2004-2013 (http://safir.sourceforge.net)
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

#include "dots_error_handler.h"
#include <Safir/Utilities/Internal/SystemLog.h>
#include <Safir/Utilities/Internal/StringEncoding.h>

#include <iostream>
#include <string>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{

    using Safir::Utilities::Internal::ToUtf16;

    void ErrorHandler::Information(const std::string & msg)
    {
        std::ostringstream ostr;

        ostr << "========== DOTS_KERNEL: information ==========" << std::endl
             << msg.c_str() << std::endl
             << "==================== END =====================" <<std::endl << std::endl;

        std::wstring str = ToUtf16(ostr.str());
        std::wcout << str;
        Safir::Utilities::Internal::SystemLog().Send(Safir::Utilities::Internal::SystemLog::Alert,
                                                     str);

    }

    void ErrorHandler::Error(const std::string & label,
                             const std::string & description,
                             const boost::filesystem::path & location)
                             //                             const std::string & severity)
    {
        std::ostringstream ostr;

        ostr << "============= DOTS_KERNEL: error =============" << std::endl
             << "Label:       " << label.c_str() << std::endl
             << "Location:    " << location.string().c_str() << std::endl
             << "Description: " << description.c_str() << std::endl
             << "==================== END =====================" << std::endl << std::endl;

        std::wstring str = ToUtf16(ostr.str());

        std::wcout << str;
        Safir::Utilities::Internal::SystemLog().Send(Safir::Utilities::Internal::SystemLog::Alert,
                                                     str);
    }

    void ErrorHandler::Error(const std::string & label,
                             const std::string & description,
                             const boost::filesystem::path & filename,
                             const int linenumber,
                             const std::string & /*location*/)
                             //                             const std::string & severity)
    {
        std::ostringstream ostr;

        ostr << filename.string().c_str() << ":" << linenumber << ": " << label.c_str() << ": " << description.c_str() << std::endl;

        std::wstring str = ToUtf16(ostr.str());

        std::wcout << str;
        Safir::Utilities::Internal::SystemLog().Send(Safir::Utilities::Internal::SystemLog::Alert,
                                                     str);
    }
}
}
}
}
