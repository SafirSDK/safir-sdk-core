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

#include "dots_error_handler.h"

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

    void ErrorHandler::Information(const std::string & msg)
    {
        std::wcout<<"========== DOTS_KERNEL: information =========="<<std::endl;
        std::wcout<<msg.c_str()<<std::endl;
        std::wcout<<"==================== END ====================="<<std::endl<<std::endl;
    }

    void ErrorHandler::Error(const std::string & label,
                             const std::string & description,
                             const boost::filesystem::path & location)
                             //                             const std::string & severity)
    {
        std::wcout<<"============= DOTS_KERNEL: error ============="<<std::endl;
        std::wcout<<"Label:       "<<label.c_str()<<std::endl;
        std::wcout<<"Location:    "<<location.string().c_str()<<std::endl;
        std::wcout<<"Description: "<<description.c_str()<<std::endl;
        std::wcout<<"==================== END ====================="<<std::endl<<std::endl;
    }

    void ErrorHandler::Error(const std::string & label,
                             const std::string & description,
                             const boost::filesystem::path & filename,
                             const int linenumber,
                             const std::string & /*location*/)
                             //                             const std::string & severity)
    {
        std::wcout << filename.string().c_str() << ":" << linenumber << ": " << label.c_str() << ": " << description.c_str() << std::endl;
/*        std::wcout<<"============= DOTS_KERNEL: error ============="<<std::endl;
        std::wcout<<"Label:       "<<label.c_str()<<std::endl;
        std::wcout<<"Location:    "<<location.c_str()<<std::endl;
        std::wcout<<"Description: "<<description.c_str()<<std::endl;
        std::wcout<<"File name:   "<<filename.string().c_str()<<std::endl;
        std::wcout<<"Line number: "<<linenumber<<std::endl;
        std::wcout<<"==================== END ====================="<<std::endl<<std::endl;*/
    }
}
}
}
}
