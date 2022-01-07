/******************************************************************************
*
* Copyright Saab AB, 2022 (http://safirsdkcore.com)
*
* Created by: Lars Hagström / lars@foldspace.nu
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
#pragma once

#include <boost/system/error_code.hpp>
#include <sstream>
#include <system_error>


namespace boost
{
namespace system
{
    //Since this is an overload for a boost class we need to put it in the boost namespace.
    static inline std::wostream& operator<<(std::wostream& out, const boost::system::error_code& error)
    {
        std::ostringstream tmp;
        tmp << error;
        return out << tmp.str().c_str();
    }

}
}

//It is illegal to put this in the namespace where it belongs (std), so we put it in the
//global namespace and hope for the best.
static inline std::wostream& operator<<(std::wostream& out, const std::error_code& error)
{
    std::ostringstream tmp;
    tmp << error;
    return out << tmp.str().c_str();
}
