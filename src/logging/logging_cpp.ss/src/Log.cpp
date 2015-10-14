/******************************************************************************
*
* Copyright Saab AB, 2013 (http://safirsdkcore.com)
*
* Created by: Anders Widén
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
#include <Safir/Logging/Log.h>
#include <Safir/Logging/Internal/Interface.h>
#include <Safir/Utilities/Internal/StringEncoding.h>
#include <boost/cstdint.hpp>

namespace Safir 
{ 
namespace Logging
{

    using Safir::Utilities::Internal::ToUtf8;

    void SendSystemLog(const Severity       severity,
                       const std::wstring&  message)
    {
        LoggingC_SendSystemLog(static_cast<const boost::int32_t>(severity), ToUtf8(message).c_str());
    }
}
}
