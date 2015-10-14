/******************************************************************************
*
* Copyright Saab AB, 2012-2013 (http://safirsdkcore.com)
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
#include "ConnectionKiller.h"

#include <Safir/Dob/Internal/Connections.h>
#include <boost/bind.hpp>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    namespace
    {
        void Kill(const ConnectionPtr& connection)
        {
            if (connection->IsLocal() && connection->Pid() != Safir::Utilities::ProcessInfo::GetPid())
            {
                connection->Died();
                connection->SendStopOrder();
            }
        }
    }

    ConnectionKiller::~ConnectionKiller()
    {
        Connections::Instance().ForEachConnectionPtr(Kill);
    }
}
}
}
