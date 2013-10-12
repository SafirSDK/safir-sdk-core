/******************************************************************************
*
* Copyright Saab AB, 2012-2013 (http://safir.sourceforge.net)
*
* Created by: Lars Hagström / lars@foldspace.nu
*
******************************************************************************/
#include "dose_main_connection_killer.h"

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



