/******************************************************************************
*
* Copyright Saab AB, 2006-2013 (http://safir.sourceforge.net)
*
* Created by: Lars Hagström / stlrha
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

#include "Subscriber.h"
#include "CommandLine.h"
#include <DoseIntegrityCheck/EntityWithAck.h>
#include <DoseIntegrityCheck/EntityWithoutAck.h>

Subscriber::Subscriber():
    m_numSubscriptions(0)
{
    m_connection.Attach();

    namespace sdt = Safir::Dob::Typesystem;
    m_connection.SubscribeEntity(DoseIntegrityCheck::RootEntity::ClassTypeId,
                                 this);
}

void Subscriber::HandleEntity(const Safir::Dob::EntityProxy& entityProxy)
{
    const DoseIntegrityCheck::RootEntityConstPtr rootEnt =
        boost::static_pointer_cast<DoseIntegrityCheck::RootEntity>(entityProxy.GetEntity());
    if (rootEnt->Data().GetVal().empty() && rootEnt->Checksum().IsNull())
    {
        //ok
    }
    else if (Safir::Dob::Typesystem::Operations::GetTypeId(rootEnt->Data().GetVal()) == rootEnt->Checksum().GetVal())
    {
        //ok
    }
    else
    {
        std::wcout << "Got bad entity " << entityProxy.GetEntityId()
            << ": Data = '" << rootEnt->Data().GetVal() << "'\n"<<std::endl;
        exit(0);
    }
    ++m_numSubscriptions;
    if (m_numSubscriptions % 1000 == 0)
    {
        std::wcout << "Number of Subscription responses = " << m_numSubscriptions << std::endl;
    }
}
