/******************************************************************************
*
* Copyright Saab AB, 2006-2013 (http://safirsdkcore.com)
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
#include <DoseStressTest/EntityWithAck.h>
#include <DoseStressTest/EntityWithoutAck.h>
#include <DoseStressTest/EntityWithAckLarge.h>
#include <DoseStressTest/EntityWithoutAckLarge.h>

void Subscriber::PrintStatistics()
{
    const boost::int64_t missed = std::count(m_received.begin(),m_received.end(),false);
    std::wcout << "Received "
               << CommandLine::Instance().NumSets() - missed
               << " entity creates/updates, missed "
               << missed
               << " ("
               << ((float)missed)/CommandLine::Instance().NumSets() * 100.0
               << "%)." <<std::endl;

    if (missed != 0 && CommandLine::Instance().PrintMisses())
    {
        std::wcout << "Missed the following updates: " <<std::endl;
        for (std::deque<bool>::iterator it = m_received.begin();
             it != m_received.end(); ++it)
        {
            if (!(*it))
            {
                std::wcout << (boost::int64_t)std::distance(m_received.begin(),it) << " ";
            }
        }
        std::wcout << std::endl;
    }
}



Subscriber::Subscriber():
    m_received(CommandLine::Instance().NumSets(),false)
{
    m_connection.Attach();

    namespace sdt = Safir::Dob::Typesystem;
    m_connection.SubscribeEntity(DoseStressTest::RootEntity::ClassTypeId,
                                 this);
}

void Subscriber::HandleEntity(const Safir::Dob::EntityProxy& entityProxy)
{
    DoseStressTest::EntityWithAckPtr rootEnt =
        boost::static_pointer_cast<DoseStressTest::EntityWithAck>(entityProxy.GetEntity());
    m_received[rootEnt->SequenceNumber()] = true;
}
