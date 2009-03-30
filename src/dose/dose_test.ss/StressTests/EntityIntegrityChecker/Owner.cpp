/******************************************************************************
*
* Copyright Saab AB, 2006-2008 (http://www.safirsdk.com)
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

#include "Owner.h"
#include "CommandLine.h"
#include <Safir/Dob/OverflowException.h>
#include <DoseIntegrityCheck/EntityWithAck.h>
#include <DoseIntegrityCheck/EntityWithoutAck.h>
#include <Safir/Dob/NodeParameters.h>
#include <Safir/Dob/ThisNodeParameters.h>
#include <sstream>

Safir::Dob::Typesystem::TypeId WhichType()
{
    if (CommandLine::Instance().Ack())
    {
        return DoseIntegrityCheck::EntityWithAck::ClassTypeId;
    }
    else
    {
        return DoseIntegrityCheck::EntityWithoutAck::ClassTypeId;
    }
}

Owner::Owner():
    m_typeId(WhichType()),
    m_numInstances(CommandLine::Instance().NumInstances()),
    m_instancesPerNode(m_numInstances / Safir::Dob::NodeParameters::NodesArraySize()),
    m_myFirst(Safir::Dob::ThisNodeParameters::NodeNumber() * m_instancesPerNode),
    m_numUpdates(0),
    m_nextInstance(m_myFirst),
    m_handler(Safir::Dob::ThisNodeParameters::NodeNumber())
{
    m_connection.Attach();

    if (CommandLine::Instance().Ack())
    {
        m_entity = DoseIntegrityCheck::EntityWithAck::Create();
    }
    else
    {
        m_entity = DoseIntegrityCheck::EntityWithoutAck::Create();
    }

    std::wcout << "This owner will use these instances: " << m_myFirst << " to " << m_myFirst + m_instancesPerNode - 1 << std::endl;

    m_connection.RegisterEntityHandler(m_entity->GetTypeId(),m_handler,Safir::Dob::InstanceIdPolicy::HandlerDecidesInstanceId,this);
}


const std::wstring & Owner::GenerateString(const int instance) const
{
    static std::wstring str; //keep it around to avoid a copy and allow return of ref.
    str.clear();
    const unsigned int desiredLength = rand() % DoseIntegrityCheck::RootEntity::DataMaxStringLength();
    for(int i = 0;; ++i)
    {
        std::wostringstream nextBit;
        nextBit << "Ix="
                << i
                << ";Instance="
                << instance
                << ";Update="
                << m_numUpdates
                << " ";
        if (str.length() + nextBit.str().size() > desiredLength)
        {
            return str;
        }
        str.append(nextBit.str());
    }
}

void Owner::Set()
{
    m_entity->Data().SetVal(GenerateString(m_nextInstance));
    if (m_entity->Data().GetVal().empty())
    {
        m_entity->Checksum().SetNull();
    }
    else
    {
        m_entity->Checksum() = Safir::Dob::Typesystem::Operations::GetTypeId(m_entity->Data());
    }
    m_connection.SetAll(m_entity,Safir::Dob::Typesystem::InstanceId(m_nextInstance),m_handler);
    ++m_numUpdates;

    if (m_numUpdates %1000 == 0)
    {
        std::wcout << "Update " << m_numUpdates<<std::endl;
    }

    ++m_nextInstance;
    if (m_nextInstance >= m_myFirst + m_instancesPerNode)
    {
        m_nextInstance = m_myFirst;
    }
}


