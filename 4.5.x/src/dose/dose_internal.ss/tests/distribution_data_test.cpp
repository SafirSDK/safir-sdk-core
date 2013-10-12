/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
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

#include <Safir/Dob/Internal/DistributionData.h>
#include <Safir/Dob/Internal/StateDeleter.h>
#include <Safir/Dob/Internal/SharedMemoryObject.h>
#include <Safir/Dob/Message.h>
#include <Safir/Dob/Typesystem/Serialization.h>

using namespace Safir::Dob::Internal;
using namespace Safir::Dob::Typesystem;

class ShmStatistics:
    public SharedMemoryObject
{
public:
    ShmStatistics():m_lastFree(0) {}

    void DumpMemoryUsage()
    {
        Int64 free = GetSharedMemory().get_free_memory();
        m_delta = m_lastFree - free;
        std::wcout << "Size / Free = " << GetSharedMemory().get_size() << " / " << free << std::endl;
        std::wcout << "Allocated delta = " << m_delta << std::endl;
        m_lastFree = free;
    }
    
    Int64 lastDelta() const {return m_delta;}
private:
    Int64 m_lastFree;
    Int64 m_delta;
};

int main(int, char**)
{
    ShmStatistics stats;
    stats.DumpMemoryUsage();
    {
        Safir::Dob::MessagePtr m = Safir::Dob::Message::Create();
        Safir::Dob::Typesystem::BinarySerialization ser;
        Safir::Dob::Typesystem::Serialization::ToBinary(m,ser);

        DistributionData d(message_tag,ConnectionId(100,0,100),Safir::Dob::Typesystem::ChannelId(),&ser[0]);
        std::wcout << d.Image() << std::endl;
        {
            DistributionData d2 (d);
            std::wcout << d.Image() << std::endl;
            std::wcout << d2.Image() << std::endl;
        }
        std::wcout << d.Image() << std::endl;

        DistributionData d3(message_tag,ConnectionId(666,0,666),Safir::Dob::Typesystem::ChannelId(),&ser[0]);
        d3 = d;
        std::wcout << d.Image() << std::endl;
        std::wcout << d3.Image() << std::endl;

        const char * extRef = d.GetReference();
        std::wcout << d.Image() << std::endl;
        DistributionData::DropReference(extRef);
        std::wcout << d.Image() << std::endl;

        char * extAlloc = DistributionData::NewData(d.Size());
        memcpy(extAlloc,extRef,d.Size());
        d3 = DistributionData(new_data_tag, extAlloc);
        std::wcout << d.Image() << std::endl;
        std::wcout << d3.Image() << std::endl;

        {
            DistributionData d4(message_tag,ConnectionId(3445,0,34455),Safir::Dob::Typesystem::ChannelId(),&ser[0]);
            DistributionData d5 = d4;
            extRef = d4.GetReference();
            DistributionData d6 = d5;
        }

        DistributionData::DropReference(extRef);
        DistributionData::DropReference(extAlloc);
    }
    stats.DumpMemoryUsage();
    if (stats.lastDelta() != 0)
    {
        std::wcout << "Memory allocation delta was not 0!" << std::endl;
        return 1;
    }
    return 0;
}

