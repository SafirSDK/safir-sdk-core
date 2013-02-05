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

#include <Safir/Dob/Internal/SharedMemoryObject.h>
#include <Safir/Dob/Typesystem/Defs.h>
#include <boost/thread.hpp>
#include <iostream>

using namespace Safir::Dob::Internal;
using namespace Safir::Dob::Typesystem;

class ShmStatistics:
    public SharedMemoryObject,
    private boost::noncopyable
{
public:
    ShmStatistics(const bool brief):
        m_size(GetSharedMemory().get_size()),
        m_brief(brief),
        m_lastFree(0)
    {

    }

    void DumpMemoryUsage()
    {
        Int64 free = GetSharedMemory().get_free_memory();
        if (m_brief)
        {
            std::wcout << (m_size - free) * 100.0 / m_size << std::endl;
        }
        else
        {
            std::wcout << "Size / Free = " << m_size << " / " << free << std::endl;
            std::wcout << "Allocated delta = " << free - m_lastFree << std::endl;
            m_lastFree = free;
            std::wcout << "Sane = " << std::boolalpha << GetSharedMemory().check_sanity() << std::endl;
            std::wcout << "Number of named objects = " << GetSharedMemory().get_num_named_objects() << std::endl;
            std::wcout << "Number of unique objects = " << GetSharedMemory().get_num_unique_objects() << std::endl;
        }
    }
private:
    const size_t m_size;
    bool m_brief;
    Int64 m_lastFree;

};



int main(int argc, char* argv[])
{
    bool brief = false;
    bool once = false;

    if (argc==2 && std::string(argv[1]) == "-b")
    {
        brief = true;
    }

    if (argc==2 && std::string(argv[1]) == "-o")
    {
        brief = true;
        once = true;
    }

    ShmStatistics stats(brief);

    for(;;)
    {
        if (!brief)
        {
            std::wcout << "--------------------------------------------------" << std::endl;
        }
        stats.DumpMemoryUsage();
        if (once)
        {
            break;
        }
        boost::this_thread::sleep(boost::posix_time::seconds(3));
    }

    return 0;
}

