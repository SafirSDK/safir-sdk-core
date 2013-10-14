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
#include <Safir/Utilities/StartupSynchronizer.h>
#include <boost/noncopyable.hpp>
#include <iostream>

//disable warnings in boost
#if defined _MSC_VER
  #pragma warning (push)
  #pragma warning (disable : 4244)
#endif

#include <boost/thread.hpp>

#if defined _MSC_VER
  #pragma warning (pop)
#endif


class Synchronized 
    : public Safir::Utilities::Synchronized
    , private boost::noncopyable
{
public:
    enum State { None = 0, Created = 1, Used = 2};
    
    explicit Synchronized(int & state)
      : m_state(state)
    {

    }
private:
    int & m_state;

    virtual void Create()
    {
        std::wcout << "Ignore this line" << std::endl;
        std::wcout << "-- Create" << std::endl;
        m_state |= Created;
    }
    virtual void Use()
    {
        std::wcout << "Ignore this line" << std::endl;
        std::wcout << "-- Use" << std::endl;
        m_state |= Used;
    }

    virtual void Destroy()
    {
        std::wcout << "-- Destroy" << std::endl;
    }
};

int main()
{
    int state = 0;

    {
        Synchronized synched(state);

        Safir::Utilities::StartupSynchronizer ss("StartupSynchronizer_processes_test");
        ss.Start(&synched);
        std::cin.get();
    }
    return state;
}


