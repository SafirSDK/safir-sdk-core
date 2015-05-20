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
#include <map>

//disable warnings in boost
#if defined _MSC_VER
  #pragma warning (push)
  #pragma warning (disable : 4244)
  #pragma warning (disable : 4702)
  #pragma warning (disable : 4100)
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
    enum State { None = 0, Created = 1, Used = 2, Destroyed = 4};
    
    explicit Synchronized(int & state)
      : m_state(state)
    {

    }
private:
    int & m_state;

    virtual void Create()
    {
        m_state |= Created;
    }
    virtual void Use()
    {
        m_state |= Used;
    }

    virtual void Destroy()
    {
        m_state |= Destroyed;
    }
};

int threadFun(boost::barrier& barrier1, boost::barrier& barrier2, boost::barrier& barrier3) 
{
    int state = 0;
    Synchronized synched(state);

    {
        Safir::Utilities::StartupSynchronizer ss("StartupSynchronizer_processes_and_threads_test");
        barrier1.wait();
        ss.Start(&synched);
        barrier2.wait();
        barrier3.wait();
    }
    return state;
}

int main()
{
    const int num = 100;
    boost::barrier barrier1(num);
    boost::barrier barrier2(num + 1);
    boost::barrier barrier3(num + 1);

    std::vector<boost::shared_future<int> > futures;
    boost::thread_group threads;
    for(int i = 0; i < num; ++i)
    {
        boost::packaged_task<int> pt(boost::bind(threadFun,boost::ref(barrier1),boost::ref(barrier2),boost::ref(barrier3)));
        futures.push_back(boost::shared_future<int>(pt.get_future()));
        threads.add_thread(new boost::thread(boost::move(pt)));
    }

    barrier2.wait();    
    std::wcout << "all threads started, collecting result" << std::endl;
    std::cin.get();
    barrier3.wait();    

    int num_created = 0;
    int num_used = 0;
    int num_destroyed = 0;
    for(std::vector<boost::shared_future<int> >::iterator it = futures.begin();
        it != futures.end(); ++it)
    {
        const int state = it->get();
        if ((state & Synchronized::Created) != 0)
        {
            ++num_created;
        }
        if ((state & Synchronized::Used) != 0)
        {
            ++num_used;
        }
        if ((state & Synchronized::Destroyed) != 0)
        {
            ++num_destroyed;
        }
    }
    std::wcout << "num_created: " << num_created << std::endl;
    std::wcout << "num_used: " << num_used << std::endl;
    std::wcout << "num_destroyed: " << num_destroyed << std::endl;

    threads.join_all();

    return 0;
}


