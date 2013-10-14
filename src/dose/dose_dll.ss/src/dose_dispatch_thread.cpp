/******************************************************************************
*
* Copyright Saab AB, 2008-2013 (http://safir.sourceforge.net)
*
* Created by: Joel Ottosson / stjoot
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

#include "dose_dispatch_thread.h"

#include <Safir/Dob/Internal/Connections.h>
#include <Safir/Dob/Typesystem/Exceptions.h>
#include <Safir/Dob/Typesystem/Internal/InternalUtils.h>
#include <Safir/Dob/Typesystem/LibraryExceptions.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/SystemLog.h>
#include <iostream>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    DispatchThread::DispatchThread(const ConnectionId & id,
                                   const std::string& connectionName,
                                   const ConsumerId & consumer,
                                   OnDispatchCb * const onDispatchCb) :
        m_onDispatchCallback(onDispatchCb),
        m_consumer(consumer),
        m_id(id),
        m_connectionName(connectionName),
        m_stop(0)
    {

    }

    DispatchThread::~DispatchThread()
    {
        ENSURE (m_thread.get_id() == boost::thread::id(), << "DispatchThread is being destroyed while the thread is still running!");
    }

    void DispatchThread::Stop()
    {
        lllout <<"DispatchThread for connection " << m_connectionName.c_str() <<": Stop was called." << std::endl;

        m_stop = 1;

        if (m_thread.get_id() == boost::thread::id())
        {
            return;
        }

        //kick the thread to get it to wake up
        Connections::Instance().GetConnection(m_id)->SignalIn();

        m_thread.join();
        
        //we got it, the thread has stopped
        m_thread = boost::thread();

        lllout << "DispatchThread for connection " << m_connectionName.c_str() <<": Stop() joined thread." << std::endl;
    }

    void DispatchThread::Start()
    {
        ENSURE(m_thread.get_id() == boost::thread::id(), << "Someone tried to start a DispatchThread that was already running!");

        //shared_from_this means that we hold on to a reference to ourselves to ensure that 
        //the object is not destroyed until the thread has ended.
        m_thread = boost::thread(boost::bind(&DispatchThread::Run, shared_from_this()));

        lllout <<"DispatchThread for connection " << m_connectionName.c_str() <<" started. ThreadId = " << m_thread.get_id() << "."<<std::endl;
    }

    void DispatchThread::Run()
    {
        const boost::function<void(void)> waiter = Connections::Instance().GetConnectionSignalWaiter(m_id);
        try
        {
            for (;;)
            {
                //wait for connection signal
                waiter();

                // Check stop-flag
                if(m_stop != 0)
                {
                    lllout << "DispatchThread for connection " << m_connectionName.c_str() <<" about to stop." << std::endl;

                    return;
                }
                
                bool success;
                m_onDispatchCallback(m_consumer.consumer, success);
                if (!success)
                {
                    Typesystem::LibraryExceptions::Instance().Throw();
                }
            }
        }
        catch (const char * e)
        {
            SEND_SYSTEM_LOG(Critical, 
                            <<"DispatchThread for connection " << m_connectionName.c_str() <<" caught 'char *' exception: "
                            << "  '" << e << "'.");
        }
        catch (const Safir::Dob::Typesystem::Internal::CommonExceptionBase & e)
        {
            SEND_SYSTEM_LOG(Critical,
                            << "DispatchThread for connection " << m_connectionName.c_str() <<" caught 'DOTS' exception: "
                            << "  '" << e.GetExceptionInfo() << "'.");
        }
        catch (const std::exception & e)
        {
            SEND_SYSTEM_LOG(Critical,
                            <<"DispatchThread for connection " << m_connectionName.c_str() <<" caught 'std::exception' exception: "
                            << "  '" << e.what() << "'.");
        }
        catch (...)
        {
            SEND_SYSTEM_LOG(Critical,
                            <<"DispatchThread for connection " << m_connectionName.c_str() <<" caught '...' exception!");
        }

        // Got exception. Connection is broken. Exit process.
        exit(100);
    }
}
}
}


