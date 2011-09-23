/******************************************************************************
*
* Copyright Saab AB, 2006-2008 (http://www.safirsdk.com)
*
* Created by: Hannah Myerscough / sthamy
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

#include "CommandLine.h"
#include "Sender.h"
#include "Subscriber.h"
#include <Safir/Dob/Connection.h>
#include <Safir/Dob/ConnectionAspectMisc.h>
#include <Safir/Dob/Consumer.h>
#include <Safir/Dob/NotOpenException.h>
#include <Safir/Dob/OverflowException.h>
#include <Safir/Dob/Typesystem/Operations.h>
#include <boost/lexical_cast.hpp>
#include <boost/interprocess/exceptions.hpp>
#include <iostream>
#include <Safir/Dob/Internal/Atomic.h>
#include <Safir/Utilities/AceDispatcher.h>

#ifdef _MSC_VER
  #pragma warning(push)
  #pragma warning(disable: 4267)
#endif

#include <ace/Reactor.h>

#ifdef _MSC_VER
  #pragma warning(pop)
#endif


class App: 
    public Safir::Dob::StopHandler,
    private boost::noncopyable
{
public:
    App(std::wstring name):m_done(false), m_dispatcher(connection)
    {
        for(int instance = 0;;++instance)
        {
            try
            {
                connection.Open(name,
                    boost::lexical_cast<std::wstring>(instance),
                    0, // Context
                    this,
                    &m_dispatcher);
                break;
            }
            catch(const Safir::Dob::NotOpenException &)
            {

            }
        }

        std::wcout << "Started as " << Safir::Dob::ConnectionAspectMisc(connection).GetConnectionName() << std::endl;

        if(CommandLine::Instance().Sender()) //We are a sender
        {       
            m_sender.Start();
            m_sender.SendSome();
        }
        else //no, a receiver
        {
            m_subscriber.Start();
        }
    }

    void Run()
    {
        ACE_Reactor::instance()->run_event_loop();
    }

protected:
 
    virtual void OnStopOrder()
    {
        m_done = true;
    }

    bool m_done;
    Safir::Utilities::AceDispatcher m_dispatcher;
    Safir::Dob::Connection connection;
    Sender m_sender;
    Subscriber m_subscriber;

};

int main(int argc, char* argv[])
{
    if(!CommandLine::Instance().Parse(argc,argv))
    {
        return -1;
    }

    try
    {
        std::wstring name = L"Message";
        if (CommandLine::Instance().Sender())
        {
            name += L"Sender";
        }
        else
        {
            name += L"Receiver";
        }


        App app(name);
        app.Run();

    }
    catch(const boost::interprocess::bad_alloc & e)
    {
        std::wcout << "Caught boost::interprocess::bad_alloc! Contents of exception is:" << std::endl
            << e.what()<<std::endl;
        std::cin.get();
    }
    catch(const std::exception & e)
    {
        std::wcout << "Caught std::exception! Contents of exception is:" << std::endl
            << e.what()<<std::endl;
        std::cin.get();
    }
    catch (...)
    {
        std::wcout << "Caught ... exception!" << std::endl;
        std::cin.get();
    }

    
 
    return 0;
}

