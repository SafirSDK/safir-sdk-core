/******************************************************************************
*
* Copyright Saab AB, 2006-2013 (http://safir.sourceforge.net)
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
#include "Dispatcher.h"
#include <Safir/Dob/Connection.h>
#include <Safir/Dob/ConnectionAspectMisc.h>
#include <Safir/Dob/Consumer.h>
#include <Safir/Dob/NotOpenException.h>
#include <Safir/Dob/OverflowException.h>
#include <Safir/Dob/Typesystem/Operations.h>
#include <boost/lexical_cast.hpp>
#include <boost/interprocess/exceptions.hpp>
#include <iostream>
#include <Safir/Utilities/Internal/Atomic.h>
#include <Safir/Utilities/AsioDispatcher.h>
#include "../common/StatisticsCollection.h"


class App: 
        public Safir::Dob::StopHandler,
        private boost::noncopyable
{
public:

#ifdef _MSC_VER
   #pragma warning(push)
   #pragma warning(disable: 4355)
#endif
    explicit App(std::wstring name, int attempts, int timeout, int instance, bool useMenu)
        : m_firstDispatcher(boost::bind(&App::DispatchFirstConnection,this), m_ioService)
        , m_secondDispatcher(boost::bind(&App::DispatchSecondConnection,this), m_ioService)
        , m_exit(false)
        , m_connectionsFirst(StatisticsCollection::Instance().AddHzCollector(L"Connections on first"))
        , m_failedConnectionsFirst(StatisticsCollection::Instance().AddPercentageCollector(L"Failed connections on first", m_connectionsFirst))
        , m_connectionsSecond(StatisticsCollection::Instance().AddHzCollector(L"Connections on second"))
        , m_failedConnectionsSecond(StatisticsCollection::Instance().AddPercentageCollector(L"Failed connections on second", m_connectionsSecond))
    {
        bool exit = false;

        while(!exit)
        {

            StatisticsCollection::Instance().Reset();

            performConnectionAttempts(name, attempts, timeout, instance);

            StatisticsCollection::Instance().PrintStatistics();

            if (useMenu)
            {
                exit = showMenu();
            }
            else
            {
                exit = true;
            }
        }

        m_ioService.stop();
    }
#ifdef _MSC_VER
   #pragma warning(pop)
#endif

    bool showMenu()
    {
        bool done = false;
        bool exit = false;

        while (!done)
        {
            std::wcout << "Choose action:" << std::endl;
            std::wcout << "1:\tRe-run test" << std::endl;
            std::wcout << "2:\tExit test" << std::endl;
            std::wcout << "$:" << std::flush;

            std::string input;
            std::getline(std::cin,input);

            switch (input[0])
            {
            case '1':
                done = true;
                break;
            case '2':
                done = true;
                exit = true;
                break;
            }
        }

        return exit;
    }

    void performConnectionAttempts(std::wstring name, int attempts, int timeout, int instance)
    {

        std::wstring firstName(name.append(L"_First"));
        std::wstring secondName(name.append(L"_Second"));

        std::wcout << std::endl << std::endl << "Running tests instance: " << instance << ", timeout: " << timeout << "ms., attempts: " << attempts << std::endl;

        for (int attemptNo = 0; attemptNo < attempts; ++attemptNo)
        {
            try
            {
                m_firstConnection.Open(firstName,
                                  boost::lexical_cast<std::wstring>(instance),
                                  0, // Context
                                  this,
                                  &m_firstDispatcher);

                m_connectionsFirst->Tick();

            }
            catch(const Safir::Dob::NotOpenException &/*ex*/)
            {
                m_failedConnectionsFirst->Tick();
            }

            try
            {
                m_secondConnection.Open(secondName,
                                  boost::lexical_cast<std::wstring>(instance),
                                  0, // Context
                                  this,
                                  &m_secondDispatcher);

                m_connectionsSecond->Tick();
            }
            catch(const Safir::Dob::NotOpenException &/*ex*/)
            {
                m_failedConnectionsSecond->Tick();
            }

            m_firstConnection.Close();
            m_secondConnection.Close();

            boost::this_thread::sleep_for(boost::chrono::milliseconds(timeout));
        }
    }

    void Run()
    {
        boost::asio::io_service::work keepRunning(m_ioService);
        m_ioService.run();
    }

protected:

    virtual void OnStopOrder()
    {
        m_ioService.stop();
    }


private:

    void DispatchFirstConnection()
    {
        try
        {
            m_firstConnection.Dispatch();
        }
        catch (const Safir::Dob::Typesystem::Exception & exc)
        {
            std::wcout << "Caught Exception when Dispatching FirstConnection: " << exc.GetName() << " - " << exc.GetExceptionInfo() << std::endl;
        }
        catch (const Safir::Dob::Typesystem::FundamentalException & exc)
        {
            std::wcout << "Caught Exception when Dispatching FirstConnection: " << exc.GetName() << " - " << exc.GetExceptionInfo() << std::endl;
        }
    }

    void DispatchSecondConnection()
    {
        try
        {
            m_secondConnection.Dispatch();
        }
        catch (const Safir::Dob::Typesystem::Exception & exc)
        {
            std::wcout << "Caught Exception when Dispatching SecondConnection: " << exc.GetName() << " - " << exc.GetExceptionInfo() << std::endl;
        }
        catch (const Safir::Dob::Typesystem::FundamentalException & exc)
        {
            std::wcout << "Caught Exception when Dispatching SecondConnection: " << exc.GetName() << " - " << exc.GetExceptionInfo() << std::endl;
        }
    }

    boost::asio::io_service                    m_ioService;
    Dispatcher                                 m_firstDispatcher;
    Dispatcher                                 m_secondDispatcher;
    Safir::Dob::Connection                     m_firstConnection;
    Safir::Dob::Connection                     m_secondConnection;
    bool                                       m_exit;
    HzCollector *                              m_connectionsFirst;
    PercentageCollector *                      m_failedConnectionsFirst;
    HzCollector *                              m_connectionsSecond;
    PercentageCollector *                      m_failedConnectionsSecond;
};

int main(int argc, char* argv[])
{
    if(!CommandLine::Instance().Parse(argc,argv))
    {
        return -1;
    }

    try
    {
        std::wstring name = L"ConnectStresser";

        std::wcout << "Starting Connectstresser with args Timeout: " << CommandLine::Instance().Timeout() << " ms., Connection attempts: " << CommandLine::Instance().Attempts() << " Instance: " << CommandLine::Instance().ConnectionInstance() <<  std::endl;

        App app(name, CommandLine::Instance().Attempts(), CommandLine::Instance().Timeout(), CommandLine::Instance().ConnectionInstance(), CommandLine::Instance().ShowMenu());
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




