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



class App: 
        public Safir::Dob::StopHandler,
        private boost::noncopyable
{
public:
    explicit App(std::wstring name, int attempts, int timeout, int instance, bool useMenu)
        : m_firstDispatcher(boost::bind(&App::DispatchFirstConnection,this), m_ioService)
        , m_secondDispatcher(boost::bind(&App::DispatchSecondConnection,this), m_ioService)
        , m_noFailedAttempts(0)
        , m_exit(false)
    {

        while(!m_exit)
        {

            performConnectionAttempts(name, attempts, timeout, instance);

            if (useMenu)
            {
                showMenu();
            }
            else
            {
                printExceptions();
                m_exit = true;
            }
        }

        m_ioService.stop();
    }

    void printExceptions()
    {
        for (auto ex = m_exceptions.cbegin(); ex != m_exceptions.cend(); ++ex)
        {
            std::wcout << ex->what() << std::endl << "--" << std::endl;
        }
    }

    void showMenu()
    {
        bool done = false;

        while (!done)
        {
            std::wcout << "Choose action:" << std::endl;
            std::wcout << "1:\tSee exceptions" << std::endl;
            std::wcout << "2:\tRe-run test" << std::endl;
            std::wcout << "3:\tExit test" << std::endl;
            std::wcout << "$:" << std::flush;

            std::string input;
            std::getline(std::cin,input);

            switch (input[0])
            {
            case '1':
                printExceptions();
                break;
            case '2':
                done = true;
                break;
            case '3':
                done = true;
                m_exit = true;
                break;
            }
        }
    }

    bool performConnectionAttempts(std::wstring name, int attempts, int timeout, int instance)
    {

        std::wstring firstName(name.append(L"_First"));
        std::wstring secondName(name.append(L"_Second"));


        m_exceptions.clear();

        std::wcout << std::endl << std::endl << "Running tests instance: " << instance << ", timeout: " << timeout << "ms., attempts: " << attempts << std::endl;


        for (int attemptNo = 0; attemptNo < attempts; ++attemptNo)
        {
            if ((attemptNo % 10) == 0)
                std::wcout << std::endl;

            try
            {
                m_firstConnection.Open(firstName,
                                  boost::lexical_cast<std::wstring>(instance),
                                  0, // Context
                                  this,
                                  &m_firstDispatcher);

                std::wcout << "+";

                m_secondConnection.Open(secondName,
                                  boost::lexical_cast<std::wstring>(instance),
                                  0, // Context
                                  this,
                                  &m_secondDispatcher);

                std::wcout << "+";

                m_firstConnection.Close();
                std::wcout << "-";

                m_secondConnection.Close();
                std::wcout << "- " << std::flush;

                boost::this_thread::sleep_for(boost::chrono::milliseconds(timeout));
            }
            catch(const Safir::Dob::NotOpenException &ex)
            {
                m_noFailedAttempts++;
                m_exceptions.push_back(ex);
                std::wcout << "// " << std::flush;
            }
        }

        std::wcout << std::endl << "No failed attempts: " << m_noFailedAttempts << std::endl << std::endl;

        return (m_exceptions.size() == 0);
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
    int                                        m_noFailedAttempts;
    std::vector<Safir::Dob::NotOpenException>  m_exceptions;
    bool                                       m_exit;
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




