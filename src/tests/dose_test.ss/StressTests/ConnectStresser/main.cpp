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
    explicit App(std::wstring name, int attempts, int timeout, int instance)
        : m_dispatcher(m_connection, m_ioService)
        , m_instance(instance)
        , m_attempts(attempts)
        , m_timeout(timeout)
        , m_noFailedAttempts(0)

    {

        std::wcout << "Starting Connectstresser with args Timeout: " << m_timeout << " ms., Connection attempts: " << m_attempts << " Instance: " << m_instance <<  std::endl;

        for (int attemptNo = 0; attemptNo < m_attempts; ++attemptNo)
        {
            if ((attemptNo % 10) == 0)
                std::wcout << std::endl;

            try
            {

                m_connection.Open(name,
                                  boost::lexical_cast<std::wstring>(m_instance),
                                  0, // Context
                                  this,
                                  &m_dispatcher);

                std::wcout << "+";

                m_connection.Close();

                std::wcout << "- " << std::flush;


                boost::this_thread::sleep_for(boost::chrono::milliseconds(m_timeout));

            }
            catch(const Safir::Dob::NotOpenException &ex)
            {
                m_noFailedAttempts++;
                m_exceptions.push_back(ex);
                std::wcout << "// " << std::flush;
            }
        }

        std::wcout << std::endl << "Finished using instance: " << m_instance << ", timeout: " << m_timeout << "ms., attempts: " << m_attempts << std::endl;
        std::wcout << "No failed attempts: " << m_noFailedAttempts << std::endl;

        if (m_noFailedAttempts > 0)
        {


            std::wcout << "Press enter to see exceptions given: " << std::endl;
            std::cin.get();
            for (auto ex : m_exceptions)
            {
                std::wcout << ex.what() << std::endl << "--" << std::endl;

            }
        }


        m_ioService.stop();
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

    boost::asio::io_service             m_ioService;
    Safir::Utilities::AsioDispatcher    m_dispatcher;
    Safir::Dob::Connection              m_connection;
    int                                 m_instance;
    int                                 m_attempts;
    int                                 m_timeout;
    int                                 m_noFailedAttempts;
    std::vector<Safir::Dob::NotOpenException> m_exceptions;
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

        App app(name, CommandLine::Instance().Attempts(), CommandLine::Instance().Timeout(), CommandLine::Instance().ConnectionInstance());
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

