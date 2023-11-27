/******************************************************************************
*
* Copyright Saab AB, 2023 (http://safirsdkcore.com)
*
* Created by: Lars Hagström / lars@foldspace.nu
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
#include <Safir/Utilities/AsioDispatcher.h>
#include <Safir/Dob/Connection.h>
#include <Safir/Dob/Internal/SharedMemoryObject.h>
#include <Safir/Dob/ConnectionAspectMisc.h>
#include <Safir/Dob/LowMemoryException.h>
#include <DoseTest/SynchronousVolatileEntity.h>
#include <boost/lexical_cast.hpp>
#include <iostream>
#include <boost/exception/diagnostic_information.hpp>
#include <boost/thread.hpp>
#include <Safir/Utilities/ProcessInfo.h>

class SlowEntitySubscriber
    : public Safir::Dob::EntitySubscriber
{
public:
    explicit SlowEntitySubscriber()
    {
        m_connection.Attach();
        m_connection.SubscribeEntity(DoseTest::SynchronousVolatileEntity::ClassTypeId,this);
    }

    void OnNewEntity(const Safir::Dob::EntityProxy) override
    {
        boost::this_thread::sleep_for(boost::chrono::milliseconds(5));
    }

    void OnUpdatedEntity(const Safir::Dob::EntityProxy) override
    {
        boost::this_thread::sleep_for(boost::chrono::milliseconds(5));
    }

    void OnDeletedEntity(const Safir::Dob::EntityProxy,
                         const bool                    /*deprecated*/) override
    {
        boost::this_thread::sleep_for(boost::chrono::milliseconds(5));
    }


private:
    Safir::Dob::SecondaryConnection m_connection;

};



class App
    : public Safir::Dob::StopHandler
{
public:
    App()
        : m_work(boost::asio::make_work_guard(m_io))
        , m_dispatcher(m_connection, m_io)
    {
    }

    void OnStopOrder() override
    {
        std::wcout << "Got stop order!" << std::endl;
        m_work.reset();
    }

    int Run()
    {
        m_connection.Open(L"slow_entity_subscriber",L"",0,this,&m_dispatcher);

        m_subscriber = std::make_unique<SlowEntitySubscriber>();

        m_io.run();
        
        m_connection.Close();

        return 0;
    }
private:
    boost::asio::io_context m_io;
    boost::asio::executor_work_guard<boost::asio::io_context::executor_type> m_work;
    Safir::Dob::Connection m_connection;
    
    Safir::Utilities::AsioDispatcher m_dispatcher;
    std::unique_ptr<SlowEntitySubscriber> m_subscriber;
};


int main()
{
    std::wcout << "Pid: " << Safir::Utilities::ProcessInfo::GetPid() << std::endl;
    try
    {
        App app;
        return app.Run();
    }
    catch (...)
    {
        std::wcout << "Caught exception: " << boost::current_exception_diagnostic_information().c_str() << std::endl;
        return 1;
    }
    std::wcout << "Done" << std::endl;
}



