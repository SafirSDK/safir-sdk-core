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
//#define VLD_AGGREGATE_DUPLICATES
//#include <vld.h>
#include <iostream>
#include <tchar.h>
#include <Safir/Dob/Typesystem/Operations.h>
#include <Safir/Dob/Connection.h>
#include <Safir/Dob/Consumer.h>
#include <Safir/Utilities/AceDispatcher.h>
#include <DopeTest/SmallEntity.h>
#include <DopeTest/BigEntity.h>
#include <boost/lexical_cast.hpp>
#include "CommandLine.h"

boost::shared_ptr<CommandLine> commandLine;

class StopHandler :
    public Safir::Dob::StopHandler
{
    virtual void OnStopOrder() {ACE_Reactor::instance()->end_reactor_event_loop();}
};

class EntityOwner : public Safir::Dob::EntityHandlerInjection
{
public:
    EntityOwner():
      m_lastInstance(-1)
    {
        m_connection.Attach();
        m_connection.RegisterEntityHandlerInjection(commandLine->GetType(),
                                                    Safir::Dob::Typesystem::HandlerId(),
                                                    Safir::Dob::InstanceIdPolicy::HandlerDecidesInstanceId,
                                                    this);

    }

    void Set()
    {
        if (commandLine->Small())
        {
            DopeTest::SmallEntityPtr sens = DopeTest::SmallEntity::Create();
            sens->Name().SetVal(L"10"/*boost::lexical_cast<std::wstring>(i)*/);
            sens->Position().SetPtr(Safir::Geodesy::Position::Create());
            sens->ExternalName().SetVal(L"asdf");
            sens->Period().SetVal(static_cast<Safir::Dob::Typesystem::Si32::Second>(i));
            connection.Set(sens);
        }
        else
        {
            //TODO
        }
    }

private:
    virtual void OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId     typeId,
        const Safir::Dob::Typesystem::HandlerId& handlerId) {}

    virtual void OnCreateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
        Safir::Dob::ResponseSenderPtr        responseSender) {}

    virtual void OnUpdateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
        Safir::Dob::ResponseSenderPtr        responseSender) {}

    virtual void OnDeleteRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
        Safir::Dob::ResponseSenderPtr        responseSender) {}

    Safir::Dob::Typesystem::Int64 m_lastInstance;
    Safir::Dob::SecondaryConnection m_connection;
};

int main(int argc, char* argv[])
{
    commandLine.reset(new CommandLine(argc,argv));
    try
    {
        const std::wstring nameCommonPart = L"C++";
        const std::wstring nameInstancePart = L"1";

        StopHandler stopHandler;

        Safir::Dob::Connection connection;

        Safir::Utilities::AceDispatcher dispatcher(connection);

        connection.Open(nameCommonPart,
            nameInstancePart,
            0, // Context
            &stopHandler,
            &dispatcher);

        std::wcout << nameCommonPart.c_str() <<  nameInstancePart.c_str() << ": Started" <<std::endl;
        EntityOwner owner;
        
        int last = 0;
        const double DELAY = 0.1;
        const int BATCH = (int)(DELAY * Safir::Dob::Typesystem::Operations::GetMaxNumberOfInstances(Safir::Sensors::Sensor::ClassTypeId));
        while(true)
        {
            const osin::EventId id = eventHandler.WaitForEvent((unsigned long)(DELAY*1000));
            if (id == doseEvent.IdOf())
            {
                connection.Dispatch();
            }
            else if (id == osin::NO_EVENT)
            {
                //          for (int i = 0; i < Safir::Dob::Typesystem::Operations::GetMaxNumberOfInstances(Safir::Sensors::Sensor::ClassTypeId); ++i)
                for (int i = 0; i < BATCH; ++i)
                {
                    
                    //std::wcout << "Set object "<< i <<std::endl;
                }
                last += BATCH;
                if (last >= Safir::Dob::Typesystem::Operations::GetMaxNumberOfInstances(Safir::Sensors::Sensor::ClassTypeId))
                {
                    last = 0;
                }
            }
            else
            {
                std::wcout << "Unknown event! Fatal error!" << std::endl;
                return 1;
            }

        }

        connection.Close();
        std::wcout << "End" << std::endl;
    }
    catch(std::exception & e)
    {
        std::wcout << "Caught std::exception! Contents of exception is:" << std::endl
            << e.what()<<std::endl;
    }
    catch (...)
    {
        std::wcout << "Caught ... exception!" << std::endl;
    }

    //std::cin.get();
    return 0;
}

