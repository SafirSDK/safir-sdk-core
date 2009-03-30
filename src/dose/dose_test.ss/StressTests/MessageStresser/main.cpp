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
#include "../common/SimpleDispatcher.h"

int main(int argc, char* argv[])
{
    if(!CommandLine::Instance().Parse(argc,argv))
    {
        return -1;
    }

    try
    {
        Safir::Dob::Connection connection;
        SimpleDispatcher dispatcher(connection);

        std::wstring name = L"Message";
        if (CommandLine::Instance().Sender())
        {
            name += L"Sender";
        }
        else
        {
            name += L"Receiver";
        }


        for(int instance = 0;;++instance)
        {
            try
            {
                connection.Open(name,
                                boost::lexical_cast<std::wstring>(instance),
                                0, // Context
                                &dispatcher,
                                &dispatcher);
                break;
            }
            catch(const Safir::Dob::NotOpenException &)
            {

            }
        }

        std::wcout << "Started as " << Safir::Dob::ConnectionAspectMisc(connection).GetConnectionName() << std::endl;


        bool done = false;

        if(CommandLine::Instance().Sender()) //We are a sender
        {
            Sender sender;
            while (!done)
            {
                sender.SendSome();

                if(dispatcher.Wait(1000))
                {
                    connection.Dispatch();
                }
            }
        }
        else //no, a receiver
        {
            Subscriber subscriber;
            while (!done)
            {
                const bool dispatch = dispatcher.Wait(1000);

                if(dispatch)
                {
                    connection.Dispatch();
                }
            }
        }


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

