/******************************************************************************
*
* Copyright Saab AB, 2006-2013 (http://safir.sourceforge.net)
*
* Created by: Lars Hagstrom / stlrha
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

#include <iostream>
#include "../common/ErrorReporter.h"
#include <Safir/Dob/Connection.h>
#include "../common/SimpleDispatcher.h"
#include <Safir/Dob/Typesystem/Operations.h>
#include <Safir/Dob/ConnectionAspectMisc.h>
#include <Safir/Dob/Consumer.h>
#include "CommandLine.h"
#include "Pinger.h"
#include "Ponger.h"
#include <Safir/Dob/NotOpenException.h>

#include <Safir/Dob/OverflowException.h>

#if defined _MSC_VER
  #pragma warning (push)
  #pragma warning (disable : 4127)
#endif

#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/lexical_cast.hpp>

#if defined _MSC_VER
  #pragma warning (pop)
#endif

int main(int argc, char* argv[])
{
    srand(static_cast<unsigned int>(time(NULL)));
    if(!CommandLine::Instance().Parse(argc,argv))
    {
        return -1;
    }

    std::wcout << "Using a timeout of " << CommandLine::Instance().Timeout() << std::endl;

    try
    {
        Safir::Dob::Connection connection;
        SimpleDispatcher dispatcher(connection);

        std::wstring name = L"MultiPingPong";

        if (CommandLine::Instance().Payload())
        {
            name += L"WithPayload";
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
        boost::shared_ptr<Pinger> pinger;
        boost::shared_ptr<Ponger> ponger;

        if(CommandLine::Instance().Pinger())
        {
            pinger.reset(new Pinger());
        }

        if(CommandLine::Instance().Ponger())
        {
            ponger.reset(new Ponger());
        }

        boost::posix_time::ptime nextTimeout = boost::posix_time::second_clock::local_time() + boost::posix_time::seconds(1);
        while (!done)
        {
            const boost::posix_time::ptime now = boost::posix_time::second_clock::local_time();

            const bool dispatch = dispatcher.Wait(static_cast<long>((nextTimeout - now).total_milliseconds()));
            if (dispatch)
            {
                connection.Dispatch();
            }
            else
            {
                nextTimeout = now + boost::posix_time::seconds(1);
                if (pinger != NULL)
                {
                    pinger->CheckForTimeouts();
                }
            }
        }

    }
    catch(std::exception & e)
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

