/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
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

#include <iostream>
#include <Safir/Dob/Internal/Connections.h>
#include <Safir/Dob/Internal/Types.h>
#include <DoseTest/GlobalMessage.h>
#include <Safir/Dob/Typesystem/Serialization.h>
#include <Safir/Dob/Typesystem/ObjectFactory.h>

#include <boost/lexical_cast.hpp>
#include <ace/OS_NS_unistd.h>

using namespace Safir::Dob::Internal;


int main(int argc, char * argv [])
{
    if (argc != 2)
    {
        std::wcout << "need an integer arg" << std::endl;
    }

    std::wcout << "Starting connector" << std::endl;
    srand(time(NULL));

    for(;;)
    {
        ConnectResult result = Success;
        ConnectionPtr connection(NULL);

        do
        {
            Connections::Instance().Connect(std::string("Connector") + boost::lexical_cast<std::string>(rand()),
                                            boost::lexical_cast<int>(argv[1]),
                                            result,
                                            connection);
        }
        while (result != Success);

        Types::Initialize();

        std::wcout << "Connected: " << connection->Name() << ", id = " << connection->Id() << std::endl;
        std::wcout << "Disconnecting: " << std::flush;
        Connections::Instance().Disconnect(connection);
        std::wcout << "Done!" << std::endl;
        //        ACE_OS::sleep(1);
    }

    std::wcout << "EXITING" << std::endl;

    return 0;
}


