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


int main()
{
    //    std::wcout << "Starting connector" << std::endl;
    srand(time(NULL));

    ConnectResult result = Success;
    ConnectionPtr connection(NULL);

    do
    {
        Connections::Instance().Connect(std::string("Subscriber") + boost::lexical_cast<std::string>(rand()),
                                        0,
                                        result,
                                        connection);
    }
    while (result != Success);

    Types::Initialize();


    std::wcout << "Connected: " << connection->Name() << ", id = " << connection->Id() << std::endl;

    Types::Instance().Subscribe(connection,
                                Safir::Dob::Typesystem::ObjectId(DoseTest::GlobalMessage::ClassTypeId,
                                                                 Safir::Dob::Typesystem::WHOLE_CLASS),
                                NULL);

    long count = 0;
    for (;;)
    {
        Connections::Instance().WaitForConnectionSignal(connection->Id());
        //        std::wcout << "Got Connection signal" << std::endl;

        if (connection->StopOrderPending())
        {
            break;
        }

        while (!connection->m_messageInQ.empty())
        {
            DistributionData recData(connection->m_messageInQ.front());
            connection->m_messageInQ.pop();
            Safir::Dob::Typesystem::ObjectPtr obj = Safir::Dob::Typesystem::ObjectFactory::Instance().CreateObject(recData.GetBlob());
            DoseTest::GlobalMessagePtr gmp = boost::static_pointer_cast<DoseTest::GlobalMessage>(obj);
            if (gmp->Info().GetVal() == L"Hello world!")
            {
                ++count;
                if (count % 10000 == 0)
                {
                    std::wcout << "Count = " << count << std::endl;
                }
            }
            else
            {
                std::wcout << gmp->Info().GetVal() << std::endl;
            }
            //            std::wcout << "Got message: '" << gmp->Info().GetVal() << "'" << std::endl;
        }

    }

    std::wcout << "Disconnecting: " << std::flush;
    Connections::Instance().Disconnect(connection);
    std::wcout << "Done!" << std::endl;

    std::wcout << "EXITING" << std::endl;

    return 0;
}


