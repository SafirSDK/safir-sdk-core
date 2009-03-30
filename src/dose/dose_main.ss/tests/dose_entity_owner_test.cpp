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
#include <DoseTest/GlobalEntity.h>
#include <Safir/Dob/Typesystem/Serialization.h>
#include <Safir/Dob/Typesystem/ObjectFactory.h>

#include <boost/lexical_cast.hpp>
#include <ace/OS_NS_unistd.h>
#include <ace/Thread.h>

using namespace Safir::Dob::Internal;


int main()
{
    srand(time(NULL));


    ConnectResult result = Success;
    ConnectionPtr connection(NULL);

    do
    {
        Connections::Instance().Connect(std::string("Sender") + boost::lexical_cast<std::string>(rand()),
                                        0,
                                        result,
                                        connection);
    }
    while (result != Success);
    Types::Initialize();


    std::wcout << "Connected: " << connection->Name() << ", id = " << connection->Id() << std::endl;

    const Safir::Dob::Typesystem::ObjectId oid(DoseTest::GlobalEntity::ClassTypeId,
                                               Safir::Dob::Typesystem::WHOLE_CLASS);

    const bool regResult = Types::Instance().Register(connection,oid,true);
    std::wcout << "Register returned : " << std::boolalpha << regResult << std::endl;

    //    ACE_OS::sleep(1);
    DoseTest::GlobalEntityPtr ent = DoseTest::GlobalEntity::Create();


    //    DistributionData data(message_tag,connection->Id(),&bin[0]);
    for (int i = 0;;++i)
    {
        ent->Info().SetVal(std::wstring(L"Hello world! ") + boost::lexical_cast<std::wstring>(i));
        ent->SetInstanceNumber(0);
        Safir::Dob::Typesystem::BinarySerialization bin;
        Safir::Dob::Typesystem::Serialization::ToBinary(ent,bin);

        Types::Instance().SetEntity(connection,&bin[0]);
        ACE_OS::sleep(1);
        ACE_Thread::yield();
    }


    std::wcout << "Disconnecting: " << std::flush;
    Connections::Instance().Disconnect(connection);
    std::wcout << "Done!" << std::endl;

    std::wcout << "EXITING" << std::endl;

    return 0;
}


