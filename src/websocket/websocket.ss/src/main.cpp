/******************************************************************************
*
* Copyright Saab AB, 2016 (http://safirsdkcore.com)
*
* Created by: Joel Ottosson / joel.ottosson@consoden.se
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
#include <boost/thread.hpp>
#include <thread>
#include "WebsocketServer.h"

#include <Safir/Dob/Typesystem/Serialization.h>

#include "Typesystem.h"
#include <Safir/Dob/Typesystem/ToolSupport/Serialization.h>

int main(int /*argc*/, const char** /*argv*/)
{
    boost::asio::io_service ioService;
    WebsocketServer ws(ioService);
    ioService.post([&]{ws.Run();});
    boost::thread_group threads;
    for (unsigned int i=0; i<3; ++i)
    {
        threads.create_thread([&]{ioService.run();});
    }

    threads.join_all();
    return 0;
}



//void JsonHelpersTest()
//{
//    std::vector<std::string> objects;
//    objects.emplace_back("{\"Val1\": 1, \"Str\": \"hello\"}");
//    objects.emplace_back("{\"MyObj\":{\"Val1\": 1, \"Str\": \"hello\"},\"Val2\":3}");
//    objects.emplace_back("{\"Val4\":\"world\"}");

//    auto array = "["+objects[0] + ", "+objects[1] + ", "+objects[2] + "]";
//    auto isArray = JsonHelpers::IsArray(array);
//    std::cout<<"isArray: "<<std::boolalpha<<isArray<<std::endl;
//    auto iObject = JsonHelpers::IsObject(array);
//    std::cout<<"isArrayObject: "<<std::boolalpha<<iObject<<std::endl;

//    auto result = JsonHelpers::SplitArrayOfObjects(array);

//    auto correctNumberOfObjects=objects.size()==result.size();
//    std::cout<<"arraySize correct: "<<std::boolalpha<<correctNumberOfObjects<<std::endl;


//    for (int i=0; i<objects.size(); ++i)
//    {
//        std::cout<<"--- index "<<i<<" ---"<<std::endl;
//        auto equal=objects[i]==result[i];
//        std::cout<<"areEqual: "<<std::boolalpha<<equal<<std::endl;
//        iObject=JsonHelpers::IsObject(result[i]);
//        std::cout<<"array item isObject: "<<std::boolalpha<<iObject<<std::endl;
//        isArray =JsonHelpers::IsArray(result[i]);
//        std::cout<<"array item isArray: "<<std::boolalpha<<isArray<<std::endl;
//    }
//}
