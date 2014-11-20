/******************************************************************************
*
* Copyright Saab AB, 2014 (http://safir.sourceforge.net)
*
* Created by: Anders Widén / anders.widen@consoden.se
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
#include <Safir/Dob/Internal/DoseMainCmd.h>
#include <boost/thread.hpp>
#include <boost/asio.hpp>

#define BOOST_TEST_MODULE DoseMainCmdTest
#include <boost/test/unit_test.hpp>

using namespace Safir::Dob::Internal::Control;


BOOST_AUTO_TEST_CASE( send_inject_node )
{
    boost::asio::io_service pubIoService;
    boost::asio::io_service subIoService;

    boost::shared_ptr<boost::asio::io_service::work> pubWork (new boost::asio::io_service::work(pubIoService));
    boost::shared_ptr<boost::asio::io_service::work> subWork (new boost::asio::io_service::work(subIoService));

    boost::thread_group threads;
    threads.create_thread([&pubIoService](){pubIoService.run();});
    threads.create_thread([&subIoService](){subIoService.run();});

    std::wcout << "About to create cmdSender" << std::endl;

    DoseMainCmdSender cmdSender(pubIoService);

    std::wcout << "About to create cmdReceiver" << std::endl;

    DoseMainCmdReceiver cmdReceiver(subIoService,
                                    [](int64_t requestId,
                                       const std::string& nodeName,
                                       int64_t nodeId,
                                       int64_t nodeTypeId,
                                       const std::string& dataAddress)
                                    {
                                        BOOST_CHECK(requestId == 12345);
                                        BOOST_CHECK(nodeName == "Kalle");
                                        BOOST_CHECK(nodeId == 54321);
                                        BOOST_CHECK(nodeTypeId == 121212);
                                        BOOST_CHECK(dataAddress == "192.168.211.10");
                                    });

    std::wcout << "cmdSender.Start()" << std::endl;
    cmdSender.Start();
    std::wcout << "cmdReceiver.Start()" << std::endl;
    cmdReceiver.Start();

    // Sleep a while to let the sender and receiver connect
    boost::this_thread::sleep_for(boost::chrono::milliseconds(2000));

    cmdSender.InjectNode(12345,
                         "Kalle",
                         54321,
                         121212,
                         "192.168.211.10");

    boost::this_thread::sleep_for(boost::chrono::milliseconds(10));

    cmdSender.Stop();
    cmdReceiver.Stop();
    pubWork.reset();
    subWork.reset();

    threads.join_all();

}
