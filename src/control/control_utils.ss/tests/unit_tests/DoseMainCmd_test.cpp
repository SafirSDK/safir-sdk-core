/******************************************************************************
*
* Copyright Saab AB, 2014-2015 (http://safirsdkcore.com)
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
#include <iostream>

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4267)
#pragma warning (disable: 4100)
#endif

#include <boost/asio.hpp>
#include <boost/thread.hpp>

#ifdef _MSC_VER
#pragma warning (pop)
#endif

#define BOOST_TEST_MODULE DoseMainCmdTest
#include <boost/test/unit_test.hpp>

using namespace Safir::Dob::Internal::Control;


BOOST_AUTO_TEST_CASE( send_commands )
{
    boost::asio::io_service pubIoService;
    boost::asio::io_service subIoService;

    std::shared_ptr<boost::asio::io_service::work> pubWork (new boost::asio::io_service::work(pubIoService));
    std::shared_ptr<boost::asio::io_service::work> subWork (new boost::asio::io_service::work(subIoService));

    boost::thread_group threads;
    threads.create_thread([&pubIoService](){pubIoService.run();});
    threads.create_thread([&subIoService](){subIoService.run();});

    bool setOwnNodeCmdReceived = false;
    bool injectNodeCmdReceived = false;
    bool excludeNodeCmdReceived = false;
    bool stoppedNodeIndicationReceived = false;

    std::unique_ptr<DoseMainCmdSender> cmdSender;

    cmdSender.reset(new DoseMainCmdSender (pubIoService,

                                           [&cmdSender]()
                                           {
                                               // cmd receiver has connected

                                               std::wcout << "The receiver has connected!" << std::endl;

                                               cmdSender->StartDoseMain("Kalle",
                                                                        54321,
                                                                        121212,
                                                                        "192.168.211.10");

                                               cmdSender->InjectNode("Olle",
                                                                     99999,
                                                                     88888,
                                                                     "192.168.213.55");

                                               cmdSender->ExcludeNode(620109, 3737);

                                               cmdSender->StoppedNodeIndication(77777);

                                               cmdSender->StopDoseMain();

                                           }));

    std::unique_ptr<DoseMainCmdReceiver> cmdReceiver;

    cmdReceiver.reset(new DoseMainCmdReceiver(subIoService,

                                    // Inject own node callback
                                    [&setOwnNodeCmdReceived](const std::string& nodeName,
                                                             int64_t nodeId,
                                                             int64_t nodeTypeId,
                                                             const std::string& dataAddress)
                                    {
                                        setOwnNodeCmdReceived = true;

                                        BOOST_CHECK(nodeName == "Kalle");
                                        BOOST_CHECK(nodeId == 54321);
                                        BOOST_CHECK(nodeTypeId == 121212);
                                        BOOST_CHECK(dataAddress == "192.168.211.10");
                                    },

                                    // Inject node callback
                                    [&injectNodeCmdReceived](const std::string& nodeName,
                                                             int64_t nodeId,
                                                             int64_t nodeTypeId,
                                                             const std::string& dataAddress)
                                    {
                                        injectNodeCmdReceived = true;

                                        BOOST_CHECK(nodeName == "Olle");
                                        BOOST_CHECK(nodeId == 99999);
                                        BOOST_CHECK(nodeTypeId == 88888);
                                        BOOST_CHECK(dataAddress == "192.168.213.55");
                                    },

                                    // Exclude node callback
                                    [&excludeNodeCmdReceived](int64_t nodeId,
                                                              int64_t nodeTypeId)
                                    {
                                        excludeNodeCmdReceived = true;

                                        BOOST_CHECK(nodeId == 620109);
                                        BOOST_CHECK(nodeTypeId == 3737);
                                    },

                                    // Stopped node indication callback
                                    [&stoppedNodeIndicationReceived](int64_t nodeId)
                                    {
                                        stoppedNodeIndicationReceived = true;

                                        BOOST_CHECK(nodeId == 77777);

                                    },

                                    // Stop dose_main
                                    [&cmdSender, &cmdReceiver, &pubWork, &subWork,
                                     &setOwnNodeCmdReceived, &injectNodeCmdReceived, &excludeNodeCmdReceived,
                                     &stoppedNodeIndicationReceived]()
                                    {
                                        BOOST_CHECK(setOwnNodeCmdReceived);
                                        BOOST_CHECK(injectNodeCmdReceived);
                                        BOOST_CHECK(excludeNodeCmdReceived);
                                        BOOST_CHECK(stoppedNodeIndicationReceived);

                                        cmdSender->Stop();
                                        cmdReceiver->Stop();
                                        pubWork.reset();
                                        subWork.reset();
                                    }

                                    ));
    cmdReceiver->Start();
    cmdSender->Start();

    threads.join_all();

}
