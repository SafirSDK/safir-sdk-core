/******************************************************************************
*
* Copyright Saab AB, 2015 (http://safirsdkcore.com/)
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
#include <Safir/Dob/Internal/ControlCmd.h>

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

#define BOOST_TEST_MODULE ControlCmdTest
#include <boost/test/unit_test.hpp>

using namespace Safir::Dob::Internal::Control;


BOOST_AUTO_TEST_CASE( send_commands )
{
    boost::asio::io_service pubIoService;
    boost::asio::io_service subIoService;

    boost::shared_ptr<boost::asio::io_service::work> pubWork (new boost::asio::io_service::work(pubIoService));
    boost::shared_ptr<boost::asio::io_service::work> subWork (new boost::asio::io_service::work(subIoService));

    boost::thread_group threads;
    threads.create_thread([&pubIoService](){pubIoService.run();});
    threads.create_thread([&subIoService](){subIoService.run();});

    bool nodeCmdReceived = false;

    std::unique_ptr<ControlCmdSender> cmdSender;

    cmdSender.reset(new ControlCmdSender (pubIoService,

                                           [&cmdSender]()
                                           {
                                               // cmd receiver has connected

                                               std::wcout << "The receiver has connected!" << std::endl;

                                               cmdSender->SendCmd(REBOOT, 1234567);

                                               cmdSender->SendCmd(SHUTDOWN,0);

                                           }));

    std::unique_ptr<ControlCmdReceiver> cmdReceiver;

    cmdReceiver.reset(new ControlCmdReceiver(subIoService,

                                    // cmd callback
                                    [&cmdSender, &cmdReceiver, &pubWork, &subWork, &nodeCmdReceived]
                                    (CommandAction cmdAction, int64_t nodeId)
                                    {
                                        if (nodeId != 0)
                                        {
                                            nodeCmdReceived =true;
                                            BOOST_CHECK(cmdAction == REBOOT);
                                            BOOST_CHECK(nodeId == 1234567);
                                        }
                                        else
                                        {
                                            BOOST_CHECK(cmdAction == SHUTDOWN);

                                            cmdSender->Stop();
                                            cmdReceiver->Stop();
                                            pubWork.reset();
                                            subWork.reset();
                                        }
                                    }));
    cmdReceiver->Start();
    cmdSender->Start();

    threads.join_all();

}
