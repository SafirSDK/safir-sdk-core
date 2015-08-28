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
#include <Safir/Dob/Internal/ControlInfo.h>

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

#define BOOST_TEST_MODULE ControlInfoTest
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

    bool infoReceived = false;

    std::unique_ptr<ControlInfoSender> infoSender;

    infoSender.reset(new ControlInfoSender (pubIoService,

                                           [&infoSender]()
                                           {
                                               // info receiver has connected

                                               std::wcout << "The receiver has connected!" << std::endl;

                                               infoSender->SendInfo(1234567, 7654321);

                                           }));

    std::unique_ptr<ControlInfoReceiver> infoReceiver;

    infoReceiver.reset(new ControlInfoReceiver(subIoService,
                                    // info callback
                                    [&infoSender, &infoReceiver, &pubWork, &subWork, &infoReceived]
                                    (int64_t incarnationId , int64_t nodeId)
                                    {
                                        infoReceived = true;

                                        BOOST_CHECK(incarnationId == 1234567);
                                        BOOST_CHECK(nodeId == 7654321);

                                        infoSender->Stop();
                                        infoReceiver->Stop();
                                        pubWork.reset();
                                        subWork.reset();

                                    }));
    infoReceiver->Start();
    infoSender->Start();

    threads.join_all();

}
