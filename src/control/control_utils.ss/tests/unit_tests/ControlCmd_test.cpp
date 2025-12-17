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
#include <atomic>
#include <chrono>
#include <system_error>
#include <thread>

#ifdef _MSC_VER
#pragma warning (pop)
#endif

#define BOOST_TEST_MODULE ControlCmdTest
#include <boost/test/unit_test.hpp>

using namespace Safir::Dob::Internal::Control;

/**
 * Helper that runs an io_context on a few threads and joins them when destroyed.
 */
class IoContextRunner
{
public:
    explicit IoContextRunner(boost::asio::io_context& io)
        : m_io(io)
    {
        for (int i = 0; i < 3; ++i)
        {
            m_threads.create_thread([this]()
            {
                m_io.run();
            });
        }
    }

    ~IoContextRunner()
    {
        m_threads.join_all();
    }

    void join()
    {
        m_threads.join_all();
    }
private:
    boost::asio::io_context& m_io;
    boost::thread_group      m_threads;
};

BOOST_AUTO_TEST_CASE(subscriber_start_stop)
{
    boost::asio::io_context subIoContext;
    auto subWork = boost::asio::make_work_guard(subIoContext);
    std::atomic<int> numCallbacks(0);
    auto cmdReceiver =
        std::make_unique<ControlCmdReceiver>(subIoContext,
                                             [&numCallbacks](CommandAction, int64_t)
                                             {
                                                 ++numCallbacks;
                                             });

    cmdReceiver->Start();
    cmdReceiver->Stop();

    IoContextRunner subRunner(subIoContext);

    subWork.reset();
    subRunner.join();
    BOOST_CHECK_EQUAL(numCallbacks,0);
}

BOOST_AUTO_TEST_CASE(publisher_without_subscriber)
{
    boost::asio::io_context pubIoContext;
    auto pubWork = boost::asio::make_work_guard(pubIoContext);
    std::unique_ptr<ControlCmdSender> cmdSender = std::make_unique<ControlCmdSender>(pubIoContext);

    std::atomic<int> numCallbacks(0);
    std::atomic<bool> timeoutReceived(false);
    cmdSender->SendCmd(REBOOT,
                       1234567,
                       std::chrono::milliseconds(200),
                       [&numCallbacks,&timeoutReceived](const std::error_code& err){
                           ++numCallbacks;
                           timeoutReceived.store(err == std::make_error_code(std::errc::timed_out));
                       });

    IoContextRunner pubRunner(pubIoContext);

    pubWork.reset();
    pubRunner.join();
    BOOST_CHECK_EQUAL(numCallbacks,1);
    BOOST_CHECK(timeoutReceived.load());
}


BOOST_AUTO_TEST_CASE(double_send_is_illegal)
{
    boost::asio::io_context pubIoContext;
    auto pubWork = boost::asio::make_work_guard(pubIoContext);
    std::unique_ptr<ControlCmdSender> cmdSender = std::make_unique<ControlCmdSender>(pubIoContext);
    IoContextRunner pubRunner(pubIoContext);

    std::atomic<int> numCallbacks(0);
    std::atomic<bool> timeoutReceived(false);
    std::atomic<bool> inProgressReceived(false);
    cmdSender->SendCmd(REBOOT,
                       1234567,
                       std::chrono::milliseconds(200),
                       [&numCallbacks,&timeoutReceived](const std::error_code& err){
                           ++numCallbacks;
                           timeoutReceived.store(err == std::make_error_code(std::errc::timed_out));
                       });
    cmdSender->SendCmd(STOP,
                       1234568,
                       std::chrono::milliseconds(100),
                       [&numCallbacks,&inProgressReceived](const std::error_code& err){
                           ++numCallbacks;
                           inProgressReceived.store(err == std::make_error_code(std::errc::operation_in_progress));
                       });

    pubWork.reset();
    pubRunner.join();
    BOOST_CHECK_EQUAL(numCallbacks,2);
    BOOST_CHECK(timeoutReceived.load());
    BOOST_CHECK(inProgressReceived.load());
}

BOOST_AUTO_TEST_CASE(recursive_send_is_illegal)
{
    boost::asio::io_context pubIoContext;
    auto pubWork = boost::asio::make_work_guard(pubIoContext);
    std::unique_ptr<ControlCmdSender> cmdSender = std::make_unique<ControlCmdSender>(pubIoContext);
    IoContextRunner pubRunner(pubIoContext);
    std::atomic<int> numCallbacks(0);
    std::atomic<bool> timeout1Received{false};
    std::atomic<bool> timeout2Received{false};

    cmdSender->SendCmd(REBOOT,
                       1234567,
                       std::chrono::milliseconds(200),
                       [&numCallbacks,&cmdSender,&timeout1Received,&timeout2Received](const std::error_code& err)
                       {
                           ++numCallbacks;
                           timeout1Received.store(err == std::make_error_code(std::errc::timed_out));
                           cmdSender->SendCmd(STOP,
                                              1234568,
                                              std::chrono::milliseconds(100),
                                              [&numCallbacks,&cmdSender,&timeout2Received](const std::error_code& err)
                                              {
                                                  ++numCallbacks;
                                                  timeout2Received.store(err == std::make_error_code(std::errc::operation_in_progress));
                                              });
                       });

    pubWork.reset();
    pubRunner.join();
    BOOST_CHECK_EQUAL(numCallbacks,2);
    BOOST_CHECK(timeout1Received.load());
    BOOST_CHECK(timeout2Received.load());
}

BOOST_AUTO_TEST_CASE(two_sends_timeouts)
{
    boost::asio::io_context pubIoContext;
    auto pubWork = boost::asio::make_work_guard(pubIoContext);
    std::unique_ptr<ControlCmdSender> cmdSender = std::make_unique<ControlCmdSender>(pubIoContext);
    IoContextRunner pubRunner(pubIoContext);
    std::atomic<int> numCallbacks(0);
    std::atomic<bool> timeout1Received{false};
    std::atomic<bool> timeout2Received{false};

    cmdSender->SendCmd(REBOOT,
                       1234567,
                       std::chrono::milliseconds(200),
                       [&numCallbacks,&cmdSender,&timeout1Received,&timeout2Received,&pubIoContext](const std::error_code& err)
                       {
                           ++numCallbacks;
                           timeout1Received.store(err == std::make_error_code(std::errc::timed_out));

                           //send the next command but in a post, since recursive sends are not legal
                           boost::asio::post(pubIoContext,[&numCallbacks,&cmdSender,&timeout2Received]
                           {
                               ++numCallbacks;
                               cmdSender->SendCmd(STOP,
                                                  1234568,
                                                  std::chrono::milliseconds(100),
                                                  [&numCallbacks,&cmdSender,&timeout2Received](const std::error_code& err)
                                                  {
                                                      timeout2Received.store(err == std::make_error_code(std::errc::timed_out));
                                                  });
                           });
                       });

    pubWork.reset();
    pubRunner.join();
    BOOST_CHECK_EQUAL(numCallbacks,2);
    BOOST_CHECK(timeout1Received.load());
    BOOST_CHECK(timeout2Received.load());
}

BOOST_AUTO_TEST_CASE(single_sender_single_receiver_one_command)
{
    BOOST_TEST_MESSAGE("Test started");
    boost::asio::io_context subIoContext;
    boost::asio::io_context pubIoContext;

    auto pubWork = boost::asio::make_work_guard(pubIoContext);
    auto subWork = boost::asio::make_work_guard(subIoContext);

    std::atomic<int> cmdReceived(0);
    std::atomic<bool> actionCorrect(false);
    std::atomic<bool> nodeIdCorrect(false);

    // Long-lived receiver
    std::unique_ptr<ControlCmdReceiver> cmdReceiver;
    cmdReceiver.reset(new ControlCmdReceiver(
        subIoContext,
        [&pubWork, &subWork, &cmdReceived, &cmdReceiver, &actionCorrect, &nodeIdCorrect]
        (CommandAction cmdAction, int64_t nodeId)
        {
            ++cmdReceived;
            actionCorrect.store(cmdAction == REBOOT);
            nodeIdCorrect.store(nodeId == 1234567);

            // Stop both io_contexts; this will allow the test to finish.
            pubWork.reset();
            subWork.reset();
            cmdReceiver->Stop();
        }));


    BOOST_TEST_MESSAGE("Start receiver");
    cmdReceiver->Start();

    BOOST_TEST_MESSAGE("Create sender");
    // Sender instance
    std::unique_ptr<ControlCmdSender> cmdSender = std::make_unique<ControlCmdSender>(pubIoContext);


    std::atomic<int> completionReceived(0);
    std::atomic<bool> sendSuccessful(false);
    BOOST_TEST_MESSAGE("Send message 1");
    // Send a node-specific command and then a system-wide command.
    cmdSender->SendCmd(REBOOT,
                       1234567,
                       std::chrono::milliseconds(10000),
                       [&completionReceived,&sendSuccessful](const std::error_code& err)
                       {
                           sendSuccessful.store(err.value() == 0);
                           ++completionReceived;
                       });

    BOOST_TEST_MESSAGE("Starting runners");
    IoContextRunner pubRunner(pubIoContext);
    IoContextRunner subRunner(subIoContext);

    BOOST_TEST_MESSAGE("joining pubRunner");
    pubRunner.join();
    BOOST_TEST_MESSAGE("joining subRunner");
    subRunner.join();

    BOOST_TEST_MESSAGE("Checking results");
    // Ensure both commands were observed.
    BOOST_CHECK_EQUAL(cmdReceived.load(), 1);
    BOOST_CHECK(actionCorrect.load());
    BOOST_CHECK(nodeIdCorrect.load());
    BOOST_CHECK_EQUAL(completionReceived.load(), 1);
    BOOST_CHECK(sendSuccessful.load());
}

/**
 * Basic test: verify that a single sender and receiver can exchange two
 * commands (one node-specific and one system-wide) successfully.
 *
 * This tests the "happy path" with a long-lived receiver and a sender
 * that sends a couple of commands while both are running.
 */
BOOST_AUTO_TEST_CASE(single_sender_single_receiver_happy_path)
{
    boost::asio::io_context subIoContext;
    boost::asio::io_context pubIoContext;

    auto pubWork = boost::asio::make_work_guard(pubIoContext);
    auto subWork = boost::asio::make_work_guard(subIoContext);

    std::atomic<int> cmdReceived(0);
    std::atomic<bool> action1Correct(false);
    std::atomic<bool> nodeId1Correct(false);
    std::atomic<bool> action2Correct(false);

    // Long-lived receiver
    std::unique_ptr<ControlCmdReceiver> cmdReceiver;
    cmdReceiver.reset(new ControlCmdReceiver(
        subIoContext,
        [&pubWork, &subWork, &cmdReceived, &action1Correct,&action2Correct,&nodeId1Correct, &cmdReceiver]
        (CommandAction cmdAction, int64_t nodeId)
        {
            ++cmdReceived;
            if (nodeId != 0)
            {
                action1Correct.store(cmdAction == REBOOT);
                nodeId1Correct.store(nodeId == 1234567);
            }
            else
            {
                action2Correct.store(cmdAction == SHUTDOWN);

                // Stop both io_contexts; this will allow the test to finish.
                pubWork.reset();
                subWork.reset();
                cmdReceiver->Stop();
            }
        }));


    BOOST_TEST_MESSAGE("Start receiver");
    cmdReceiver->Start();

    BOOST_TEST_MESSAGE("Create sender");
    // Sender instance
    std::unique_ptr<ControlCmdSender> cmdSender = std::make_unique<ControlCmdSender>(pubIoContext);


    BOOST_TEST_MESSAGE("Send message 1");

    std::atomic<int> numCallbacks(0);
    std::atomic<bool> completion1Received(false);
    std::atomic<bool> completion2Received(false);
    // Send a node-specific command and then a system-wide command.
    cmdSender->SendCmd(REBOOT,
                       1234567,
                       std::chrono::milliseconds(10000),
                       [&numCallbacks,&cmdSender,&completion1Received,&completion2Received,&pubIoContext](const std::error_code& err){
                           ++numCallbacks;
                           completion1Received.store(err.value() == 0);

                           //send the next command but in a post, since recursive sends are not legal
                           boost::asio::post(pubIoContext,[&numCallbacks,&cmdSender,&completion2Received]
                           {
                               cmdSender->SendCmd(SHUTDOWN,
                                                  0,
                                                  std::chrono::milliseconds(10000),
                                                  [&numCallbacks,&completion2Received](const std::error_code& err){
                                                      ++numCallbacks;
                                                      completion2Received.store(err.value() == 0);
                                                  });
                           });
                       });

    BOOST_TEST_MESSAGE("Starting runners");
    IoContextRunner pubRunner(pubIoContext);
    IoContextRunner subRunner(subIoContext);

    BOOST_TEST_MESSAGE("joining pubRunner");
    pubRunner.join();
    BOOST_TEST_MESSAGE("joining subRunner");
    subRunner.join();

    BOOST_TEST_MESSAGE("Checking results");

    BOOST_CHECK_EQUAL(cmdReceived,2);
    BOOST_CHECK(action1Correct.load());
    BOOST_CHECK(nodeId1Correct.load());
    BOOST_CHECK(action2Correct.load());

    BOOST_CHECK_EQUAL(numCallbacks,2);
    BOOST_CHECK(completion1Received.load());
    BOOST_CHECK(completion2Received.load());
}



BOOST_AUTO_TEST_CASE(two_senders_single_receiver_sequential_sends)
{
    BOOST_TEST_MESSAGE("two_senders_single_receiver_sequential_sends started");

    boost::asio::io_context subIoContext;
    boost::asio::io_context pubIoContext;

    auto pubWork = boost::asio::make_work_guard(pubIoContext);
    auto subWork = boost::asio::make_work_guard(subIoContext);

    IoContextRunner pubRunner(pubIoContext);
    IoContextRunner subRunner(subIoContext);

    std::atomic<bool> firstCmdReceived{false};
    std::atomic<bool> secondCmdReceived{false};
    std::atomic<bool> firstCompletion{false};
    std::atomic<bool> secondCompletion{false};

    std::atomic<bool> firstActionCorrect{false};
    std::atomic<bool> secondActionCorrect{false};

    std::error_code firstErr;
    std::error_code secondErr;

    // Long‑lived receiver
    std::unique_ptr<ControlCmdReceiver> cmdReceiver =
        std::make_unique<ControlCmdReceiver>(
            subIoContext,
            [&firstCmdReceived, &secondCmdReceived,
             &firstActionCorrect, &secondActionCorrect]
            (CommandAction cmdAction, int64_t nodeId)
            {
                if (nodeId == 1)
                {
                    firstCmdReceived.store(true, std::memory_order_release);
                    firstActionCorrect.store(cmdAction == REBOOT, std::memory_order_release);
                }
                else if (nodeId == 2)
                {
                    secondCmdReceived.store(true, std::memory_order_release);
                    secondActionCorrect.store(cmdAction == SHUTDOWN, std::memory_order_release);
                }
            });

    BOOST_TEST_MESSAGE("Starting receiver");
    cmdReceiver->Start();

    // Two separate sender instances using the same pubIoContext
    std::unique_ptr<ControlCmdSender> sender1 =
        std::make_unique<ControlCmdSender>(pubIoContext);
    std::unique_ptr<ControlCmdSender> sender2 =
        std::make_unique<ControlCmdSender>(pubIoContext);

    // 1) First sender: REBOOT to nodeId=1
    BOOST_TEST_MESSAGE("Sender1: Send REBOOT to node 1");
    sender1->SendCmd(
        REBOOT,
        1,
        std::chrono::milliseconds(10000),
        [&firstCompletion, &firstErr](const std::error_code& err)
        {
            firstErr = err;
            firstCompletion.store(true, std::memory_order_release);
        });

    // 2) From the test thread, wait for firstCompletion, then start second sender.
    for (int i = 0; i < 500 && !firstCompletion.load(std::memory_order_acquire); ++i)
    {
        boost::this_thread::sleep_for(boost::chrono::milliseconds(10));
    }

    BOOST_TEST_MESSAGE("Sender2: Send SHUTDOWN to node 2");
    sender2->SendCmd(
        SHUTDOWN,
        2,
        std::chrono::milliseconds(10000),
        [&secondCompletion, &secondErr](const std::error_code& err)
        {
            secondErr = err;
            secondCompletion.store(true, std::memory_order_release);
        });

    // 3) Wait for both completions and both receives
    for (int i = 0;
         i < 500 &&
         (!firstCompletion.load(std::memory_order_acquire) ||
          !secondCompletion.load(std::memory_order_acquire) ||
          !firstCmdReceived.load(std::memory_order_acquire) ||
          !secondCmdReceived.load(std::memory_order_acquire));
         ++i)
    {
        boost::this_thread::sleep_for(boost::chrono::milliseconds(10));
    }

    pubWork.reset();
    subWork.reset();


    BOOST_CHECK(firstCompletion.load());
    BOOST_CHECK(secondCompletion.load());
    BOOST_CHECK(firstCmdReceived.load());
    BOOST_CHECK(secondCmdReceived.load());

    BOOST_CHECK(!firstErr);
    BOOST_CHECK(!secondErr);

    BOOST_CHECK(firstActionCorrect.load());
    BOOST_CHECK(secondActionCorrect.load());

    BOOST_TEST_MESSAGE("two_senders_single_receiver_sequential_sends finished");
}
