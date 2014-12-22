/******************************************************************************
*
* Copyright Saab AB, 2014 (http://safir.sourceforge.net)
* Copyright Consoden AB, 2014 (http://www.consoden.se)
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
#include <Safir/Dob/Internal/Communication.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/SystemLog.h>
#include <Safir/Utilities/Internal/MakeUnique.h>
#include <Safir/Dob/Internal/SystemPicture.h>
#include <Safir/Utilities/Internal/Id.h>
#include <Safir/Dob/Internal/ControlConfig.h>
#include <Safir/Dob/Internal/DoseMainCmd.h>
#include <iostream>
#include <map>
#include <boost/thread.hpp>

//disable warnings in boost
#if defined _MSC_VER
#  pragma warning (push)
#  pragma warning (disable : 4100 4267)
#endif

#include <boost/program_options.hpp>
#include <boost/asio.hpp>
#include <boost/asio/steady_timer.hpp>

#if defined _MSC_VER
#  pragma warning (pop)
#endif

namespace SP = Safir::Dob::Internal::SP;
namespace Com = Safir::Dob::Internal::Com;
namespace Control = Safir::Dob::Internal::Control;

std::wostream& operator<<(std::wostream& out, const std::string& str)
{
    return out << str.c_str();
}

std::wostream& operator<<(std::wostream& out, const boost::program_options::options_description& opt)
{
    std::ostringstream ostr;
    ostr << opt;
    return out << ostr.str().c_str();
}


// Communication callbacks
void QueueNotFullCb(int64_t nodeTypeId)
{
    lllog(3) << "DOSE_MAIN: Got queue not full indication for nodetypeId " << nodeTypeId << std::endl;
}

boost::shared_ptr<char[]> StrToPtr(const std::string& s)
{
    boost::shared_ptr<char[]> ptr(new char[s.length()]);

    memcpy(ptr.get(), s.c_str(), s.length());
    return ptr;
}

int main(int /*argc*/, char * /*argv*/[])
{
    lllog(3) << "DOSE_MAIN: Started" << std::endl;

    const Safir::Dob::Internal::Control::Config config;

    bool running = true;

    boost::asio::io_service ioService;

    boost::asio::steady_timer timer(ioService);

    boost::asio::signal_set signalSet(ioService);

    // Make some work to stop io_service from exiting.
    auto work = Safir::make_unique<boost::asio::io_service::work>(ioService);

    // Create and populate structures that are needed when creating the Communication and
    // SP instances.
    std::vector<Com::NodeTypeDefinition> commNodeTypes;
    std::map<boost::int64_t, SP::NodeType> spNodeTypes;

    for (const auto& nt: config.GetNodeTypes())
    {
        commNodeTypes.push_back({nt.id, 
                                 nt.name,
                                 nt.multicastAddressControl,
                                 nt.multicastAddressData,
                                 nt.heartbeatInterval,
                                 nt.retryTimeout});

        spNodeTypes.insert(std::make_pair(nt.id,
                                          SP::NodeType(nt.id,
                                                       nt.name,
                                                       nt.isLight,
                                                       boost::chrono::milliseconds(nt.heartbeatInterval),
                                                       nt.maxLostHeartbeats,
                                                       boost::chrono::milliseconds(nt.retryTimeout))));
    }
    
    std::unique_ptr<Com::Communication> communication;
    std::unique_ptr<SP::SystemPicture> sp;

    int counter = 0;

    std::function<void (const boost::system::error_code&)> onTimeout =
            [&running, &timer, &onTimeout, &communication, &counter, &config]
            (const boost::system::error_code& error)
            {
               if (!running)
               {
                   return;
               }

               if (error)
               {
                   lllog(3) << "DOSE_MAIN: OnTimeout error!" << std::endl;
               }

               std::ostringstream output;
               output << "Kalle" << counter;
               ++counter;

               // Send message to all node types
               for (const auto& nt: config.GetNodeTypes())
               {
                   communication->Send(0, // Send to all nodes of this type
                                       nt.id,
                                       StrToPtr(output.str()),
                                       output.str().length(),
                                       12345,
                                       true);
               }
               timer.expires_from_now(boost::chrono::milliseconds(2000));
               timer.async_wait(onTimeout);
            };

    std::unique_ptr<Control::DoseMainCmdReceiver> doseMainCmdReceiver;

    doseMainCmdReceiver.reset(new Control::DoseMainCmdReceiver
                        (ioService,

                         // Action when InjectOwnNode command is received
                         [&communication, &sp, &ioService, &commNodeTypes, &spNodeTypes, &timer, &onTimeout]
                         (int64_t /*requestId*/,
                          const std::string& nodeName,
                          int64_t nodeId,
                          int64_t nodeTypeId,
                          const std::string& dataAddress)
                         {
                             // Create the communication instance...
                             communication.reset(new Com::Communication(Com::dataModeTag,
                                                                        ioService,
                                                                        nodeName,
                                                                        nodeId,
                                                                        nodeTypeId,
                                                                        dataAddress,
                                                                        commNodeTypes));
                             // ... and the System Picture instance
                             sp.reset(new SP::SystemPicture(SP::slave_tag,
                                                            ioService,
                                                            *communication,
                                                            nodeName,
                                                            nodeId,
                                                            nodeTypeId,
                                                            dataAddress,
                                                            spNodeTypes));

                             communication->SetDataReceiver
                                     ([](int64_t fromNodeId,
                                         int64_t fromNodeType,
                                         const boost::shared_ptr<char[]>& data,
                                         size_t size)
                                        {
                                            std::string msg(data.get(), size);
                                            lllog(3) << "DOSE_MAIN: Received " << msg << " from Node Id " << fromNodeId
                                                     << " of Node Type " << fromNodeType << std::endl;
                                        },
                                        12345);

                             communication->Start();

                             // Start cyclic sending of messages
                             timer.expires_from_now(boost::chrono::milliseconds(2000));
                             timer.async_wait(onTimeout);
                         },

                        // Action when InjectNode command is received
                        [&communication]
                        (int64_t /*requestId*/,
                         const std::string& nodeName,
                         int64_t nodeId,
                         int64_t nodeTypeId,
                         const std::string& dataAddress)
                        {
                           communication->InjectNode(nodeName,
                                                     nodeId,
                                                     nodeTypeId,
                                                     dataAddress);

                        },

                        // Action when StopDoseMain command is received
                        [&sp, &work, &communication, &running, &doseMainCmdReceiver, &signalSet]
                        (int64_t /*requestId*/)
                        {
                            lllog(0) << "DOSE_MAIN: Got stop command" << std::endl;
                            signalSet.cancel();
                            sp->Stop();
                            communication->Stop();
                            doseMainCmdReceiver->Stop();
                            work.reset();
                            running = false;
                        }
                        ));

    // Start reception of commands
    doseMainCmdReceiver->Start();

#if defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
    signalSet.add(SIGABRT);
    signalSet.add(SIGBREAK);
    signalSet.add(SIGINT);
    signalSet.add(SIGTERM);
#elif defined(linux) || defined(__linux) || defined(__linux__)
    signalSet.add(SIGQUIT);
    signalSet.add(SIGINT);
    signalSet.add(SIGTERM);
#endif

    signalSet.async_wait([&sp, &work, &communication, &running, &doseMainCmdReceiver]
                         (const boost::system::error_code& error,
                          const int signalNumber)
                         {
                             lllog(0) << "DOSE_MAIN: got signal " << signalNumber  << std::endl;

                             if (error)
                             {
                                 if (error == boost::asio::error::operation_aborted)
                                 {
                                     // We probably got a stop command which canceled all waiting operations,
                                     // do nothing.
                                 }
                                 else
                                 {
                                    SEND_SYSTEM_LOG(Error,
                                                     << "DOSE_MAIN: Got a signals error: " << error);
                                 }
                             }

                             sp->Stop();
                             communication->Stop();
                             doseMainCmdReceiver->Stop();
                             work.reset();
                             running = false;
                         }
                         );

    boost::thread_group threads;
    for (int i = 0; i < 9; ++i)
    {
        threads.create_thread([&ioService]{ioService.run();});
    }

    ioService.run();

    threads.join_all();

    lllog(0) << "DOSE_MAIN: Exiting..." << std::endl;
    return 0;
}
