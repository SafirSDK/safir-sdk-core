/******************************************************************************
*
* Copyright Saab AB, 2022 (http://safirsdkcore.com)
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
#include <Safir/Dob/Internal/ControlConfig.h>
#include <Safir/Dob/Internal/DoseMainCmd.h>
#include <Safir/Dob/Internal/SystemPicture.h>
#include <Safir/Utilities/Internal/Id.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/SharedCharArray.h>
#include <Safir/Utilities/Internal/MakeUnique.h>
#include <Safir/Utilities/Internal/SystemLog.h>
#include <iostream>
#include <map>

//disable warnings in boost
#if defined _MSC_VER
#  pragma warning (push)
#  pragma warning (disable : 4100 4267)
#endif

#include <boost/program_options.hpp>
#include <boost/asio.hpp>
#include <boost/asio/steady_timer.hpp>
#include <boost/thread.hpp>

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

Safir::Utilities::Internal::SharedCharArray StrToPtr(const std::string& s)
{
    Safir::Utilities::Internal::SharedCharArray ptr = Safir::Utilities::Internal::MakeSharedArray(s.length());;

    memcpy(ptr.get(), s.c_str(), s.length());
    return ptr;
}

int main(int /*argc*/, char * /*argv*/[])
{
    lllog(3) << "DOSE_MAIN: Started" << std::endl;

    const Safir::Dob::Internal::Control::Config config;

    bool running = true;

    boost::asio::io_context io;

    boost::asio::steady_timer timer(io);

    boost::asio::signal_set signalSet(io);

    // Make some work to stop io_service from exiting.
    auto work = boost::asio::make_work_guard(io);

    // Map so that we can use the node name in printouts
    std::map<int64_t, std::string> nodeNameMap;

    // Create and populate structures that are needed when creating the Communication and
    // SP instances.
    std::vector<Com::NodeTypeDefinition> commNodeTypes;
    std::map<std::int64_t, SP::NodeType> spNodeTypes;


    for (auto nt = config.nodeTypesParam.cbegin(); nt != config.nodeTypesParam.cend(); ++nt)
    {
        commNodeTypes.push_back( Com::NodeTypeDefinition(nt->id,
                                                         nt->name,
                                                         nt->multicastAddressControl,
                                                         nt->multicastAddressData,
                                                         nt->isLightNode,
                                                         nt->heartbeatInterval,
                                                         nt->maxLostHeartbeats,
                                                         nt->slidingWindowSize,
                                                         nt->ackRequestThreshold,
                                                         nt->retryTimeout));

        std::vector<std::chrono::steady_clock::duration> retryTimeouts;
        for (auto rt = nt->retryTimeout.cbegin(); rt != nt->retryTimeout.cend(); ++rt)
        {
            retryTimeouts.push_back(std::chrono::milliseconds(*rt));
        }

        spNodeTypes.insert(std::make_pair(nt->id,
                                          SP::NodeType(nt->id,
                                                       nt->name,
                                                       nt->isLightNode,
                                                       std::chrono::milliseconds(nt->heartbeatInterval),
                                                       nt->maxLostHeartbeats,
                                                       retryTimeouts)));
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
               for (auto nt = config.nodeTypesParam.cbegin(); nt != config.nodeTypesParam.cend(); ++nt)
               {
                   communication->Send(0, // Send to all nodes of this type
                                       nt->id,
                                       StrToPtr(output.str()),
                                       output.str().length(),
                                       12345,
                                       true);
               }
               timer.expires_after(std::chrono::milliseconds(2000));
               timer.async_wait(onTimeout);
            };

    std::unique_ptr<Control::DoseMainCmdReceiver> doseMainCmdReceiver;

    doseMainCmdReceiver.reset(new Control::DoseMainCmdReceiver
                        (io,

                         // Action when SetOwnNode command is received
                         [&communication, &sp, &io, &commNodeTypes,
                          &spNodeTypes, &timer, &onTimeout, &nodeNameMap]
                         (const std::string& nodeName,
                          int64_t nodeId,
                          int64_t nodeTypeId,
                          const std::string& dataAddress)
                         {
                             // Create the communication instance...
                             communication.reset(new Com::Communication(Com::dataModeTag,
                                                                        io,
                                                                        nodeName,
                                                                        nodeId,
                                                                        nodeTypeId,
                                                                        Safir::Dob::Internal::Com::ResolvedAddress(dataAddress),
                                                                        commNodeTypes,
                                                                        1450));
                             // ... and the System Picture instance
                             sp.reset(new SP::SystemPicture(SP::slave_tag,
                                                            io,
                                                            *communication,
                                                            nodeName,
                                                            nodeId,
                                                            nodeTypeId,
                                                            spNodeTypes));

                             auto & nodeNameMap_ = nodeNameMap; //fix for vs2010 which cant capture a variable "through two lambdas"
                             auto & spNodeTypes_ = spNodeTypes;

                             communication->SetDataReceiver
                                 ([&nodeNameMap_, &spNodeTypes_]
                                  (int64_t fromNodeId,
                                   int64_t fromNodeType,
                                   const char* const data_,
                                   size_t size)
                                  {
                                      const Safir::Utilities::Internal::SharedConstCharArray data(data_);
                                      std::string msg(data.get(), size);
                                      std::ostringstream os;
                                      os << "DOSE_MAIN: Received " << msg
                                         << " from Node " << nodeNameMap_[fromNodeId]
                                         << " of Node Type " << spNodeTypes_.find(fromNodeType)->second.name;
                                      std::cout << os.str() << std::endl;
                                      lllog(3) << os.str() << std::endl;
                                  },
                                  12345,
                                  [](size_t size){return new char[size];},
                                  [](const char * data){delete[] data;});

                             communication->Start();

                             // Start cyclic sending of messages
                             timer.expires_after(std::chrono::milliseconds(2000));
                             timer.async_wait(onTimeout);
                         },

                        // Action when InjectNode command is received
                        [&communication, &nodeNameMap]
                        (const std::string& nodeName,
                         int64_t nodeId,
                         int64_t nodeTypeId,
                         const std::string& dataAddress)
                        {
                           communication->InjectNode(nodeName,
                                                     nodeId,
                                                     nodeTypeId,
                                                     dataAddress);

                           nodeNameMap.insert(std::make_pair(nodeId, nodeName));
                        },

                        // Action when ExcludeNode command is received
                        []
                        (int64_t /*nodeId*/, int64_t /*nodeTypeId*/)
                        {
                        },

                        // Action when StoppedNodeIndication command is received
                        []
                        (int64_t /*nodeId*/)
                        {
                        },

                        // Action when StopDoseMain command is received
                        [&sp, &work, &communication, &running, &doseMainCmdReceiver, &signalSet]
                        ()
                        {
                            lllog(1) << "DOSE_MAIN: Got stop command" << std::endl;
                            signalSet.cancel();
                            sp->Stop();
                            communication->Stop();
                            doseMainCmdReceiver->Stop();
                            work.reset();
                            running = false;
                        },

                        // Action when NodeState command is received
                        [](Safir::Dob::Internal::Control::NodeState)
                        {
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
                             lllog(1) << "DOSE_MAIN: got signal " << signalNumber  << std::endl;

                             if (error)
                             {
                                 if (error == boost::asio::error::operation_aborted)
                                 {
                                     // We probably got a stop command which canceled all waiting operations,
                                     // do nothing.
                                     return;
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
        threads.create_thread([&io]{io.run();});
    }

    io.run();

    threads.join_all();

    lllog(1) << "DOSE_MAIN: Exiting..." << std::endl;
    return 0;
}
