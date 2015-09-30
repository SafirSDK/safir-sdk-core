/******************************************************************************
*
* Copyright Saab AB, 2015 (http://safirsdkcore.com)
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
#include "../../src/StopHandler.h"

#include <Safir/Dob/Internal/ControlCmd.h>

#include <boost/tuple/tuple.hpp>
#include <boost/tuple/tuple_comparison.hpp>

#ifdef _MSC_VER
#  pragma warning (push)
#  pragma warning (disable: 4244)
#  pragma warning (disable: 4127)
#  pragma warning (disable: 4100)
#endif

#include <boost/thread.hpp>
#include <boost/function.hpp>

#ifdef _MSC_VER
#  pragma warning (pop)
#endif

#define BOOST_TEST_MODULE StopHandlerTests
#include <boost/test/unit_test.hpp>

namespace Com = Safir::Dob::Internal::Com;

namespace Control = Safir::Dob::Internal::Control;


class Communication
{
public:

    Communication()
        : sendReturnValue(true),
          id(9999)

    {
        sendAction = [this](int64_t /*nodeId*/,
                            int64_t /*nodeTypeId*/,
                            const boost::shared_ptr<const char[]>& /*data*/,
                            size_t /*size*/,
                            int64_t /*dataTypeIdentifier*/,
                            bool /*deliveryGuarantee*/) -> bool
                            {
                                return true;
                            };
    }

    void SetDataReceiver(const Com::ReceiveData& callback,
                         int64_t dataTypeIdentifier,
                         const Com::Allocator& /*allocator*/,
                         const Com::DeAllocator& /*deAllocator*/)
    {
        setDataReceiverCalls.push_back(std::make_pair(callback, dataTypeIdentifier));
    }
    typedef std::vector<std::pair<Com::ReceiveData, int64_t>> SetDataReceiverCalls;

    SetDataReceiverCalls setDataReceiverCalls;

    boost::function<bool(int64_t, int64_t, const boost::shared_ptr<const char[]>&, size_t, int64_t, bool)> sendAction;

    bool Send(int64_t nodeId,
              int64_t nodeTypeId,
              const boost::shared_ptr<const char[]>& data,
              size_t size,
              int64_t dataTypeIdentifier,
              bool deliveryGuarantee)
    {
       return sendAction(nodeId, nodeTypeId, data, size, dataTypeIdentifier, deliveryGuarantee);
    }

    typedef std::vector<boost::tuple<int64_t, int64_t, boost::shared_ptr<const char[]>, size_t, int64_t, bool> > SendCalls;

    SendCalls sendCalls;
    bool sendReturnValue;

    int64_t Id() const
    {
        return id;
    }
    int64_t id;

};

class SP
{
public:

    void ExcludeNode(int64_t nodeId)
    {
        excludedNodes.push_back(nodeId);
    }
    std::vector<int64_t> excludedNodes;

};

typedef std::function<void(Control::CommandAction cmdAction, int64_t nodeId)> ControlCmdCb;
ControlCmdCb controlCmdCallback;
unsigned int controlCmdStartCalls;
unsigned int controlCmdStopCalls;

class ControlCmdReceiver
{
public:

    ControlCmdReceiver(boost::asio::io_service& /*ioService*/,
                       const ControlCmdCb&      cmdCb)
    {
        controlCmdCallback = cmdCb;
    }

    void Start()
    {
        ++controlCmdStartCalls;
    }
    unsigned int startCalls;

    void Stop()
    {
        ++controlCmdStopCalls;
    }
};

class DoseMainCmdSender
{
public:

    void StoppedNodeIndication(int64_t nodeId)
    {
        stoppedNodeIndications.push_back(nodeId);
    }
    std::vector<int64_t> stoppedNodeIndications;

};

class ControlConfig
{
public:

    struct NodeType
    {
        NodeType(const boost::int64_t id_,
                 const int heartbeatInterval_,
                 const int maxLostHeartbeats_)

            : id(id_),
              heartbeatInterval(heartbeatInterval_),
              maxLostHeartbeats(maxLostHeartbeats_)

        {}

        boost::int64_t id;
        int heartbeatInterval;
        int maxLostHeartbeats;
    };

    std::vector<NodeType> nodeTypesParam;
};

struct Fixture
{
    Fixture()
        : stopSafirNodeCb(0),
          shutdownCb(0),
          rebootCb(0),
          stopSystemCb(0),
          stopNotificationMsgTypeId(LlufId_Generate64("Safir.Dob.Control.StopNotificationMsgTypeId")),
          stopOrderMsgTypeId(LlufId_Generate64("Safir.Dob.Control.StopOrderMsgTypeId"))

    {
        controlConfig.nodeTypesParam.push_back(ControlConfig::NodeType(1111, 1000, 5));
        controlConfig.nodeTypesParam.push_back(ControlConfig::NodeType(2222, 700, 5));
        controlConfig.nodeTypesParam.push_back(ControlConfig::NodeType(3333, 20000, 5));

        stopHandler.reset(new StopHandler(ioService,
                                          communication,
                                          sp,
                                          doseMainCmdSender,
                                          controlConfig,
                                          [this]() {++stopSafirNodeCb;},
                                          [this]() {++shutdownCb;},
                                          [this]() {++rebootCb;},
                                          [this]() {++stopSystemCb;},
                                          false));
    }

    Communication       communication;
    SP                  sp;
    DoseMainCmdSender   doseMainCmdSender;
    ControlConfig       controlConfig;

    typedef Control::StopHandlerBasic<Communication, SP, ControlCmdReceiver, DoseMainCmdSender, ControlConfig> StopHandler;

    boost::asio::io_service ioService;

    unsigned int stopSafirNodeCb;
    unsigned int shutdownCb;
    unsigned int rebootCb;
    unsigned int stopSystemCb;

    int64_t stopNotificationMsgTypeId;
    int64_t stopOrderMsgTypeId;

    std::unique_ptr<StopHandler> stopHandler;

    void RunIoService(int numThreads = 1)
    {
        ioService.reset();

        boost::thread_group threads;
        for (int i = 0; i < numThreads - 1; ++i)
        {
            threads.create_thread([this]{ioService.run();});
        }

        ioService.run();
        threads.join_all();
    }

};

BOOST_FIXTURE_TEST_SUITE( s, Fixture )


BOOST_AUTO_TEST_CASE( stop_external_node )
{
    stopHandler->Start();
    stopHandler->AddNode(1234, 1111);
    ioService.post([this](){controlCmdCallback(Control::STOP, 1234);});

    communication.sendAction = [this](int64_t nodeId,
                                      int64_t nodeTypeId,
                                      const boost::shared_ptr<const char[]>& data,
                                      size_t size,
                                      int64_t dataTypeIdentifier,
                                      bool deliveryGuarantee) -> bool
    {
        BOOST_CHECK(nodeId == 1234);
        BOOST_CHECK(nodeTypeId == 1111);

        auto cmd = Control::DeserializeCmd(data.get(), size);
        BOOST_CHECK(cmd.first == Control::STOP);
        BOOST_CHECK(cmd.second == 1234);

        BOOST_CHECK(dataTypeIdentifier == stopOrderMsgTypeId);
        BOOST_CHECK(deliveryGuarantee == true);

        stopHandler->RemoveNode(1234);
        return true;
    };

    RunIoService();

    BOOST_CHECK(stopSafirNodeCb == 0);
    BOOST_CHECK(shutdownCb == 0);
    BOOST_CHECK(rebootCb == 0);
    BOOST_CHECK(stopSystemCb == 0);
}

BOOST_AUTO_TEST_CASE( stop_non_existing_external_node )
{
    stopHandler->Start();
    stopHandler->AddNode(1234, 1111);
    ioService.post([this](){controlCmdCallback(Control::STOP, 5678);});

    auto nbrOfSend = 0;
    communication.sendAction = [this, &nbrOfSend](int64_t nodeId,
                                                  int64_t nodeTypeId,
                                                  const boost::shared_ptr<const char[]>& data,
                                                  size_t size,
                                                  int64_t dataTypeIdentifier,
                                                  bool deliveryGuarantee) -> bool
    {
        ++nbrOfSend;
        return true;
    };

    RunIoService();

    BOOST_CHECK(nbrOfSend == 0);
    BOOST_CHECK(stopSafirNodeCb == 0);
    BOOST_CHECK(shutdownCb == 0);
    BOOST_CHECK(rebootCb == 0);
    BOOST_CHECK(stopSystemCb == 0);
}

BOOST_AUTO_TEST_CASE( shutdown_external_node_resending )
{
    // Check that resending of shutdown command will be performed

    stopHandler->Start();
    stopHandler->AddNode(1234, 1111);
    stopHandler->AddNode(5678, 2222);
    ioService.post([this](){controlCmdCallback(Control::SHUTDOWN, 5678);});

    auto nbrOfSend = 0;
    communication.sendAction = [this, &nbrOfSend](int64_t nodeId,
                                                  int64_t nodeTypeId,
                                                  const boost::shared_ptr<const char[]>& data,
                                                  size_t size,
                                                  int64_t dataTypeIdentifier,
                                                  bool deliveryGuarantee) -> bool
    {
        BOOST_CHECK(nodeId == 5678);
        BOOST_CHECK(nodeTypeId == 2222);

        auto cmd = Control::DeserializeCmd(data.get(), size);
        BOOST_CHECK(cmd.first == Control::SHUTDOWN);
        BOOST_CHECK(cmd.second == 5678);

        BOOST_CHECK(dataTypeIdentifier == stopOrderMsgTypeId);
        BOOST_CHECK(deliveryGuarantee == true);

        ++nbrOfSend;

        if (nbrOfSend == 3)
        {
            stopHandler->RemoveNode(5678);
        }

        return true;
    };

    RunIoService();

    BOOST_CHECK(nbrOfSend == 3);
    BOOST_CHECK(stopSafirNodeCb == 0);
    BOOST_CHECK(shutdownCb == 0);
    BOOST_CHECK(rebootCb == 0);
    BOOST_CHECK(stopSystemCb == 0);
}

BOOST_AUTO_TEST_CASE( stop_own_node )
{
    stopHandler->Start();
    stopHandler->AddNode(1234, 1111);
    stopHandler->AddNode(5678, 2222);
    ioService.post([this](){controlCmdCallback(Control::STOP, 9999);});

    auto nbrOfSend = 0;
    communication.sendAction = [this, &nbrOfSend](int64_t nodeId,
                                                  int64_t nodeTypeId,
                                                  const boost::shared_ptr<const char[]>& /*data*/,
                                                  size_t /*size*/,
                                                  int64_t dataTypeIdentifier,
                                                  bool deliveryGuarantee) -> bool
    {
        BOOST_CHECK(nodeId == 0);
        BOOST_CHECK(nodeTypeId == 1111 || nodeTypeId == 2222);

        BOOST_CHECK(dataTypeIdentifier == stopNotificationMsgTypeId);
        BOOST_CHECK(deliveryGuarantee == false);

        ++nbrOfSend;

        return true;
    };

    RunIoService();

    BOOST_CHECK(nbrOfSend == 4); // 2 node types, 2 messages each
    BOOST_CHECK(stopSafirNodeCb == 1);
    BOOST_CHECK(shutdownCb == 0);
    BOOST_CHECK(rebootCb == 0);
    BOOST_CHECK(stopSystemCb == 0);
}

BOOST_AUTO_TEST_CASE( stop_own_node_while_pending_extrnal_node_stops  )
{
    // Check that remote nodes are stopped before going down

    stopHandler->Start();
    stopHandler->AddNode(1234, 1111);
    ioService.post([this](){controlCmdCallback(Control::STOP, 1234);});  // stop external node
    ioService.post([this](){controlCmdCallback(Control::STOP, 9999);});  // stop own node

    auto nbrOfSend = 0;
    communication.sendAction = [this, &nbrOfSend](int64_t nodeId,
                                                  int64_t nodeTypeId,
                                                  const boost::shared_ptr<const char[]>& data,
                                                  size_t size,
                                                  int64_t dataTypeIdentifier,
                                                  bool deliveryGuarantee) -> bool
    {
        BOOST_CHECK(nodeId == 1234);
        BOOST_CHECK(nodeTypeId == 1111);

        auto cmd = Control::DeserializeCmd(data.get(), size);
        BOOST_CHECK(cmd.first == Control::STOP);
        BOOST_CHECK(cmd.second == 1234);

        BOOST_CHECK(dataTypeIdentifier == stopOrderMsgTypeId);
        BOOST_CHECK(deliveryGuarantee == true);

        ++nbrOfSend;

        if (nbrOfSend == 3)
        {
            stopHandler->RemoveNode(1234);
        }
        return true;
    };

    RunIoService();

    BOOST_CHECK(nbrOfSend == 3);
    BOOST_CHECK(stopSafirNodeCb == 1);
    BOOST_CHECK(shutdownCb == 0);
    BOOST_CHECK(rebootCb == 0);
    BOOST_CHECK(stopSystemCb == 0);
}

BOOST_AUTO_TEST_CASE( stop_system )
{
    stopHandler->Start();
    stopHandler->AddNode(1234, 1111);
    stopHandler->AddNode(5678, 2222);
    ioService.post([this](){controlCmdCallback(Control::STOP, 0);});

    auto nbrOfSentStopNotifications = 0;
    auto nbrOfSentStopOrders = 0;

    communication.sendAction = [this,
                               &nbrOfSentStopNotifications,
                               &nbrOfSentStopOrders]
                               (int64_t nodeId,
                                int64_t nodeTypeId,
                                const boost::shared_ptr<const char[]>& data,
                                size_t size,
                                int64_t dataTypeIdentifier,
                                bool deliveryGuarantee) -> bool
    {
        if (dataTypeIdentifier == stopOrderMsgTypeId)
        {
            BOOST_CHECK(nodeId == 0);
            BOOST_CHECK(nodeTypeId == 1111 || nodeTypeId == 2222);

            auto cmd = Control::DeserializeCmd(data.get(), size);
            BOOST_CHECK(cmd.first == Control::STOP);
            BOOST_CHECK(cmd.second == 0);

            BOOST_CHECK(deliveryGuarantee == true);
            ++nbrOfSentStopOrders;

            if (nbrOfSentStopOrders == 1)
            {
                stopHandler->RemoveNode(5678);
                stopHandler->AddNode(6767, 3333); // add node while in system stop phase
            }
            else if (nbrOfSentStopOrders == 3)
            {

                stopHandler->RemoveNode(1234);
                stopHandler->RemoveNode(6767);
            }
        }
        else if (dataTypeIdentifier == stopNotificationMsgTypeId)
        {
            ++nbrOfSentStopNotifications;
        }
        else
        {
            BOOST_FAIL("Unexpected dataTypeIdentifier");
        }
        return true;
    };

    RunIoService();

    BOOST_CHECK(nbrOfSentStopOrders == 3);
    BOOST_CHECK(nbrOfSentStopNotifications == 0); // all other nodes are gone before this node dies
    BOOST_CHECK(stopSafirNodeCb == 1);
    BOOST_CHECK(shutdownCb == 0);
    BOOST_CHECK(rebootCb == 0);
    BOOST_CHECK(stopSystemCb == 1);
}

BOOST_AUTO_TEST_CASE( stop_system_external_nodes_unresponsive )
{
    stopHandler->Start();
    stopHandler->AddNode(1234, 1111);
    stopHandler->AddNode(5678, 2222);
    ioService.post([this](){controlCmdCallback(Control::STOP, 0);});

    auto nbrOfSentStopNotifications = 0;
    auto nbrOfSentStopOrders = 0;

    communication.sendAction = [this,
                               &nbrOfSentStopNotifications,
                               &nbrOfSentStopOrders]
                               (int64_t nodeId,
                                int64_t nodeTypeId,
                                const boost::shared_ptr<const char[]>& data,
                                size_t size,
                                int64_t dataTypeIdentifier,
                                bool deliveryGuarantee) -> bool
    {
        if (dataTypeIdentifier == stopOrderMsgTypeId)
        {
            BOOST_CHECK(nodeId == 0);
            BOOST_CHECK(nodeTypeId == 1111 || nodeTypeId == 2222);

            auto cmd = Control::DeserializeCmd(data.get(), size);
            BOOST_CHECK(cmd.first == Control::STOP);
            BOOST_CHECK(cmd.second == 0);

            BOOST_CHECK(deliveryGuarantee == true);
            ++nbrOfSentStopOrders;
        }
        else if (dataTypeIdentifier == stopNotificationMsgTypeId)
        {
            BOOST_CHECK(nodeId == 0);
            BOOST_CHECK(nodeTypeId == 1111 || nodeTypeId == 2222);
            BOOST_CHECK(deliveryGuarantee == false);

            ++nbrOfSentStopNotifications;
        }
        else
        {
            BOOST_FAIL("Unexpected dataTypeIdentifier");
        }
        return true;
    };

    RunIoService();

    BOOST_CHECK(nbrOfSentStopOrders > 2);
    BOOST_CHECK(nbrOfSentStopNotifications == 4); // 2 node types, 2 messages each
    BOOST_CHECK(stopSafirNodeCb == 1);
    BOOST_CHECK(shutdownCb == 0);
    BOOST_CHECK(rebootCb == 0);
    BOOST_CHECK(stopSystemCb == 1);
}

BOOST_AUTO_TEST_CASE( receive_node_stop_cmd )
{
    stopHandler->Start();

    ioService.post([this]()
                   {
                       BOOST_CHECK(communication.setDataReceiverCalls.size() == 2);

                       auto stopOrder = Control::SerializeCmd(Control::STOP, communication.Id());

                       communication.setDataReceiverCalls[0].first(1234, 1111, stopOrder.first.get(), stopOrder.second);

                       stopOrder.first.release();
                   });

    RunIoService();

    BOOST_CHECK(sp.excludedNodes.size() == 0);
    BOOST_CHECK(stopSafirNodeCb == 1);
    BOOST_CHECK(shutdownCb == 0);
    BOOST_CHECK(rebootCb == 0);
    BOOST_CHECK(stopSystemCb == 0);
}

BOOST_AUTO_TEST_CASE( receive_system_stop_cmd )
{
    stopHandler->Start();

    ioService.post([this]()
                   {
                       BOOST_CHECK(communication.setDataReceiverCalls.size() == 2);

                       auto stopOrder = Control::SerializeCmd(Control::STOP, 0);

                       communication.setDataReceiverCalls[0].first(1234, 1111, stopOrder.first.get(), stopOrder.second);

                       stopOrder.first.release();
                   });

    RunIoService();

    BOOST_CHECK(sp.excludedNodes.size() == 0);
    BOOST_CHECK(stopSafirNodeCb == 1);
    BOOST_CHECK(shutdownCb == 0);
    BOOST_CHECK(rebootCb == 0);
    BOOST_CHECK(stopSystemCb == 1);
}

BOOST_AUTO_TEST_CASE( system_stop_cmd_while_in_system_stop_mode )
{
    // Test that a received system stop from a node with a higher node id than ourself
    // is acted upon even if in "system stop mode"
    stopHandler->Start();
    stopHandler->AddNode(1234, 1111);
    stopHandler->AddNode(5678, 2222);
    ioService.post([this](){controlCmdCallback(Control::STOP, 0);});

    auto nbrOfSentStopNotifications = 0;
    auto nbrOfSentStopOrders = 0;

    communication.sendAction = [this,
                               &nbrOfSentStopNotifications,
                               &nbrOfSentStopOrders]
                               (int64_t nodeId,
                                int64_t nodeTypeId,
                                const boost::shared_ptr<const char[]>& data,
                                size_t size,
                                int64_t dataTypeIdentifier,
                                bool deliveryGuarantee) -> bool
    {
        if (dataTypeIdentifier == stopOrderMsgTypeId)
        {
            ++nbrOfSentStopOrders;

            if (nbrOfSentStopOrders == 1)
            {
                auto this_ = this; //fix for vs2010 issues with lambdas

                // System stop from node with higher node id ...should NOT be ignored
                ioService.post([this_]
                               {
                                   auto stopOrder = Control::SerializeCmd(Control::STOP, 0);
                                   this_->communication.setDataReceiverCalls[0].first(9999999999, 1111, stopOrder.first.get(), stopOrder.second);
                                   stopOrder.first.release();
                               });
            }
        }
        else if (dataTypeIdentifier == stopNotificationMsgTypeId)
        {
            ++nbrOfSentStopNotifications;
        }
        else
        {
            BOOST_FAIL("Unexpected dataTypeIdentifier");
        }
        return true;
    };

    RunIoService();

    BOOST_CHECK(nbrOfSentStopOrders == 2);
    BOOST_CHECK(nbrOfSentStopNotifications == 4);
    BOOST_CHECK(stopSafirNodeCb == 1);
    BOOST_CHECK(shutdownCb == 0);
    BOOST_CHECK(rebootCb == 0);
    BOOST_CHECK(stopSystemCb == 1);
}

BOOST_AUTO_TEST_CASE( ignored_system_stop_cmd_while_in_system_stop_mode )
{
    // Test that a received system stop from a node with a lower node id than ourself
    // is ignored if we are in "system stop mode"
    stopHandler->Start();
    stopHandler->AddNode(1234, 1111);
    stopHandler->AddNode(5678, 2222);
    ioService.post([this](){controlCmdCallback(Control::STOP, 0);});

    auto nbrOfSentStopNotifications = 0;
    auto nbrOfSentStopOrders = 0;

    communication.sendAction = [this,
                               &nbrOfSentStopNotifications,
                               &nbrOfSentStopOrders]
                               (int64_t nodeId,
                                int64_t nodeTypeId,
                                const boost::shared_ptr<const char[]>& data,
                                size_t size,
                                int64_t dataTypeIdentifier,
                                bool deliveryGuarantee) -> bool
    {
        if (dataTypeIdentifier == stopOrderMsgTypeId)
        {
            ++nbrOfSentStopOrders;

            if (nbrOfSentStopOrders == 1)
            {
                auto this_ = this; //fix for vs2010 issues with lambdas

                ioService.post([this_]
                               {
                                   auto stopOrder = Control::SerializeCmd(Control::STOP, 0);
                                   // System stop from node with lower node id ...should be ignored
                                   this_->communication.setDataReceiverCalls[0].first(1234, 1111, stopOrder.first.get(), stopOrder.second);
                                   stopOrder.first.release();
                               });
            }
            else if (nbrOfSentStopOrders == 2)
            {

                stopHandler->RemoveNode(1234);
                stopHandler->RemoveNode(5678);
            }
        }
        else if (dataTypeIdentifier == stopNotificationMsgTypeId)
        {
            ++nbrOfSentStopNotifications;
        }
        else
        {
            BOOST_FAIL("Unexpected dataTypeIdentifier");
        }
        return true;
    };

    RunIoService();

    BOOST_CHECK(nbrOfSentStopOrders == 2);
    BOOST_CHECK(nbrOfSentStopNotifications == 0); // all other nodes are gone before this node dies
    BOOST_CHECK(stopSafirNodeCb == 1);
    BOOST_CHECK(shutdownCb == 0);
    BOOST_CHECK(rebootCb == 0);
    BOOST_CHECK(stopSystemCb == 1);
}

BOOST_AUTO_TEST_CASE( receive_stop_notification )
{
    stopHandler->Start();

    ioService.post([this]()
                   {
                       BOOST_CHECK(communication.setDataReceiverCalls.size() == 2);

                       communication.setDataReceiverCalls[1].first(1234, 1111, new char[1], 1);
                   });

    RunIoService();

    BOOST_CHECK(sp.excludedNodes.size() == 1);
    BOOST_CHECK(stopSafirNodeCb == 0);
    BOOST_CHECK(shutdownCb == 0);
    BOOST_CHECK(rebootCb == 0);
    BOOST_CHECK(stopSystemCb == 0);
}

BOOST_AUTO_TEST_SUITE_END()
