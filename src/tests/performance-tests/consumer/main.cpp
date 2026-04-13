/******************************************************************************
*
* Copyright Saab AB, 2026 (http://safirsdkcore.com)
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

/**
 * Performance test consumer skeleton.
 *
 * Connects to the Dob, subscribes to DoseTest.RootEntity (and all subclasses),
 * counts received callbacks and measures end-to-end throughput/latency.
 *
 * TODO: Implement actual measurement and result reporting.
 */

#include <Safir/Dob/Connection.h>
#include <Safir/Dob/Consumer.h>
#include <Safir/Dob/EntityProxy.h>
#include <Safir/Dob/ResponseProxy.h>
#include <Safir/Utilities/AsioDispatcher.h>
#include <Safir/Logging/Log.h>
#include <DoseTest/RootEntity.h>
#include <boost/asio.hpp>
#include <iostream>
#include <optional>
#include <string>
#include <unordered_map>

class Consumer
    : public Safir::Dob::StopHandler
    , public Safir::Dob::EntitySubscriber
    , public Safir::Dob::Requestor
{
public:
    int Run(int numInstances, int numUpdates)
    {
        m_numInstances = numInstances;
        m_numUpdates   = numUpdates;

        m_connection.Open(L"Consumer", L"0", 0, this, &m_dispatch);

        Safir::Logging::SendSystemLog(Safir::Logging::Informational,
                                      L"Consumer started");

        m_connection.SubscribeEntity(DoseTest::RootEntity::ClassTypeId, this);

        // Signal readiness to the driver script
        std::cout << "Consumer ready" << std::endl;

        m_work.emplace(m_ioContext.get_executor());
        m_ioContext.run();
        return 0;
    }

    // --- StopHandler ---
    void OnStopOrder() override
    {
        Safir::Logging::SendSystemLog(Safir::Logging::Informational,
                                      L"Consumer received stop order");
        m_ioContext.stop();
    }

    // --- Requestor ---
    void OnResponse(const Safir::Dob::ResponseProxy /*responseProxy*/) override
    {
        // This callback is expected to be called when the producer sends a delete request to signal that it's done and we can stop.
        boost::asio::post(m_ioContext, [this]()
        {
            std::cout << "Consumer finished" << std::endl;
            m_connection.Close();
            m_work.reset();
        });
    }
    void OnNotRequestOverflow() override {}

    // --- EntitySubscriber ---
    void OnNewEntity(const Safir::Dob::EntityProxy entityProxy) override
    {
        ++m_receivedCount;
        UpdateInstanceInfo(entityProxy);
    }

    void OnUpdatedEntity(const Safir::Dob::EntityProxy entityProxy) override
    {
        ++m_receivedCount;
        UpdateInstanceInfo(entityProxy);
    }

    void OnDeletedEntity(const Safir::Dob::EntityProxy entityProxy,
                         const bool                    /*deprecated*/) override
    {
        // TODO: handle deletion if relevant for the performance scenario
        (void)entityProxy;
    }

private:

    // void UpdateInstanceInfo(const Safir::Dob::EntityProxy& entityProxy)
    // {
    //     auto expected = static_cast<std::size_t>(m_numInstances + m_numInstances * m_numUpdates);
    //     if (m_receivedCount ==  expected)
    //     {
    //         std::cout << "Consumer received all updates. Total receive count: " << m_receivedCount << std::endl;
    //         const Safir::Dob::Typesystem::EntityId entityId{DoseTest::RootEntity::ClassTypeId, Safir::Dob::Typesystem::InstanceId(0)};
    //         m_connection.DeleteRequest(entityId, this);
    //     }
    // }
    
    void UpdateInstanceInfo(const Safir::Dob::EntityProxy& entityProxy)
    {
        if (m_doneCount == m_numInstances)
            return; // already finished

        auto entity = std::dynamic_pointer_cast<DoseTest::RootEntity>(entityProxy.GetEntity());
        if (!entity || entity->Info().IsNull())
            return;

        const int newVal = std::stoi(entity->Info().GetVal());
        const auto instanceId = entityProxy.GetEntityId().GetInstanceId().GetRawValue();

        auto& stored = m_instanceInfo[instanceId];
        if (newVal == m_numUpdates && stored != m_numUpdates)
        {
            ++m_doneCount;
            if (m_doneCount == m_numInstances)
            {
                std::cout << "Consumer received all updates. Total receive count: " << m_receivedCount << std::endl;

                // Send a dummy delete request to signal the producer that we're done and it can stop.
                const Safir::Dob::Typesystem::EntityId entityId{
                    DoseTest::RootEntity::ClassTypeId,
                    Safir::Dob::Typesystem::InstanceId(0)};
                m_connection.DeleteRequest(entityId, this);
            }
                
        }
        stored = newVal;
    }

    using WorkGuard = boost::asio::executor_work_guard<boost::asio::io_context::executor_type>;

    Safir::Dob::Connection                    m_connection;
    boost::asio::io_context                   m_ioContext;
    Safir::Utilities::AsioDispatcher          m_dispatch{m_connection, m_ioContext};
    std::optional<WorkGuard>                  m_work;
    std::size_t                               m_receivedCount{0};
    int                                       m_numInstances{0};
    int                                       m_numUpdates{0};
    std::unordered_map<int64_t, int>          m_instanceInfo;
    int                                       m_doneCount{0};
};

int main(int argc, char* argv[])
{
    int numInstances = 0;
    int numUpdates   = 0;
    for (int i = 1; i < argc - 1; ++i)
    {
        const std::string arg(argv[i]);
        if (arg == "--num-instances")
            numInstances = std::stoi(argv[i + 1]);
        else if (arg == "--num-updates")
            numUpdates = std::stoi(argv[i + 1]);
    }

    try
    {
        Consumer consumer;
        return consumer.Run(numInstances, numUpdates);
    }
    catch (const std::exception& e)
    {
        std::cerr << "Consumer caught exception: " << e.what() << std::endl;
        return 1;
    }
}
