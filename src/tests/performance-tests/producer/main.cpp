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
 * Performance test producer skeleton.
 *
 * Connects to the Dob, registers as the handler of DoseTest.RootEntity
 * (RequestorDecidesInstanceId), and is ready to service create/update/delete
 * requests from other connections.
 *
 * TODO: Implement the production loop — set entities on demand, measure
 *       throughput/latency, and report results.
 */

#include <Safir/Dob/Connection.h>
#include <Safir/Dob/Consumer.h>
#include <Safir/Dob/EntityRequestProxy.h>
#include <Safir/Dob/ErrorResponse.h>
#include <Safir/Dob/InstanceIdPolicy.h>
#include <Safir/Dob/ResponseSender.h>
#include <Safir/Dob/SuccessResponse.h>
#include <Safir/Dob/Typesystem/HandlerId.h>
#include <Safir/Utilities/AsioDispatcher.h>
#include <Safir/Logging/Log.h>
#include <DoseTest/RootEntity.h>
#include <boost/asio.hpp>
#include <chrono>
#include <iostream>
#include <optional>
#include <string>

class Producer
    : public Safir::Dob::StopHandler
    , public Safir::Dob::EntityHandler
{
public:
    int Run(int numInstances, int numUpdates)
    {
        m_numInstances = numInstances;
        m_numUpdates   = numUpdates;

        m_connection.Open(L"Producer", L"0", 0, this, &m_dispatch);

        Safir::Logging::SendSystemLog(Safir::Logging::Informational,
                                      L"Producer started");

        m_connection.RegisterEntityHandler(
            DoseTest::RootEntity::ClassTypeId,
            Safir::Dob::Typesystem::HandlerId(),          // DEFAULT_HANDLER
            Safir::Dob::InstanceIdPolicy::RequestorDecidesInstanceId,
            this);

        // Signal readiness to the driver script
        std::cout << "Producer ready" << std::endl;

        // Schedule initial entity creation once the event loop is running
        boost::asio::post(m_ioContext, [this]() { CreateInstances(); });

        m_work.emplace(m_ioContext.get_executor());
        m_ioContext.run();
        return 0;
    }

    // --- StopHandler ---
    void OnStopOrder() override
    {
        Safir::Logging::SendSystemLog(Safir::Logging::Informational,
                                      L"Producer received stop order");
        m_ioContext.stop();
    }

    // --- EntityHandler ---
    void OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId     /*typeId*/,
                               const Safir::Dob::Typesystem::HandlerId& /*handlerId*/) override
    {
        // Registration was revoked — stop the event loop
        m_ioContext.stop();
    }

    void OnCreateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
                         Safir::Dob::ResponseSenderPtr        responseSender) override
    {
        auto entity = std::dynamic_pointer_cast<DoseTest::RootEntity>(entityRequestProxy.GetRequest());
        m_connection.SetAll(entity,
                            entityRequestProxy.GetInstanceId(),
                            entityRequestProxy.GetReceivingHandlerId());
        responseSender->Send(Safir::Dob::ErrorResponse::Create()); // TODO: send SuccessResponse
    }

    void OnUpdateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
                         Safir::Dob::ResponseSenderPtr        responseSender) override
    {
        auto entity = std::dynamic_pointer_cast<DoseTest::RootEntity>(entityRequestProxy.GetRequest());
        m_connection.SetAll(entity,
                            entityRequestProxy.GetInstanceId(),
                            entityRequestProxy.GetReceivingHandlerId());
        responseSender->Send(Safir::Dob::ErrorResponse::Create()); // TODO: send SuccessResponse
    }

    void OnDeleteRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
                         Safir::Dob::ResponseSenderPtr        responseSender) override
    {
        m_connection.Delete(entityRequestProxy.GetEntityId(),
                            entityRequestProxy.GetReceivingHandlerId());
        responseSender->Send(Safir::Dob::SuccessResponse::Create());
        boost::asio::post(m_ioContext, [this]()
        {
            m_connection.Close();
            m_work.reset();
        });
    }

private:
    void CreateInstances()
    {
        auto entity = DoseTest::RootEntity::Create();
        for (int i = 1; i <= m_numInstances; ++i)
        {
            entity->Info() = L"0";
            m_connection.SetAll(entity,
                                Safir::Dob::Typesystem::InstanceId(static_cast<int64_t>(i)),
                                Safir::Dob::Typesystem::HandlerId());
        }
        boost::asio::post(m_ioContext, [this]() { UpdateInstances(); });
    }

    void UpdateInstances()
    {
        auto entity = DoseTest::RootEntity::Create();
        const auto start = std::chrono::steady_clock::now();
        for (int u = 1; u <= m_numUpdates; ++u)
        {
            for (int i = 1; i <= m_numInstances; ++i)
            {
                entity->Info() = std::to_wstring(u);
                m_connection.SetAll(entity,
                                    Safir::Dob::Typesystem::InstanceId(static_cast<int64_t>(i)),
                                    Safir::Dob::Typesystem::HandlerId());
            }
        }
        const auto elapsed = std::chrono::steady_clock::now() - start;
        const auto ms = std::chrono::duration_cast<std::chrono::milliseconds>(elapsed).count();
        std::cout << "All updates done (tot: " << (m_numInstances * m_numUpdates) << "). Total time: " << ms << " ms" << std::endl;
    }

    using WorkGuard = boost::asio::executor_work_guard<boost::asio::io_context::executor_type>;

    Safir::Dob::Connection           m_connection;
    boost::asio::io_context          m_ioContext;
    Safir::Utilities::AsioDispatcher m_dispatch{m_connection, m_ioContext};
    std::optional<WorkGuard>         m_work;
    int                              m_numInstances{0};
    int                              m_numUpdates{0};
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
        Producer producer;
        return producer.Run(numInstances, numUpdates);
    }
    catch (const std::exception& e)
    {
        std::cerr << "Producer caught exception: " << e.what() << std::endl;
        return 1;
    }
}
