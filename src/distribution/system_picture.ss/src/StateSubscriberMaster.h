/******************************************************************************
*
* Copyright Saab AB, 2013 (http://safir.sourceforge.net)
*
* Created by: Lars Hagström / lars.hagstrom@consoden.se
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
#pragma once

#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/MakeUnique.h>
#include <Safir/Utilities/Internal/SystemLog.h>
#include <Safir/Dob/Internal/SystemState.h>
#include <functional>
#include <boost/asio.hpp>

namespace Safir
{
namespace Dob
{
namespace Internal
{
namespace SP
{

    class StateSubscriberMaster
        : public SystemStateSubscriber
    {
    public:
        StateSubscriberMaster(boost::asio::io_service& ioService,
                              Coordinator& coordinator)
            : m_strand(ioService)
        {
            coordinator.SetStateChangedCallback([this](const SystemStateMessage& data)
                                                {
                                                    const auto dataCopy = SystemStateCreator::Create(Safir::make_unique<SystemStateMessage>(data));
                                                    m_strand.post([this, dataCopy]
                                                                  {
                                                                      if (m_dataCallback != nullptr)
                                                                      {
                                                                          m_dataCallback(dataCopy);
                                                                      }
                                                                  });
                                                });
        }

        void Start(const std::function<void (const SystemState& data)>& dataCallback) override
        {
            m_strand.dispatch([this, dataCallback]
                              {
                                  if (m_dataCallback != nullptr)
                                  {
                                      throw std::logic_error("StateSubscriberMaster already started");
                                  }
                                  
                                  m_dataCallback = dataCallback;
                              });
        }

        void Stop() override
        {
            m_strand.dispatch([this]
                              {
                                  m_dataCallback = nullptr;
                              });
        }

    private:

        std::function<void (const SystemState& data)> m_dataCallback;
        boost::asio::strand m_strand;
    };

    
}
}
}
}

