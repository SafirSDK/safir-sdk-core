/******************************************************************************
*
* Copyright Consoden AB, 2015 (http://www.consoden.se)
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

#include <Safir/Utilities/Internal/AsioPeriodicTimer.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/SystemLog.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    class MemoryMonitor:
        public SharedMemoryObject,
        private boost::noncopyable
    {
    public:
        explicit MemoryMonitor(boost::asio::io_service& ioService):
            m_capacity(GetSharedMemory().get_size()),
            m_warningPercent(20)
        {

            m_timer.reset(new Safir::Utilities::Internal::AsioPeriodicTimer(ioService,
                                                                            boost::chrono::seconds(30),
                                                                            [this](const boost::system::error_code& error)
                                                                            {
                                                                                if (!error)
                                                                                {
                                                                                    Check();
                                                                                }
                                                                            }));

            m_timer->Start();
        }

        void Stop()
        {
            m_timer->Stop();
        }
    private:
        void Check()
        {
            try
            {
                try
                {
                    const size_t free = GetSharedMemory().get_free_memory();
                    const double percentFree = static_cast<double>(free)/static_cast<double>(m_capacity) * 100;
                    lllog(4) << percentFree << "% of shared memory is available" << std::endl;
                    if (percentFree < m_warningPercent)
                    {
                        SEND_SYSTEM_LOG(Alert,
                                        << "Less than " << m_warningPercent << "% of the Dob shared memory is available!"
                                        << "This probably means that you're close to running out of memory!"
                                        << "Increase Safir.Dob.NodeParameters.SharedMemorySize.");
                    }
                }
                catch (const std::exception& exc)
                {
                    SEND_SYSTEM_LOG(Alert,
                                    << "Got exception in dose_main MemoryMonitor: "
                                    << exc.what());
                }
                catch (...)
                {
                    SEND_SYSTEM_LOG(Alert,
                                    << "Got ... exception in dose_main MemoryMonitor!");
                }
            }
            catch(...)
            {

            }
        }
    private:
        const size_t m_capacity;
        const double m_warningPercent;

        std::unique_ptr<Safir::Utilities::Internal::AsioPeriodicTimer> m_timer;
    };
}
}
}


