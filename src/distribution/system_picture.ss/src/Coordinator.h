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
#ifndef __SYSTEM_PICTURE_COORDINATOR_H__
#define __SYSTEM_PICTURE_COORDINATOR_H__

#include <boost/noncopyable.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/asio.hpp>
#include <boost/asio/steady_timer.hpp>
#include <boost/function.hpp>
#include <limits>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    namespace Com
    {
        //forward declaration.
        class Communication;
    }


namespace SP
{
    //forward declarations
    class RawHandler;
    class RawStatistics;
    class SystemStateMessage;
    class SystemState;

    class Coordinator
        : private boost::noncopyable
    {
    public:
        Coordinator(const boost::shared_ptr<boost::asio::io_service>& ioService,
                    const boost::shared_ptr<Com::Communication>& communication,
                    const boost::int64_t id,
                    const char* const receiverId,
                    const boost::shared_ptr<RawHandler>& rawHandler);

        void PerformOnStateMessage(const boost::function<void(const boost::shared_ptr<char []>& data, 
                                                              const size_t size)> & fn) const;
        
        void Stop() {m_electionTimer.cancel();}
    private:
        void StatisticsChanged(const RawStatistics& statistics);
        
        void UpdateMyState(const RawStatistics& statistics);

        bool IsElected() const {return m_id == m_elected;}

        void StartElection();
        void ElectionTimeout();

        void GotData(const boost::int64_t from, 
                     const boost::shared_ptr<char[]>& data, 
                     size_t size);

        mutable boost::asio::strand m_strand;
        const boost::shared_ptr<Com::Communication> m_communication;
        const boost::uint64_t m_dataIdentifier;
        boost::shared_ptr<SystemStateMessage> m_stateMessage;
        
        const boost::int64_t m_id;
        boost::int64_t m_elected = std::numeric_limits<boost::int64_t>::min();
        boost::asio::steady_timer m_electionTimer;
        boost::int64_t m_currentElectionId = 0;
    };
}
}
}
}

#endif

