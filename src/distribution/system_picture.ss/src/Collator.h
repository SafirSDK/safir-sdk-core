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
#ifndef __SYSTEM_PICTURE_COLLATOR_H__
#define __SYSTEM_PICTURE_COLLATOR_H__

#include <boost/noncopyable.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/asio.hpp>
#include <boost/function.hpp>

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

    class Collator
        : private boost::noncopyable
    {
    public:
        Collator(const boost::shared_ptr<boost::asio::io_service>& ioService,
                 const boost::shared_ptr<Com::Communication>& communication,
                 const boost::int64_t id,
                 const boost::shared_ptr<RawHandler>& rawHandler);

        void PerformOnStateMessage(const boost::function<void(const boost::shared_ptr<char []>& data, 
                                                              const size_t size)> & fn) const;

    private:
        void Collate(const RawStatistics& statistics);

        bool IsCoordinator() const {return m_id == m_coordinator;}
        mutable boost::asio::strand m_strand;
        const boost::shared_ptr<Com::Communication> m_communication;
        boost::shared_ptr<SystemStateMessage> m_stateMessage;
        
        const boost::int64_t m_id;
        boost::int64_t m_coordinator = 0;
    };
}
}
}
}

#endif

