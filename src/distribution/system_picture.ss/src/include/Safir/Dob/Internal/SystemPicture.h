/******************************************************************************
*
* Copyright Saab AB, 2012 (http://safirsdkcore.com)
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

#include <boost/asio/io_service.hpp>
#include <boost/noncopyable.hpp>
#include <Safir/Dob/Internal/RawStatistics.h>
#include <Safir/Dob/Internal/SystemState.h>
#include <Safir/Dob/Internal/SystemPictureDefs.h>
#include <map>
#include <vector>
#include <memory>

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

    struct master_tag_t {};
    const master_tag_t master_tag = master_tag_t();

    struct slave_tag_t {};
    const slave_tag_t slave_tag = slave_tag_t();

    struct subscriber_tag_t {};
    const subscriber_tag_t subscriber_tag = subscriber_tag_t();

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4275)
#pragma warning (disable: 4251)
#endif

    class DISTRIBUTION_SYSTEM_PICTURE_API SystemPicture
        : private boost::noncopyable
    {
    public:
        /**
         * Constructor for creating a master instance of SystemPicture.
         *
         * This is meant to be used in the Control executable.
         *
         * @param validateJoinSystem Called when the node wants to join an existing system. Return false if
         *                           the incarnation id belongs to a system that we should not join.
         *                           If true is returned the node will use the given incarnation id.
         *
         * @param validateFormSystem Called when the node wants to form a new system. Return false if
         *                           a we are not allowed to form a system. If true is returned the node will
         *                           use the given incarnation id.
         */
        SystemPicture(master_tag_t,
                      boost::asio::io_service& ioService,
                      Com::Communication& communication,
                      const std::string& name,
                      const int64_t id,
                      const int64_t nodeTypeId,
                      const std::map<int64_t, NodeType>& nodeTypes,
                      const boost::chrono::steady_clock::duration& aloneTimeout,
                      const std::function<bool (const int64_t incarnationId)>& validateJoinSystemCallback,
                      const std::function<bool (const int64_t incarnationId)>& validateFormSystemCallback);


        /**
         * Constructor for creating a slave instance of SystemPicture.
         *
         * This is meant to be used in the dose_main executable.
         */
        SystemPicture(slave_tag_t,
                      boost::asio::io_service& ioService,
                      Com::Communication& communication,
                      const std::string& name,
                      const int64_t id,
                      const int64_t nodeTypeId,
                      const std::map<int64_t, NodeType>& nodeTypes);
        /**
         * Constructor for creating a subscriber instance of SystemPicture.
         *
         * This is meant to be used in applications that just want to subscribe
         * to the system picture information, such as dobexplorer.
         */
        SystemPicture(subscriber_tag_t,
                      boost::asio::io_service& ioService);

        /** Destructor. */
        ~SystemPicture();

        /**
         * Stop the internal workings of this class.
         * Must be called before destroying the object.
         */
        void Stop();

        /**
         * Start a subscription to raw information.
         *
         * Call Stop to end all subscriptions.
         * Note that it is not possible to call Start again after a Stop call.
         */
        void StartRawSubscription(const std::function<void (const RawStatistics& data)>& dataCallback);

        /**
         * Start a subscription to system state information.
         *
         * Call Stop to end all subscriptions.
         * Note that it is not possible to call Start again after a Stop call.
         */
        void StartStateSubscription(const std::function<void (const SystemState& data)>& dataCallback);

        /**
         * Exclude a node from the system.
         *
         * Calling this function will immediately exclude a node from the system.
         * This function will also call Communications ExcludeNode function.
         */
        void ExcludeNode(const int64_t id);

        /**
         * Resurrect a lightnode that has been marked as dead.
         *
         * Calling this function will allow a lightnode that has previously
         * been marked as dead to be resurrected.
         * This function will also call Communications InjectNode function.
         */
        void ResurrectNode(const int64_t id);
    private:
        class Impl;

        std::unique_ptr<Impl> m_impl;
    };

#ifdef _MSC_VER
#pragma warning (pop)
#endif
}
}
}
}
