/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
* Copyright Consoden AB, 2015 (http://www.consoden.se)
*
* Created by: Lars Hagström / stlrha
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

#include <boost/noncopyable.hpp>
#include <Safir/Dob/Internal/InternalFwd.h>
#include <Safir/Dob/Internal/DistributionData.h>
#include <deque>

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4267)
#endif

#include <boost/asio.hpp>

#ifdef _MSC_VER
#pragma warning (pop)
#endif

namespace Safir
{
namespace Dob
{
namespace Internal
{
    //forward declarations:
    namespace Com
    {
        class Communication;
    }
    class RequestHandler;
    class PendingRegistrationHandler;
    class NodeHandler;

    class ConnectionHandler:
        private boost::noncopyable
    {
    public:
        ConnectionHandler(boost::asio::io_service& ioService,
                          Com::Communication& communication,
                          RequestHandler& requesthandler,
                          PendingRegistrationHandler& prh);

        void HandleConnect(const ConnectionPtr & connection);
        void HandleDisconnect(const ConnectionPtr & connection);

        bool HandleUnsent();

        void OnPoolDistributionComplete();

    private:

        boost::asio::io_service::strand m_strand;
        Com::Communication&             m_communication;
        RequestHandler&                 m_requestHandler;
        PendingRegistrationHandler&     m_pendingRegistrationHandler;

        std::deque<DistributionData> m_unsent;

        bool m_poolDistributionComplete = false;
    };
}
}
}


