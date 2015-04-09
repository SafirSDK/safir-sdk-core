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

#include "Distribution.h"
#include <Safir/Dob/Internal/InternalFwd.h>
#include <Safir/Dob/Internal/Communication.h>
#include <Safir/Dob/Connection.h>
#include <boost/noncopyable.hpp>
#include <set>

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

    // TODO Comment
    class PersistHandler:
        public Safir::Dob::Dispatcher,
        public Safir::Dob::ServiceHandler,
        private boost::noncopyable
    {
    public:

        PersistHandler(boost::asio::io_service& ioService,
                       Distribution& distribution,
                       const std::function<void(const std::string& str)>& logStatus,
                       const std::function<void()>& persistentDataReadyCb,
                       const std::function<void()>& persistentDataAllowedCb);

        void SetPersistentDataReady();

    private:

        void OnDoDispatch() override;
        void OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId    typeId,
                                   const Safir::Dob::Typesystem::HandlerId& handlerId) override;

        void OnServiceRequest(const Safir::Dob::ServiceRequestProxy serviceRequestProxy,
                              Safir::Dob::ResponseSenderPtr   responseSender) override;

        void SetPersistentDataAllowed();

        void RequestPersistenceInfo();

        void HandleMessageFromRemoteNode(const int64_t  fromNodeId,
                                         const int64_t  fromNodeType,
                                         const char*    data);

        void Resend();

        void CheckResponseStatus();

        std::pair<boost::shared_ptr<const char[]>, size_t> CreateRequest() const;
        std::pair<boost::shared_ptr<const char[]>, size_t> CreateResponse() const;


        boost::asio::io_service::strand         m_strand;
        bool                                    m_systemFormed;
        std::set<std::pair<int64_t, int64_t> >  m_nodes; // pair<nodeId, nodeTypeId>
        std::set<std::pair<int64_t, int64_t> >  m_unsentRequests;
        std::set<std::pair<int64_t, int64_t> >  m_unsentResponses;
        Distribution&                           m_distribution;
        Com::Communication&                     m_communication;
        Safir::Dob::Connection                  m_connection;
        bool                                    m_persistentDataReady;
        bool                                    m_persistentDataAllowed;
        std::vector<std::function<void()> >     m_persistentDataReadyCb;
        std::vector<std::function<void()> >     m_persistentDataAllowedCb;
        const int64_t                           m_dataTypeIdentifier;
    };
}
}
}

