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


    class PersistHandler:
        public Safir::Dob::Dispatcher,
        public Safir::Dob::ServiceHandler,
        private boost::noncopyable
    {
    public:

        using PersistenDataReadyCallback = std::function<void()>;

        PersistHandler(boost::asio::io_service& ioService,
                       const int64_t nodeId,
                       Com::Communication& communication);

        void Start(const std::function<void(const std::string& str)>& logStatus);
        void Stop();

        void AddSubscriber(const PersistenDataReadyCallback& cb);

        void SetPersistentDataReady();

        bool IsPersistentDataReady() const;

    private:

        void OnDoDispatch() override;
        void OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId    typeId,
                                   const Safir::Dob::Typesystem::HandlerId& handlerId) override;

        void OnServiceRequest(const Safir::Dob::ServiceRequestProxy serviceRequestProxy,
                              Safir::Dob::ResponseSenderPtr   responseSender) override;


        //void HandleTimeout(const TimerInfoPtr & timer) override;

        void RequestPersistenceInfo();

        boost::asio::io_service::strand         m_strand;
        const int64_t                           m_nodeId;
        Com::Communication&                     m_communication;
        Safir::Dob::Connection                  m_connection;
        std::set<Safir::Dob::Typesystem::Int32> m_waitingForResponsesFromNodes;
        std::atomic<bool>                       m_persistDataReady;
        std::vector<PersistenDataReadyCallback> m_callbacks;
    };
}
}
}

