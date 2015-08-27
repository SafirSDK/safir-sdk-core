/******************************************************************************
*
* Copyright Saab AB, 2007-2013,2015 (http://safir.sourceforge.net)
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

#include <Safir/Dob/SecondaryConnection.h>
#include <boost/noncopyable.hpp>


namespace Safir
{
namespace Control
{
    /**
     * This class handles the Safir.Control.Status entity.
     *
     * The application own one local instance of this entity. No requests are allowed.
     */
    class StatusEntityHandler:
        public Safir::Dob::EntityHandler,
        private boost::noncopyable
    {
    public:
        StatusEntityHandler();

        void Stop();
        void Start();

        void SetValues(int64_t nodeId, int64_t incarnationId);

    private:
        void OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId    typeId,
                                   const Safir::Dob::Typesystem::HandlerId& handlerId) override;

        void OnCreateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
                             Safir::Dob::ResponseSenderPtr    responseSender) override;

        void OnUpdateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
                                     Safir::Dob::ResponseSenderPtr    responseSender) override;

        void OnDeleteRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
                             Safir::Dob::ResponseSenderPtr    responseSender) override;



        Safir::Dob::SecondaryConnection m_dobConnection;
    };
}
}
