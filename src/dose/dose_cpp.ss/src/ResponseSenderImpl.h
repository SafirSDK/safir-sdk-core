/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safirsdkcore.com)
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

#ifndef _SAFIR_DOB_RESPONSESENDERIMP_H
#define _SAFIR_DOB_RESPONSESENDERIMP_H

#include <boost/shared_ptr.hpp>
#include <Safir/Dob/Response.h>
#include <Safir/Dob/ResponseSender.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    class ResponseSenderImpl :
        public Dob::ResponseSender
    {
    public:
        ResponseSenderImpl(const long ctrl,
                           void * const consumer,
                           const Typesystem::Int32 responseId);

        virtual ~ResponseSenderImpl();

        virtual void Send(const Safir::Dob::ResponsePtr & response);

        virtual bool IsDone();

        virtual void Discard();

    private:
        bool m_valid;
        const long m_ctrl;
        void * const m_consumer;
        const Typesystem::Int32 m_responseId;
    };
}
}
}

#endif
