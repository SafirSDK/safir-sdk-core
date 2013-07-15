/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
*
* Created by: Joel Ottosson / stjoot
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

#include "ResponseSenderImpl.h"

#include <Safir/Dob/AccessDeniedException.h>
#include <Safir/Dob/NotOpenException.h>
#include <Safir/Dob/Internal/Interface.h>
#include <Safir/Dob/Typesystem/Serialization.h>
#include <Safir/Dob/Typesystem/LibraryExceptions.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/SystemLog.h>
#include <Safir/Dob/ThisNodeParameters.h>
#include <Safir/Dob/ErrorListResponse.h>
#include <Safir/Dob/ResponseGeneralErrorCodes.h>
#include <sstream>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    ResponseSenderImpl::ResponseSenderImpl(const long ctrl,
                                           void * const consumer,
                                           const Typesystem::Int32 responseId) :
        m_valid(true),
        m_ctrl(ctrl),
        m_consumer(consumer),
        m_responseId(responseId)
    {

    }

    ResponseSenderImpl::~ResponseSenderImpl()
    {
        if (m_valid)
        {
            lllerr << "A ResponseSender was discarded without having been used!"
                   << " ctrl = " << m_ctrl
                   << ", responseId = " << m_responseId << std::endl;

            std::wostringstream ostr;
            ostr << "A ResponseSender was destroyed without having been used!" << std::endl
                 << "This is usually due to a programming error in the application:" << std::endl
                 << "Either the application did not send a response to a request, or " << std::endl
                 << "an exception was thrown inside a Service or Entity request callback, " << std::endl
                 << "causing the ResponseSender to be destroyed prematurely." << std::endl
                 << "In either case the application needs to be fixed and the system restarted"
                 << std::endl << std::endl;

            ostr << "NodeNumber = " << Safir::Dob::ThisNodeParameters::NodeNumber();
            Safir::Utilities::Internal::SystemLog().Send(Safir::Utilities::Internal::SystemLog::Alert,
                                                         ostr.str());
        }
    }

    void ResponseSenderImpl::Send(const Safir::Dob::ResponsePtr & response)
    {
        if (!m_valid)
        {
            lllout << "ResponseSenderImpl::Send: Has already been used!" << std::endl;
            throw Dob::Typesystem::SoftwareViolationException
                (L"ResponseSender object has already been used once.",
                 __WFILE__,__LINE__);
        }

        Typesystem::BinarySerialization bin;
        Typesystem::Serialization::ToBinary(response,bin);

        bool success = false;
        DoseC_SendResponse(m_ctrl, &bin[0], m_consumer, DOSE_LANGUAGE_CPP, m_responseId, success);

        if (!success)
        {
            Typesystem::LibraryExceptions::Instance().Throw();
        }
        m_valid=false;
    }

    bool ResponseSenderImpl::IsDone()
    {
        return !m_valid;
    }

    void ResponseSenderImpl::Discard()
    {
        lllout << "ResponseSenderImpl::Discard: A ResponseSender is being discarded!!!" << std::endl;
        if (!m_valid)
        {
            throw Dob::Typesystem::SoftwareViolationException
                (L"ResponseSender object has already been used once.",
                 __WFILE__,__LINE__);
        }
        m_valid = false;
    }

}
}
}
