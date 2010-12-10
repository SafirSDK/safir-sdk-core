/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
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

#include <Safir/Dob/SecondaryConnection.h>

#include <Safir/Dob/Internal/Interface.h>
#include <Safir/Dob/Typesystem/LibraryExceptions.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Dob/NotOpenException.h>

#include "Callbacks.h"

namespace Safir
{
namespace Dob
{

    SecondaryConnection::SecondaryConnection():
        m_ctrl(-1)
    {

    }

    SecondaryConnection::~SecondaryConnection()
    {
        Detach();
    }

    void SecondaryConnection::Attach()
    {
        if (IsAttached()) //already attached!
        {
            Detach();
        }

        Attach(L"",L"");
    }

    void SecondaryConnection::Attach(const std::wstring& connectionNameCommonPart,
                                     const std::wstring& connectionNameInstancePart)
    {
        if (IsAttached()) //already attached!
        {
            Detach();
        }

        long newCtrlId;
        std::string nameCommonPart=Dob::Typesystem::Utilities::ToUtf8(connectionNameCommonPart);
        std::string nameInstancePart=Dob::Typesystem::Utilities::ToUtf8(connectionNameInstancePart);
        bool success;
        DoseC_ConnectSecondary(nameCommonPart.c_str(),
                               nameInstancePart.c_str(),
                               DOSE_LANGUAGE_CPP,
                               &Internal::Callbacks::OnNewEntity,
                               &Internal::Callbacks::OnUpdatedEntity,
                               &Internal::Callbacks::OnDeletedEntity,
                               &Internal::Callbacks::OnCreateRequest,
                               &Internal::Callbacks::OnUpdateRequest,
                               &Internal::Callbacks::OnDeleteRequest,
                               &Internal::Callbacks::OnServiceRequest,
                               &Internal::Callbacks::OnResponse,
                               &Internal::Callbacks::OnMessage,
                               &Internal::Callbacks::OnRegistered,
                               &Internal::Callbacks::OnUnregistered,
                               &Internal::Callbacks::OnRevokedRegistration,
                               &Internal::Callbacks::OnCompletedRegistration,
                               &Internal::Callbacks::OnInjectedNewEntity,
                               &Internal::Callbacks::OnInjectedUpdatedEntity,
                               &Internal::Callbacks::OnInjectedDeletedEntity,
                               &Internal::Callbacks::OnInitialInjectionsDone,
                               &Internal::Callbacks::OnNotRequestOverflow,
                               &Internal::Callbacks::OnNotMessageOverflow,
                               NULL, // No drop reference callback needed for C++.
                               newCtrlId,
                               success);
        if (!success)
        {
            Typesystem::LibraryExceptions::Instance().Throw();
        }
        m_ctrl=newCtrlId;
    }

    void SecondaryConnection::Detach()
    {
        m_ctrl = -1;
    }

    bool SecondaryConnection::IsAttached() const
    {
        if (m_ctrl < 0)
        {
            return false;
        }
        bool isConn = false;
        bool success;
        DoseC_IsConnected(m_ctrl, isConn, success);
        if (!success)
        {
            Typesystem::LibraryExceptions::Instance().Throw();
        }
        return isConn;
    }

    long SecondaryConnection::GetControllerId() const
    {
        if (!IsAttached())
        {
            throw Dob::NotOpenException(L"This SecondaryConnection is not attached to an open primary connection!",__WFILE__,__LINE__);
        }

        return m_ctrl;
    }
}
}
