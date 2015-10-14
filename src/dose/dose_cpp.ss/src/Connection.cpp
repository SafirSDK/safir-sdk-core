/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safirsdkcore.com)
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

#include <Safir/Dob/Connection.h>

#include <Safir/Dob/Internal/Interface.h>
#include <Safir/Dob/Typesystem/LibraryExceptions.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/SystemLog.h>
#include "Callbacks.h"

namespace Safir
{
namespace Dob
{
    Connection::Connection()
    {
        bool success;
        DoseC_Constructor(m_ctrl, success);
        if (!success)
        {
            Typesystem::LibraryExceptions::Instance().Throw();
        }
    }

    Connection::~Connection()
    {
        try
        {
            Close();
        }
        catch (const std::exception & exc)
        {
            SEND_SYSTEM_LOG(Alert, << "Connection::~Connection: Caught exception: " << exc.what());
        }
        DoseC_Destructor(m_ctrl);
    }

//----------------------------
    // Connect
    //----------------------------
    void Connection::Open(const std::wstring& connectionNameCommonPart,
                          const std::wstring& connectionNameInstancePart,
                          const Safir::Dob::Typesystem::Int32 context,
                          StopHandler * const stopHandler,
                          Dispatcher * const dispatcher)
    {
        bool success;
        std::string nameCommonPart=Dob::Typesystem::Utilities::ToUtf8(connectionNameCommonPart);
        std::string nameInstancePart=Dob::Typesystem::Utilities::ToUtf8(connectionNameInstancePart);
        DoseC_Connect(m_ctrl,
                      nameCommonPart.c_str(),
                      nameInstancePart.c_str(),
                      context,
                      DOSE_LANGUAGE_CPP,
                      static_cast<Internal::ConsumerBase* const>(stopHandler),
                      static_cast<Internal::ConsumerBase* const>(dispatcher),
                      &Internal::Callbacks::OnDispatch,
                      &Internal::Callbacks::OnStopOrder,
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
                      success);
        if (!success)
        {
            Typesystem::LibraryExceptions::Instance().Throw();
        }
    }



    void Connection::Close()
    {
        bool success;
        DoseC_Disconnect(m_ctrl, success);
        if (!success)
        {
            Typesystem::LibraryExceptions::Instance().Throw();
        }
    }

    bool Connection::IsOpen() const
    {
        bool isConn;
        bool success;
        DoseC_IsConnected(m_ctrl, isConn, success);
        if (!success)
        {
            Typesystem::LibraryExceptions::Instance().Throw();
        }
        return isConn;
    }

    //----------------------
    // Dispatch method
    //----------------------
    void Connection::Dispatch() const
    {
        bool success;
        DoseC_Dispatch(GetControllerId(), success);

        if (!success)
        {
            Typesystem::LibraryExceptions::Instance().Throw();
        }
    }

}
}
