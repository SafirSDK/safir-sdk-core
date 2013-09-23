/******************************************************************************
*
* Copyright Saab AB, 2008-2013 (http://safir.sourceforge.net)
*
* Created by: Anders Widén / stawi
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

#include <Safir/Dob/ConnectionAspectMisc.h>

#include <Safir/Dob/Internal/Interface.h>
#include <Safir/Dob/Typesystem/Utilities.h>
#include <Safir/Dob/Typesystem/LibraryExceptions.h>

namespace Safir
{
namespace Dob
{
    const CallbackId::Enumeration ConnectionAspectMisc::GetCurrentCallbackId() const
    {
        DotsC_Int32 callback;
        bool success;
        DoseC_GetCurrentCallbackId(GetControllerId(),callback, success);

        if (!success)
        {
            Typesystem::LibraryExceptions::Instance().Throw();
        }
        return static_cast<CallbackId::Enumeration>(callback);
    }

    const std::wstring ConnectionAspectMisc::GetConnectionName() const
    {
        const char* name;
        bool success;
        DoseC_GetConnectionName(GetControllerId(), name, success);

        if (!success)
        {
            Typesystem::LibraryExceptions::Instance().Throw();
        }
        return Typesystem::Utilities::ToWstring(name);
    }

    const std::wstring ConnectionAspectMisc::GetConnectionNameCommonPart() const
    {
        const char* name;
        bool success;
        DoseC_GetConnectionNameCommonPart(GetControllerId(), name, success);

        if (!success)
        {
            Typesystem::LibraryExceptions::Instance().Throw();
        }
        return Typesystem::Utilities::ToWstring(name);
    }

    const std::wstring ConnectionAspectMisc::GetConnectionNameInstancePart() const
    {
        const char* name;
        bool success;
        DoseC_GetConnectionNameInstancePart(GetControllerId(), name, success);

        if (!success)
        {
            Typesystem::LibraryExceptions::Instance().Throw();
        }
        return Typesystem::Utilities::ToWstring(name);
    }

    Dob::Typesystem::Int32 ConnectionAspectMisc::GetContext() const
    {
        Dob::Typesystem::Int32 context;
        bool success;
        DoseC_GetContext(GetControllerId(), context, success);

        if (!success)
        {
            Typesystem::LibraryExceptions::Instance().Throw();
        }
        return context;
    }

    Safir::Dob::Typesystem::Int32
    ConnectionAspectMisc::GetQueueCapacity(const Safir::Dob::ConnectionQueueId::Enumeration queue) const
    {
        Safir::Dob::Typesystem::Int32 queueCapacity;
        bool success;
        DoseC_GetQueueCapacity(GetControllerId(), queue, queueCapacity, success);
        if (!success)
        {
            Typesystem::LibraryExceptions::Instance().Throw();
        }
        return queueCapacity;
    }


    Safir::Dob::Typesystem::Int32
    ConnectionAspectMisc::GetQueueSize(const Safir::Dob::ConnectionQueueId::Enumeration queue) const
    {
        Safir::Dob::Typesystem::Int32 queueSize;
        bool success;
        DoseC_GetQueueSize(GetControllerId(), queue, queueSize, success);
        if (!success)
        {
            Typesystem::LibraryExceptions::Instance().Throw();
        }
        return queueSize;
    }

    //-----------------------
    // Debug
    //-----------------------
    void ConnectionAspectMisc::SimulateOverflows(const bool inQueues, const bool outQueues) const
    {
        bool success;
        DoseC_SimulateOverflows(GetControllerId(), inQueues, outQueues, success);
        if (!success)
        {
            Typesystem::LibraryExceptions::Instance().Throw();
        }
    }

}
}
