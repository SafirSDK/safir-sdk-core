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
#include <Safir/Application/Tracer.h>
#include <Safir/SwReports/Internal/Interface.h>
#include <Safir/Dob/Typesystem/Utilities.h>
#include <Safir/Dob/Typesystem/LibraryExceptions.h>
#include <Safir/Dob/ConnectionAspectMisc.h>

namespace Safir
{
namespace Application
{

//---------------------------------
Tracer::Tracer(const std::wstring & prefix):
    m_ostream(&m_buf),
    m_buf(prefix),
    m_isEnabled(NULL)
{

}

//---------------------------------
Tracer::~Tracer()
{

}

using Safir::Dob::Typesystem::Utilities::ToUtf8;

void Tracer::SetProgramName(const std::wstring& programName)
{
    using namespace Safir::Dob::Typesystem::Utilities;

    bool success;
    SwreC_SetProgramName(ToUtf8(programName).c_str(), success);
    if (!success)
    {
        Safir::Dob::Typesystem::LibraryExceptions::Instance().Throw();
    }
}

void Tracer::StartTraceBackdoor(const Safir::Dob::ConnectionBase& connection)
{
    Safir::Dob::ConnectionAspectMisc conn(connection);

    bool success;
    SwreC_StartTraceBackdoor(ToUtf8(conn.GetConnectionNameCommonPart()).c_str(),
                             ToUtf8(conn.GetConnectionNameInstancePart()).c_str(),
                             success);
    if (!success)
    {
        Safir::Dob::Typesystem::LibraryExceptions::Instance().Throw();
    }
}

void Tracer::StopTraceBackdoor()
{
    SwreC_StopTraceBackdoor();
}

void Tracer::InitializeEnabledHandling() const
{
    if (m_isEnabled == NULL)
    {
        m_isEnabled = SwreC_TracePrefixGetIsEnabledPointer(m_buf.GetPrefixId());
    }
}

}
}

