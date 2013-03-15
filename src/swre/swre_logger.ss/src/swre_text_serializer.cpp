/******************************************************************************
*
* Copyright Saab AB, 2006-2008 (http://www.safirsdk.com)
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
#include "swre_text_serializer.h"
#include <Safir/SwReports/Internal/ReportSerializer.h>

namespace Safir
{
namespace Swre
{

//-----------------------------------------------------------------------------
TextSerializer::TextSerializer()
    : m_includeTypeSequenceNumber(false)
{
}

//-----------------------------------------------------------------------------
TextSerializer::~TextSerializer()
{
}

//-----------------------------------------------------------------------------
TextSerializer& TextSerializer::Inst()
{
    static TextSerializer inst;
    return inst;
}

//-----------------------------------------------------------------------------
std::wstring TextSerializer::SerializeReport(const Safir::SwReports::Internal::ReportPtr report)
{
    return Safir::SwReports::Internal::ReportSerializer::SerializeReport(report, m_includeTypeSequenceNumber);
}

//-----------------------------------------------------------------------------
void TextSerializer::SetIncludeTypeSequenceNumber(bool on)
{
    m_includeTypeSequenceNumber = on;
}

}
}
