/******************************************************************************
*
* Copyright Saab AB, 2013 (http://www.safirsdk.com)
*
* Created by: Anders Widén
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
#ifndef __SWRE_REPORT_SERIALIZER_H__
#define __SWRE_REPORT_SERIALIZER_H__

#include <Safir/SwReports/Internal/UtilsExportDefs.h>
#include <Safir/SwReports/Internal/Report.h>

namespace Safir
{
namespace SwReports
{
namespace Internal
{
    class SWRE_UTILS_API ReportSerializer
    {
    public:

        static std::wstring SerializeReport(const Safir::SwReports::Internal::ReportPtr report, bool includeTypeSequenceNumber);

    private:
        ReportSerializer(); //not instantiable
    };
}
}
}

#endif

