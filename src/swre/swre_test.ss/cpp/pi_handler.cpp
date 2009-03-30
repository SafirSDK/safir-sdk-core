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
#include "pi_handler.h"
#include <Safir/SwReports/SwReport.h>
#include <sstream>



//=================

void SeparatePiHandler::HandleCommand(const std::vector<std::wstring>& cmdTokens)
{
    std::wostringstream ostr;
    ostr << "Received command + parameters:";

    for (std::vector<std::wstring>::const_iterator it = cmdTokens.begin();
         it != cmdTokens.end();
         ++it)
    {
        ostr << ' ';
        ostr << (*it);
    }

    Safir::SwReports::SendProgramInfoReport(ostr.str());
}

std::wstring SeparatePiHandler::GetHelpText()
{
    return L"\tSHOW_STATUS [1]\tShow application status (handler 2)\n"
           L"\tSHOW_SWITCHES [2]\tShow status for switches (handler 2)\n";
}
/*
//=================

void PiHandler3::HandleCommand(const std::vector<std::wstring>& cmdTokens)
{
    std::wostringstream ostr;
    ostr << "Received command + parameters:";

    for (std::vector<std::wstring>::const_iterator it = cmdTokens.begin();
         it != cmdTokens.end();
         ++it)
    {
        ostr << ' ';
        ostr << (*it);
    }

    Safir::SwReports::SendProgramInfoReport(ostr.str(),
                                            GetDoseApi());
}

std::wstring PiHandler3::GetHelpText()
{
    return L"\tSHOW_STATUS [1]\tShow application status (handler 3)\n"
           L"\tSHOW_SWITCHES [2]\tShow status for switches (handler 3)\n";
}

//=================

void PiHandler4::HandleCommand(const std::vector<std::wstring>& cmdTokens)
{
    std::wostringstream ostr;
    ostr << "Received command + parameters:";

    for (std::vector<std::wstring>::const_iterator it = cmdTokens.begin();
         it != cmdTokens.end();
         ++it)
    {
        ostr << ' ';
        ostr << (*it);
    }

    Safir::SwReports::SendProgramInfoReport(ostr.str(),
                                            GetDoseApi());
}

std::wstring PiHandler4::GetHelpText()
{
    return L"\tSHOW_STATUS [1]\tShow application status (handler 4)\n"
           L"\tSHOW_SWITCHES [2]\tShow status for switches (handler 4)\n";
}

*/
