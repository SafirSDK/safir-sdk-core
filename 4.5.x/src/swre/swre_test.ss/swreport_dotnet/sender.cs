/******************************************************************************
*
* Copyright Saab AB, 2013 (http://safir.sourceforge.net)
*
* Created by: Lars Hagstrom / lars.hagstrom@consoden.se
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
using System;

class Sender
{
    static int Main(String[] args)
    {
        Safir.SwReports.SwReport.SendFatalErrorReport("FatalError",
                                                      "here",
                                                      "Testing SendFatalErrorReport");
        
        Safir.SwReports.SwReport.SendErrorReport("Error",
                                                 "there",
                                                 "Testing SendErrorReport");
        
        Safir.SwReports.SwReport.SendResourceReport("ResourceReport",
                                                    false,
                                                    "Testing SendResourceReport");
        
        Safir.SwReports.SwReport.SendResourceReport("ResourceReport",
                                                    true,
                                                    "Testing SendResourceReport");
        
        Safir.SwReports.SwReport.SendProgrammingErrorReport("ProgrammingError",
                                                            "everywhere",
                                                            "Testing SendProgrammingErrorReport");
        
        Safir.SwReports.SwReport.SendProgramInfoReport("Testing SendProgramInfoReport");
        
        
        Safir.SwReports.SwReport.SendErrorReport("brynanuppafjässasponken", //ä
                                                 "Don't know\u203d", //interrobang
                                                 "Testing\nfunny characters");
        

        Safir.SwReports.SwReport.SendProgramInfoReport("Finished!");
        
        return 0;

    }
}
