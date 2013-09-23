/******************************************************************************
*
* Copyright Saab AB, 2013 (http://safir.sourceforge.net)
*
* Created by: Lars Hagström / lars.hagstrom@foldspace.nu
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

/**
 * Main test class
 */
@SuppressWarnings("deprecation")
public class Sender {
    public static void main(String[] args) {
        com.saabgroup.safir.swreports.SwReport.SendFatalErrorReport("FatalError",
                                                                    "here",
                                                                    "Testing SendFatalErrorReport");
        
        com.saabgroup.safir.swreports.SwReport.SendErrorReport("Error",
                                                               "there",
                                                               "Testing SendErrorReport");
        
        com.saabgroup.safir.swreports.SwReport.SendResourceReport("ResourceReport",
                                                                  false,
                                                                  "Testing SendResourceReport");
        
        com.saabgroup.safir.swreports.SwReport.SendResourceReport("ResourceReport",
                                                                  true,
                                                                  "Testing SendResourceReport");
        
        com.saabgroup.safir.swreports.SwReport.SendProgrammingErrorReport("ProgrammingError",
                                                                          "everywhere",
                                                                          "Testing SendProgrammingErrorReport");
        
        com.saabgroup.safir.swreports.SwReport.SendProgramInfoReport("Testing SendProgramInfoReport");
        
        
        com.saabgroup.safir.swreports.SwReport.SendErrorReport("brynanuppafjässasponken", //ä
                                                               "Don't know\u203d", //interrobang
                                                               "Testing\nfunny characters");
        

        com.saabgroup.safir.swreports.SwReport.SendProgramInfoReport("Finished!");
        
        System.exit(0);
    }
}
