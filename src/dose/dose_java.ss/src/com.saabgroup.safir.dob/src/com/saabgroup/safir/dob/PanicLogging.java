// -*- coding: utf-8 -*-
/******************************************************************************
*
* Copyright Saab AB, 2009 (http://www.safirsdk.com)
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
package com.saabgroup.safir.dob;

import java.net.*;


/**
 * This stuff should really be exported in an lluf interface, but there wasnt time
 * to do that. Raised ticket #691 to cover this issue.
 */
class PanicLogging {
    static void log(String text){
        try
        {
            String message =
                "== PANIC LOG! =================================================================\n" +
                "Pid = <not implemented in java>\n" +
                "Process Name = <some java process\n" +
                "Process Description = <dunno, sorry>\n" +
                "Error Message:\n" +
                text +
                "\n===============================================================================\n";

            byte[] messageBytes = message.getBytes();

            DatagramPacket packet = new DatagramPacket(messageBytes,
                                                       messageBytes.length,
                                                       InetAddress.getLocalHost(),
                                                       31221); //port

            DatagramSocket dsocket = new DatagramSocket();
            dsocket.send(packet);
            dsocket.close();

        }
        catch (java.lang.Exception exc)
        {
            System.out.println("PanicLog failed:");
            exc.printStackTrace();
        }

    }
}
