/******************************************************************************
*
* Copyright Saab AB, 2013 (http://www.safirsdk.com)
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
 * Mait test class
 */
public class Sender {
    public static void main(String[] args) {
        com.saabgroup.safir.application.TracerBackdoor.setProgramName("tracer_sender_java");
        com.saabgroup.safir.application.Tracer razor = new com.saabgroup.safir.application.Tracer("Razor");
        com.saabgroup.safir.application.Tracer rb = new com.saabgroup.safir.application.Tracer("Rymd-Börje");
        razor.setEnabled(true);
        rb.setEnabled(true);
        rb.println("blahonga");
        rb.println("blahong®a");
        rb.println("blahongaåäö");
        razor.println("brynanuppafjässasponken");
        razor.println("\u202ereversed");
        rb.println("skull and crossbones: \u2620");
        rb.println("interrobang: \u203d");
        razor.printf("%d%d%.1f\n",1,2,3.1);
        razor.print("foo");
        razor.println("bar");
        razor.println("this is the end\nmy only friend, the end");
        rb.println("of our elaborate plans");
        System.exit(0);
    }
}

