// -*- coding: utf-8 -*-
/******************************************************************************
*
* Copyright Saab AB, 2006-2013, 2022 (http://safirsdkcore.com)
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

/** Main entrypoint class */
public class Program
{
    private Program() {}
    /**
     * main program
     * @param args command line
     */
    public static void main(String[] args)
    {
        System.out.println("Starting");
        Executor app = new Executor(args);
        try {
            app.run();
        }
        catch (InterruptedException exc) {
        }

            Logger.instance().println("Performing extra checks");
            {
                //Check shared memory usage. And expect more than 100 bytes used.
                long usage = new com.saabgroup.safir.dob.ConnectionAspectMisc(new com.saabgroup.safir.dob.Connection()).getSharedMemoryUsage();
                if (usage < 100)
                {
                    Logger.instance().println("getSharedMemoryUsage returned unexpected value.");
                    System.exit(1);
                }

                if (new com.saabgroup.safir.dob.ConnectionAspectMisc(new com.saabgroup.safir.dob.Connection()).getSharedMemoryLevel() !=
                    com.saabgroup.safir.dob.MemoryLevel.NORMAL)
                {
                    Logger.instance().println("getSharedMemoryLevel returned unexpected value.");
                    System.exit(1);
                }
            }

        Logger.instance().println("Exiting");
        //        System.exit(0);
    }
}
