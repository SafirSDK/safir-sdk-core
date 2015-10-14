// -*- coding: utf-8 -*-
/******************************************************************************
*
* Copyright Saab AB, 2006-2013 (http://safirsdkcore.com)
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

//package DoseTestJava;

public class Program
{
    /** main program */
    public static void main(String[] args)
    {
        System.out.println("Starting");
        Executor app = new Executor(args);
        try {
            app.run();
        }
        catch (InterruptedException exc) {
        }

        Logger.instance().println("Exiting");
        //        System.exit(0);
    }
}
