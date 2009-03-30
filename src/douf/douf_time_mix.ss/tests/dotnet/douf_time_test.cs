/******************************************************************************
*
* Copyright Saab AB, 2005-2008 (http://www.safirsdk.com)
*
* Created by: Erik Adolfsson / sterad
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
using System.Threading;

namespace douf_time_test
{
    class douf_time_test
    { 
        /// <summary>
        /// The main entry point for the application.
        /// </summary>
        [STAThread]
        static void Main()
        {
            DobHandler dobHandler = new DobHandler();
            dobHandler.Start();

            char choice = '\0';
            int i;
            System.Console.WriteLine("1: Time Subscriber");
            System.Console.WriteLine("2: Time Owner");
            System.Console.WriteLine("Make your choice");
            i = System.Console.Read();
            choice = (char) i;
            switch (choice)
            {
                case '1':
                    Subscriber subscriber = new Subscriber(dobHandler);
                    subscriber.Start();
                    break;
                case '2':
                    Requestor requestor = new Requestor(dobHandler);
                    requestor.Start();
                    break;
            }


            Thread.Sleep(Timeout.Infinite);
        }
    }
}
