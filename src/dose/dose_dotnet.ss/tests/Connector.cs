/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
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

using System;

class Program
{
    private class Dispatcher: Safir.Dob.Dispatcher
    {
        public void OnDoDispatch() {}
    }

    private class Stopper: Safir.Dob.StopHandler
    {
        public void OnStopOrder() {}
    }

    static void Main(string[] args)
    {
        Dispatcher dispatcher = new Dispatcher();
        Stopper stopper = new Stopper();
        Safir.Dob.Connection connection = new Safir.Dob.Connection();
        connection.Open("foo","foo",0,stopper,dispatcher);
        System.Console.WriteLine("Connected!");
        for (int i = 10; i>0; --i)
        {
            System.Console.WriteLine("Sleeping: " + i);
            System.Threading.Thread.Sleep(1000);
        }
    }
}
