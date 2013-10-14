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
using System.Runtime.InteropServices;

class Program
{
    class foo
    {
        public foo()
        {
            Console.WriteLine("Constructor");
        }
        ~foo()
        {
            Console.WriteLine("Destructor");
        }

    }
    static void Main(string[] args)
    {
        System.IntPtr p;
        {
            GCHandle handle;
            Object obj = new foo();
            handle = GCHandle.Alloc(obj);
            p = GCHandle.ToIntPtr(handle);
            obj = new Object();
        }

        for (int i = 0; i< 100; ++i)
        {
            GC.Collect();
            System.Threading.Thread.Sleep(0);
        }

        Console.WriteLine("Freeing");

        GCHandle.FromIntPtr(p).Free();

        for (int i = 0; i< 100; ++i)
        {
            GC.Collect();
            System.Threading.Thread.Sleep(0);
        }


        Console.WriteLine("End.");
    }
}
