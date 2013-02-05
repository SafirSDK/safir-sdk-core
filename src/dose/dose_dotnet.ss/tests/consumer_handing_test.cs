/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
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
using Safir.Dob;
class Program
{
    static int objects = 0;

    class foo:
        Safir.Dob.Dispatcher
    {
        public foo()
        {
            ++objects;
            //Console.WriteLine("Constructor");
        }
        ~foo()
        {
            --objects;
            //Console.WriteLine("Destructor");
        }


        #region Dispatcher Members

        public void OnDoDispatch()
        {
            throw new Exception("The method or operation is not implemented.");
        }

        #endregion
    }

    static void RunGC()
    {
        for (int i = 0; i < 100; ++i)
        {
            GC.Collect();
            System.Threading.Thread.Sleep(1);
        }
    }

    static void Check0()
    {
        RunGC();
        if (objects != 0)
        {
            System.Console.WriteLine("ERROR! objects should be 0 but is " + objects);
        }
    }

    static void Check1()
    {
        RunGC();
        if (objects != 1)
        {
            System.Console.WriteLine("ERROR! objects should be 1 but is " + objects);
        }
    }

    static void Main(string[] args)
    {
        //no external refs
        {
            foo f = new foo();
            f = null;
            Check0();
        }

        //one external ref
        {
            foo f = new foo();
            
            System.IntPtr p = ConsumerHandler.Instance.AddReference(f);

            ConsumerHandler.Instance.DropReference(ConsumerHandler.ToConsumer(p));
            f = null;
            Check0();
        }

        //one external ref
        {
            foo f = new foo();

            System.IntPtr p = ConsumerHandler.Instance.AddReference(f);
            f = null;
            Check1();
            ConsumerHandler.Instance.DropReference(ConsumerHandler.ToConsumer(p));
            Check0();
        }

        //one external ref
        {
            foo f = new foo();

            System.IntPtr p = ConsumerHandler.Instance.AddReference(f);
            Safir.Dob.Internal.ConsumerBase cb = ConsumerHandler.ToConsumer(p);
            cb = ConsumerHandler.ToConsumer(p);
            cb = ConsumerHandler.ToConsumer(p);
            ConsumerHandler.Instance.DropReference(ConsumerHandler.ToConsumer(p));
            f = null;
            Check1();
            cb = null;
            Check0();
        }

        //two external ref
        {
            foo f = new foo();

            System.IntPtr p1 = ConsumerHandler.Instance.AddReference(f);
            System.IntPtr p2 = ConsumerHandler.Instance.AddReference(f);
            f = null;

            if (p1 != p2)
            {
                Console.WriteLine("Consumer address should be the same!");
            }

            ConsumerHandler.Instance.DropReference(ConsumerHandler.ToConsumer(p1));
            Check1();
            ConsumerHandler.Instance.DropReference(ConsumerHandler.ToConsumer(p2));
            Check0();

           
        }

        //two objects
        {
            foo f1 = new foo();
            foo f2 = new foo();

            System.IntPtr p1 = ConsumerHandler.Instance.AddReference(f1);
            System.IntPtr p2 = ConsumerHandler.Instance.AddReference(f2);
            f1 = null;
            f2 = null;

            if (p1 == p2)
            {
                Console.WriteLine("Consumer address should be different!");
            }

            ConsumerHandler.Instance.DropReference(ConsumerHandler.ToConsumer(p1));
            ConsumerHandler.Instance.DropReference(ConsumerHandler.ToConsumer(p2));
            Check0();
        }

        //one external ref
        /*
        {
            foo f = new foo();

            System.IntPtr p1 = ConsumerHandler.Instance.AddReference(f);
            System.IntPtr p2 = ConsumerHandler.Instance.AddReference(f);

            System.Console.WriteLine("p1 = " + p1);
            System.Console.WriteLine("p2 = " + p2);

            ConsumerHandler.Instance.DropReference(p1);
            ConsumerHandler.Instance.DropReference(p2);
            f = null;
            Check0();

        }*/

    }
}
