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

namespace douf_time_test
{
    /// <summary>
    /// Summary description for print_time.
    /// </summary>
    public class Print
    {
        public Print()
        {
        }

        static public void Time(System.DateTime t)
        {
            System.DateTime d = new DateTime(t.Year,t.Month,t.Day,t.Hour,t.Minute,0);
            System.TimeSpan ts = t-d;
            long nsec = (long)((ts.TotalSeconds - (double)t.Second) * 1000000.0 + 0.5);

            long m = 100000;
            Console.Write(t.ToString() + ".");
            if (nsec > 0)
            {
                while (nsec <= m)
                {
                    Console.Write( "0" );
                    m = m / 10;
                }
            }
            Console.WriteLine( nsec );
        }
    }
}
