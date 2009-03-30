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
    class douf_time_test2
    {
        [STAThread]
        static void Main(string[] args)
        {
            double time;
            Console.Write( "Current local time             - " );

            System.DateTime currentTime = new DateTime(0);
            currentTime = DateTime.Now;
            Print.Time(currentTime);

            Console.Write( "Converted to double            - " );

            time = Safir.Time.TimeProvider.ToDouble( currentTime );
            Print.Time( Safir.Time.TimeProvider.ToDateTime(time) );

            Console.Write( "Current UTC time               - " );

            time = Safir.Time.TimeProvider.GetUtcTime();
            Print.Time( Safir.Time.TimeProvider.ToDateTime( time ) );

            Console.Write( "To local time                  - " );

            DateTime dtime = Safir.Time.TimeProvider.ToLocalTime( time );
            Print.Time( dtime );

            Console.Write( "To Utc time                    - " );

            time = Safir.Time.TimeProvider.ToUtcTime( dtime );
            Print.Time( Safir.Time.TimeProvider.ToDateTime( time ) );

            Console.Write( "1 Jan 1970                     - " );

            time = Safir.Time.TimeProvider.ToDouble( new DateTime( 1970, 1, 1 ) );
            Print.Time( Safir.Time.TimeProvider.ToDateTime( time ) );

            Console.Write( "1 Feb 1868                     - " );

            time = Safir.Time.TimeProvider.ToDouble( new DateTime( 1868, 2, 1 ) );
            Print.Time( Safir.Time.TimeProvider.ToDateTime( time ) );

            Console.Write( "1 Feb 2039                     - " );

            time = Safir.Time.TimeProvider.ToDouble( new DateTime( 2039, 2, 1 ) );
            Print.Time( Safir.Time.TimeProvider.ToDateTime( time ) );
        }
    }
}
