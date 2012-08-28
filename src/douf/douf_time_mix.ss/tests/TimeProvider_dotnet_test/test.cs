/******************************************************************************
*
* Copyright Saab AB, 2011 (http://www.safirsdk.com)
*
* Created by: Lars Hagstrom
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

class Sender
{
    static TimeSpan GetUtcOffset()
    {
        return TimeZoneInfo.Local.GetUtcOffset(DateTime.UtcNow);
    }

    static readonly DateTime a_date = new DateTime(2010,2,24,1,5,0);
    static int Main(String[] args)
    {
        double as_double = Safir.Time.TimeProvider.ToDouble(a_date);

        //correct value calculated with online utc converter.
        if (as_double != 1266973500.0)
        {
            Console.WriteLine("Safir.Time.TimeProvider.ToDouble(ptime) returned incorrect value");
            return 1;
        }

        DateTime back_to_datetime = Safir.Time.TimeProvider.ToDateTime(as_double);
        if (back_to_datetime != a_date)
        {
            Console.WriteLine("Safir.Time.TimeProvider.ToDateTime(double) returned incorrect value");
            return 1;
        }

        DateTime as_localtime = Safir.Time.TimeProvider.ToLocalTime(as_double);
        if (as_localtime - a_date != GetUtcOffset())
        {
            Console.WriteLine("Safir.Time.TimeProvider.ToLocalTime(double) returned incorrect value");
            return 1;
        }

        double back_to_utc_double = Safir.Time.TimeProvider.ToUtcTime(as_localtime);
        if (back_to_utc_double != as_double)
        {
            Console.WriteLine("Safir.Time.TimeProvider.ToUtcTime(DateTime) returned incorrect value");
            return 1;
        }

        double now_safir = Safir.Time.TimeProvider.GetUtcTime();
        double now_dotnet = (DateTime.UtcNow - new DateTime(1970,1,1)).TotalSeconds;
        double diff = Math.Abs(now_safir - now_dotnet);

        if (diff > 1.0e-3)
        {
            Console.WriteLine("Safir::Time::TimeProvider::GetUtcTime() returned incorrect value");
            return 1;
        }
        


        return 0;
    }
}
