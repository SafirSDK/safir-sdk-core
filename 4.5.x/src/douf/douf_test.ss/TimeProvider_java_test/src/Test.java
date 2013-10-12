/******************************************************************************
*
* Copyright Saab AB, 2013 (http://safir.sourceforge.net)
*
* Created by: Lars Hagström / lars.hagstrom@consoden.se
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
public class Test {
    public static void main(String[] args) {
        java.util.GregorianCalendar cal = new java.util.GregorianCalendar(java.util.TimeZone.getTimeZone("UTC"));
        cal.setTimeInMillis(0);
        cal.set(2010,1,24,1,5,0);
        java.util.Date a_date = cal.getTime();

        double as_double = com.saabgroup.safir.time.TimeProvider.ToDouble(a_date);

        //correct value calculated with online utc converter.
        if (as_double != 1266973500.0)
        {
            System.out.println("com.saabgroup.safir.time.TimeProvider.ToDouble(ptime) returned incorrect value");
            System.exit(1);
        }
        
        java.util.Date back_to_datetime = com.saabgroup.safir.time.TimeProvider.ToDate(as_double);
        if (!back_to_datetime.equals(a_date))
        {
            System.out.println("com.saabgroup.safir.time.TimeProvider.ToDate(double) returned incorrect value");
            System.exit(1);
        }

        if (com.saabgroup.safir.time.TimeProvider.getLocalTimeOffset() != 
            java.util.TimeZone.getDefault().getOffset(new java.util.Date().getTime()) / 1000)
        {
            System.out.println("com.saabgroup.safir.time.TimeProvider.getLocalTimeOffset() returned incorrect value");
            System.exit(1);
        }

        double now_safir = com.saabgroup.safir.time.TimeProvider.getUtcTime();
        double now_java = new java.util.Date().getTime() / 1000.0;
        double diff = Math.abs(now_safir - now_java);

        if (diff > 0.1)
        {
            System.out.println("Safir.Time.TimeProvider.getUtcTime() returned incorrect value");
            System.exit(1);
        }

        System.exit (0);
    }



}
