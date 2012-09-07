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
    static int Main(String[] args)
    {
        Safir.Application.Tracer debug = new Safir.Application.Tracer("test");
        debug.Enabled = true;
        debug.WriteLine("blahonga");
        debug.WriteLine("blahonga");
        debug.WriteLine("blahonga");
        
        System.Threading.Thread.Sleep(100);
        debug.WriteLine("blahonga");
        System.Threading.Thread.Sleep(2000);
        debug.WriteLine("blahonga");
        
        Safir.SwReports.SwReport.Stop();
        return 0;
    }
}
