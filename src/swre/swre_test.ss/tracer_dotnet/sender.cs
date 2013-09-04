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
        Safir.Application.Tracer razor = new Safir.Application.Tracer("Razor");
        Safir.Application.Tracer rb = new Safir.Application.Tracer("Rymd-Börje");
        razor.Enabled = true;
        rb.Enabled = true;
        rb.WriteLine("blahonga");
        rb.WriteLine("blahong®a");
        rb.WriteLine("blahongaåäö");
        razor.WriteLine("brynanuppafjässasponken");
        razor.WriteLine("{0}{1}{2}",1,2,3.1);
        razor.Write("foo");
        razor.WriteLine("bar");
        razor.WriteLine("this is the end\nmy only friend");

        return 0;
    }
}
