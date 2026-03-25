/******************************************************************************
*
* Copyright Saab AB, 2026 (http://safirsdkcore.com)
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
#include <Safir/Application/Tracer.h>

//disable stupid incorrect microsoft warning.
#ifdef _MSC_VER
#pragma warning (disable : 4428)
#endif

int main()
{
    Safir::Application::Tracer tracer(L"Einstürzende Neubauten");

    tracer.Enable(true);
    for(;;)
    {
        tracer << "Halber Mensch\n";
    }

    return 0;
}


