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
#include <Safir/Application/Tracer.h>
#include <Safir/Dob/Typesystem/Utilities.h>
#include <boost/thread.hpp>
#include <iostream>

//disable stupid incorrect microsoft warning.
#ifdef _MSC_VER
#pragma warning (disable : 4428)
#endif

int main()
{
    Safir::Application::Tracer razor(L"Razor");
    Safir::Application::Tracer rb(L"Rymd-Börje");
    razor.Enable(true);
    rb.Enable(true);
    rb << L"blahonga" << std::endl;
    rb << L"blahong®a" << std::endl;
    rb << L"blahongaåäö" << std::endl;
    razor << L"brynanuppafjässasponken" << std::endl;
    razor << L"\u202ereversed" << std::endl;
    rb << L"skull and crossbones: \u2620" << std::endl;
    rb << L"interrobang: \u203d" << std::endl;
    razor << 1 << 2 << 3.1 << std::endl;
    razor << "foo" << std::flush << "bar" << std::endl;
    razor << "this is the end\nmy only friend, the end" << std::endl;
    rb << "of our elaborate plans" << std::endl;
    return 0;
}


