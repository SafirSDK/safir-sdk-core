/******************************************************************************
*
* Copyright Saab AB, 2011-2013 (http://safir.sourceforge.net)
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

int main(int argc, char ** argv)
{
    bool enable = false;
    if (argc == 2 && std::string(argv[1]) == "enable")
    {
        enable = true;
    }

    Safir::Application::Tracer razor(L"Razor");
    Safir::Application::Tracer rb(L"Rymd-B\u00f6rje"); //ö
    if (enable)
    {
        razor.Enable(true);
        rb.Enable(true);
    }
    rb << L"blahonga" << std::endl;
    rb << L"blahong\u00aea" << std::endl; //registered sign
    rb << L"blahonga\u00e5\u00e4\u00f6" << std::endl; //åäö
    razor << L"brynanuppafj\u00e4ssasponken" << std::endl; //ä
    razor << L"\u202ereversed" << std::endl;
    rb << L"skull and crossbones: \u2620" << std::endl;
    rb << L"interrobang: \u203d" << std::endl;
    razor << 1 << 2 << 3.1 << std::endl;
    razor << "foo" << std::flush << "bar" << std::endl;
    razor << "this is the end\nmy only friend, the end" << std::endl;
    rb << "of our elaborate plans" << std::endl;
    return 0;
}


