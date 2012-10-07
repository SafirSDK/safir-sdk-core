/******************************************************************************
*
* Copyright Saab AB, 2011 (http://www.safirsdk.com)
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
#include <iostream>

#if defined _MSC_VER
  #pragma warning (push)
  #pragma warning (disable : 4244)
#endif

#include <boost/lexical_cast.hpp>
#include <boost/thread.hpp>

#if defined _MSC_VER
  #pragma warning (pop)
#endif

int main(int argc, char* argv[])
{
    if (argc != 2)
    {
        std::wcout << "Need one numeric argument (seconds to sleep)" << std::endl;
        return 1;
    }

    boost::this_thread::sleep(boost::posix_time::seconds(boost::lexical_cast<int>(argv[1])));
    return 0;
}

