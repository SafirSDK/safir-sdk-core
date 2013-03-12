/******************************************************************************
*
* Copyright Saab AB, 2013 (http://www.safirsdk.com)
*
* Created by: Anders Widén
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
#include <Safir/Utilities/LogInterface.h>

#if defined(linux) || defined(__linux) || defined(__linux__)


#elif defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
#ifndef _WIN32_WINNT
  #define _WIN32_WINNT 0x0501
#endif
//#include <windows.h>
//#include <psapi.h>
//#include <process.h>
#endif


namespace Safir
{
namespace Utilities
{

void LogInterface::Log(const Severity severity, const std::string& text)
{

}


}
}
