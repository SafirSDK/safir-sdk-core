/******************************************************************************
*
* Copyright Saab AB, 2006-2008 (http://www.safirsdk.com)
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

#ifndef __COMMON_INCLUDES_H__
#define __COMMON_INCLUDES_H__

//we include a bunch of extra stuff in here that most of the stress tests use anyway, to reduce 
//the number of places for the warning stuff.
#ifdef _MSC_VER
  #pragma warning(push)
  #pragma warning(disable: 4267)
  #pragma warning(disable: 4512)
  #pragma warning(disable: 4702)
  #pragma warning(disable: 4127)
  #pragma warning(disable: 4800)
  #pragma warning(disable: 4251 4275 4127)
#endif

#include <boost/program_options/cmdline.hpp>
#include <boost/program_options/parsers.hpp>
#include <boost/program_options/variables_map.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/asio.hpp>
#include <boost/thread.hpp>

#ifdef _MSC_VER
  #pragma warning(pop)
#endif

static inline std::wostream& operator<<(std::wostream& out, const boost::program_options::options_description& opt)
{
    std::ostringstream ostr;
    ostr << opt;
    return out << ostr.str().c_str();
}



#endif


