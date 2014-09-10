/******************************************************************************
*
* Copyright Saab AB, 2012-2013 (http://safir.sourceforge.net)
*
* Created by: Lars Hagström / lars@foldspace.nu
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
#include <boost/noncopyable.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/version.hpp>

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4267)
#endif

#include <boost/asio.hpp>

#ifdef _MSC_VER
#pragma warning (pop)
#endif

#include "config.h"

int main()
{
    boost::shared_ptr<int> foo;
    foo.reset(new int);
    *foo = 10;
#if defined(CMAKE_HAVE_FENCED_BLOCK_HPP)
#if ((BOOST_VERSION / 100000) >= 1 && (BOOST_VERSION / 100 % 1000) >= 48)
    boost::asio::detail::fenced_block f(boost::asio::detail::fenced_block::full);
#else
    boost::asio::detail::fenced_block f();
#endif
#endif
    return 0;
}

