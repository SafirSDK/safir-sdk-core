/******************************************************************************
*
* Copyright Saab Systems AB, 2012 (http://www.safirsdk.com)
*
* Created by: Lars Hagstr�m / lars@foldspace.nu
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
#include <boost/asio.hpp>

#include <boost/shared_ptr.hpp>
#include "config.h"

int main()
{
    boost::shared_ptr<int> foo;
    foo.reset(new int);
    *foo = 10;
#if defined(CMAKE_HAVE_FENCED_BLOCK_HPP)
    boost::asio::detail::fenced_block f(boost::asio::detail::fenced_block::full);
#endif
    return 0;
}

