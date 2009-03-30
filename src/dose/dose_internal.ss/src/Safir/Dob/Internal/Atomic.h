/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
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

#ifndef __DOSE_ATOMIC_H__
#define __DOSE_ATOMIC_H__

#include <boost/interprocess/detail/atomic.hpp>

namespace Safir
{
namespace Dob
{
namespace Internal
{

    using boost::interprocess::detail::atomic_inc32;
    using boost::interprocess::detail::atomic_dec32;
    using boost::interprocess::detail::atomic_read32;
    using boost::interprocess::detail::atomic_write32;
    using boost::interprocess::detail::atomic_cas32;

}
}
}

#endif

