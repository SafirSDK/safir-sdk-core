/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
*
* Created by: Anders Widén / stawi
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
#ifndef __DOSE_SHARED_LOCK_H__
#define __DOSE_SHARED_LOCK_H__

#include <boost/shared_ptr.hpp>
#include <boost/mem_fn.hpp>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    /**
     * A class to be used when there is a need to return a lock from a function.
     * Note that an ordinary shared _ptr is used so a SharedLock instance can NOT
     * be stored in shared memory.
     */
    class SharedLock
    {
    public:

        template<class Lock> explicit SharedLock(Lock& lck): lckPtr((lck.lock(), &lck), boost::mem_fn(&Lock::unlock)) {}

        SharedLock() {}

    private:

        boost::shared_ptr<void> lckPtr;
    };
}
}
}
#endif
