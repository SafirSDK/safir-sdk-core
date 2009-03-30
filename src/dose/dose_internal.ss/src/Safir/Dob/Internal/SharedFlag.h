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
#ifndef __DOSE_SHARED_FLAG_H__
#define __DOSE_SHARED_FLAG_H__

#include <Safir/Dob/Internal/Atomic.h>

#include <boost/function.hpp>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    class SharedFlag
    {
    public:
        typedef boost::function<void (void)> SetFunction;
        typedef boost::function<bool (void)> ProcessFunction;

        SharedFlag() : m_flag(0) {}
        explicit SharedFlag(bool isSet) {isSet ? m_flag = 1 : m_flag = 0;}

        //DONT EVER add an IsSet or something like that to this class!
        //It will not work to do something conditionally on whether the
        //flag is set.


        /**
         * Set the flag.
         */
        void Set() {atomic_write32(&m_flag, 1);}


        /**
         * Set the flag and run setFunc if the state changed to set.
         */
        void Set(const SetFunction& setFunc)
        {
            const boost::uint32_t oldVal = atomic_cas32(&m_flag,1,0);
            if (oldVal == 0) //if we changed it to 1 affected the flag
            {
                setFunc();
            }
        }


        /**
         * Process the flag, clearing it in the process.
         *
         * @return Whether the flag has been cleared or not after
         *  running processFunc.
         *  True - means that the flag was either not set (so processFunc was not run),
         *         or that the flag was set and processFunc was run successfully, or
         *         that processFunc returned false but another call to Set was made while
         *         processFunc was running.
         *  False - means that the flag was set and that processFunc returned false but
         *          no call to Set was made while processFunc was running.
         */
        bool Process(const ProcessFunction& processFunc) const
        {
            const boost::uint32_t oldVal = atomic_cas32(&m_flag,0,1);
            if (oldVal == 1)
            {
                if (!processFunc())
                {
                    const boost::uint32_t setAgain = atomic_cas32(&m_flag,1,0);
                    return setAgain != 0;
                }
            }
            return true;
        }

    private:
        mutable volatile boost::uint32_t m_flag;
    };
}
}
}
#endif
