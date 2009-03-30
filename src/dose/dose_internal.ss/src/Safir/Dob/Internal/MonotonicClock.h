/******************************************************************************
*
* Copyright Saab AB, 2008 (http://www.safirsdk.com)
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
#ifndef __DOSE_MONOTONIC_CLOCK_H__
#define __DOSE_MONOTONIC_CLOCK_H__

#include <Safir/Dob/Typesystem/Defs.h>
#include <Safir/Dob/Internal/InternalExportDefs.h>
#include <boost/noncopyable.hpp>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    class DOSE_INTERNAL_API MonotonicClock:
        private boost::noncopyable
    {
    public:
        static Dob::Typesystem::Int64 Get(const Dob::Typesystem::Int64 oldTime);

    private:
        //Not instantiable..
        MonotonicClock();
        ~MonotonicClock();
    };
}
}
}

#endif

