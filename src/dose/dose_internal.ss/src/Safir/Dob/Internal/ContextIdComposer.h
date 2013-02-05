/******************************************************************************
*
* Copyright Saab AB, 2009 (http://www.safirsdk.com)
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
#ifndef __DOSE_CONTEXT_ID_COMPOSER_H__
#define __DOSE_CONTEXT_ID_COMPOSER_H__

#include <Safir/Dob/Defs.h>
#include <Safir/Dob/Typesystem/Internal/InternalUtils.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    inline
    const ContextId ComposeMinusOneContext(const ContextId context)
    {
        ENSURE(context >= 0 && context < 1000000, << "Invalid context!");
        
        return (context + 1000000) * -1;
    }

    inline
    const ContextId ComposeContext(const ContextId minusOneContext)
    {
        ENSURE(minusOneContext <= -1000000, << "Invalid \"minus one\" context!");

        return abs(minusOneContext) - 1000000;        
    }
}
}
}

#endif

