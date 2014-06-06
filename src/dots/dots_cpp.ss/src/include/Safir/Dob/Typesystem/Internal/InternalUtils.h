/******************************************************************************
*
* Copyright Saab AB, 2006-2013 (http://safir.sourceforge.net)
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

#ifndef __DOSE_INTERNAL_UTILS_H__
#define __DOSE_INTERNAL_UTILS_H__

#include <string>
#include <sstream>

#include <Safir/Dob/Typesystem/Defs.h>

//Usage: ENSURE(foo == 10, << "foo had wrong value: " << foo);
#define ENSURE(expr, comment) \
    if (Safir::Dob::Typesystem::Internal::EnsureHelper(expr)); else {std::wostringstream ostr; ostr comment; Safir::Dob::Typesystem::Internal::EnsureFailed(ostr.str());}

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    void DOTS_CPP_API EnsureFailed (const std::wstring & str);

    static inline bool EnsureHelper(const bool expr)
    {
        return expr;
    }

}
}
}
}


#endif

