/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safirsdkcore.com)
*
* Created by: Joel Ottosson / stjoot
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

#ifndef _SAFIR_DOB_DEFS_H
#define _SAFIR_DOB_DEFS_H

#include <Safir/Dob/Typesystem/Defs.h>
#include <boost/shared_ptr.hpp>

namespace Safir
{
namespace Dob
{
    //-----------------------------
    // Type definitions
    //-----------------------------

    /**
     * Represents the id of a request.
     */
    typedef Safir::Dob::Typesystem::Int32 RequestId;

    typedef Safir::Dob::Typesystem::Int32 ContextId;

};
};

#endif
