/******************************************************************************
*
* Copyright Saab AB, 2006-2013 (http://safirsdkcore.com)
*
* Created by: Erik Adolfsson / sterad
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
#include <Safir/Dob/Typesystem/Defs.h>
#include <Safir/Dob/Typesystem/Exceptions.h>
#include <Safir/Time/TimeProvider.h>
#include <Safir/Time/Internal/Interface.h>

namespace Safir
{
namespace Time
{

    //---------------------------------------------------------------------------
    Safir::Dob::Typesystem::Si64::Second TimeProvider::GetUtcTime()
    {
        Safir::Dob::Typesystem::Si64::Second utcTime;
        bool success;
        DoufTimeC_GetUtcTime(utcTime, success);
        if (!success)
        {
            throw Safir::Dob::Typesystem::ConfigurationErrorException(L"Configuration error in TimeProvider, please check your logs!",__WFILE__, __LINE__);
        }

        return utcTime;
    }

    //---------------------------------------------------------------------------
    Safir::Dob::Typesystem::Int32 TimeProvider::GetLocalTimeOffset()
    {
        Safir::Dob::Typesystem::Int32 offset;
        bool success;
        DoufTimeC_GetLocalTimeOffset(offset, success);
        if (!success)
        {
            throw Safir::Dob::Typesystem::ConfigurationErrorException(L"Configuration error in TimeProvider, please check your logs!",__WFILE__, __LINE__);
        }

        return offset;
    }

} // namespace Time
} // namespace Safir
