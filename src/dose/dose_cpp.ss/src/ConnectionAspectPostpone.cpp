/******************************************************************************
*
* Copyright Saab AB, 2008 (http://www.safirsdk.com)
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

#include <Safir/Dob/ConnectionAspectPostpone.h>
#include <Safir/Dob/Typesystem/LibraryExceptions.h>
#include <Safir/Dob/Internal/Interface.h>

namespace Safir
{
namespace Dob
{
    void ConnectionAspectPostpone::Postpone(const bool redispatchCurrent) const
    {
        bool success;
        DoseC_Postpone(GetControllerId(),redispatchCurrent, success);
        if (!success)
        {
            Typesystem::LibraryExceptions::Instance().Throw();
        }
    }


    void ConnectionAspectPostpone::ResumePostponed() const
    {
        bool success;
        DoseC_ResumePostponed(GetControllerId(), success);
        if (!success)
        {
            Typesystem::LibraryExceptions::Instance().Throw();
        }
    }

    void ConnectionAspectPostpone::IncompleteInjectionState() const
    {
        bool success;
        DoseC_IncompleteInjectionState(GetControllerId(), success);
        if (!success)
        {
            Typesystem::LibraryExceptions::Instance().Throw();
        }
    }

}
}
