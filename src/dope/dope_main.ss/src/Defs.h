/******************************************************************************
*
* Copyright Saab AB, 2006-2013 (http://safirsdkcore.com)
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
#pragma once

#include <set>
#include <Safir/Dob/Typesystem/Defs.h>
#include <Safir/Dob/Typesystem/EntityId.h>
#include <stdexcept>

typedef std::set<Safir::Dob::Typesystem::TypeId> TypeIdSet;
typedef std::set<Safir::Dob::Typesystem::EntityId> EntityIdSet;


class StartupError
    : public std::logic_error
{
public:
    StartupError() : std::logic_error("StartupError") {}
};
