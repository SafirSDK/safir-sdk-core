/******************************************************************************
*
* Copyright Saab AB, 2024 (http://safirsdkcore.com)
*
* Created by: Joel Ottosson
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
#include <Safir/Dob/Typesystem/Object.h>
#include "membertreeitem.h"

class DobObjectBuilder
{
public:
    DobObjectBuilder() = default;

    Safir::Dob::Typesystem::ObjectPtr CreateObject(const MemberTreeItem* objectRoot) const;

private:
    void SetSingleValue(const MemberTreeItem* mi, Safir::Dob::Typesystem::ContainerBase& cb) const;
    void SetSequenceValues(const MemberTreeItem* mi, Safir::Dob::Typesystem::ContainerBase& cb) const;
    void SetDictionaryValues(const MemberTreeItem* mi, Safir::Dob::Typesystem::ContainerBase& cb) const;
};
