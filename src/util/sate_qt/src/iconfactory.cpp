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
#include "iconfactory.h"

void IconFactory::LoadIcons()
{
    // TODO: cache icons
}

QIcon IconFactory::GetNamespaceIcon()
{
    return QIcon(":/img/icons/folder_orange.png");
}

QIcon IconFactory::GetEnumIcon()
{
    return QIcon(":/img/icons/enum_orange.png");
}

QIcon IconFactory::GetIcon(TypesystemRepository::DobBaseClass baseClass)
{
    switch (baseClass)
    {
    case TypesystemRepository::Entity:
        return QIcon(":/img/icons/entity_orange");
    case TypesystemRepository::Message:
        return QIcon(":/img/icons/message_orange");
    case TypesystemRepository::Service:
        return QIcon(":/img/icons/gear_orange");
    case TypesystemRepository::Response:
        return QIcon(":/img/icons/response_orange.png");
    case TypesystemRepository::Parametrization:
        return QIcon(":/img/icons/parameters_orange.png");
    case TypesystemRepository::Item:
        return QIcon(":/img/icons/item_orange.png");
    case TypesystemRepository::Struct:
        return QIcon(":/img/icons/struct_orange.png");
    case TypesystemRepository::Object:
        return {};
    default:
        return {};
    }
}
