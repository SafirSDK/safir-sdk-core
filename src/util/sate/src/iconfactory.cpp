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
#include <QPixmap>
#include <QPainter>
#include <QColor>
#include <QPoint>

namespace
{
QPixmap GetIconPixmap(TypesystemRepository::DobBaseClass baseClass, IconFactory::Modifier modifier)
{
    switch (baseClass)
    {
    case TypesystemRepository::Entity:
        return modifier == IconFactory::SubscribeRecursive ? QPixmap(":/img/icons/entity_orange_filled") : QPixmap(":/img/icons/entity_orange");
    case TypesystemRepository::Message:
        return QPixmap(":/img/icons/message_orange");
    case TypesystemRepository::Service:
        return modifier != IconFactory::None ? QPixmap(":/img/icons/gear_orange_filled") : QPixmap(":/img/icons/gear_orange");
    case TypesystemRepository::Response:
        return QPixmap(":/img/icons/response_orange");
    case TypesystemRepository::Parametrization:
        return QPixmap(":/img/icons/parameters_orange");
    case TypesystemRepository::Item:
        return QPixmap(":/img/icons/item_orange");
    case TypesystemRepository::Struct:
        return QPixmap(":/img/icons/struct_orange");
    case TypesystemRepository::Object:
        return {};
    default:
        return {};
    }
}

QPixmap GetModifierPixmap(IconFactory::Modifier modifier)
{
    switch (modifier)
    {
    case IconFactory::None: return {};
    case IconFactory::Register: return QPixmap(":/img/icons/r_black");
    case IconFactory::Pending: return QPixmap(":/img/icons/p_black");
    case IconFactory::Subscribe: return QPixmap(":/img/icons/s_black");
    case IconFactory::SubscribeRecursive: return QPixmap(":/img/icons/s_plus_black");
    }

    return {};
}

QPoint GetOffset(TypesystemRepository::DobBaseClass baseClass, IconFactory::Modifier modifier)
{
    switch (baseClass)
    {
    case TypesystemRepository::Entity:
        return QPoint{4, 13};
    case TypesystemRepository::Message:
        return modifier == IconFactory::SubscribeRecursive ? QPoint{8, 5} : QPoint{11, 5};
    case TypesystemRepository::Service:
        return QPoint{11, 7};
    default:
        return {};
    }
}
}

QIcon IconFactory::GetNamespaceIcon()
{
    return QIcon(":/img/icons/folder_orange");
}

QIcon IconFactory::GetEnumIcon()
{
    return QIcon(":/img/icons/enum_orange");
}

QIcon IconFactory::GetSearchIcon()
{
    return QIcon(":/img/icons/magnifying-glass-search.svg");
}

QIcon IconFactory::GetScriptIcon()
{
    return QIcon(":/img/icons/script-orange.svg");
}

QIcon IconFactory::GetIcon(TypesystemRepository::DobBaseClass baseClass, IconFactory::Modifier modifier)
{
    auto pixmap = GetIconPixmap(baseClass, modifier);
    if (modifier != IconFactory::None)
    {
        auto letter = GetModifierPixmap(modifier);
        auto offset = GetOffset(baseClass, modifier);
        QPainter painter(&pixmap);
        painter.drawPixmap(offset, letter);
    }
    return QIcon(pixmap);
}
