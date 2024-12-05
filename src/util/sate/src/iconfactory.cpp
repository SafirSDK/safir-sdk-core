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
QPixmap GetIconPixmap(TypesystemRepository::DobBaseClass baseClass, bool hasLetter)
{
    switch (baseClass)
    {
    case TypesystemRepository::Entity:
        return QPixmap(":/img/icons/entity_orange");
    case TypesystemRepository::Message:
        return QPixmap(":/img/icons/message_orange");
    case TypesystemRepository::Service: // For visibility a special version of gearIcon is used if letter is to be drawn
        return hasLetter ? QPixmap(":/img/icons/gear_orange") : QPixmap(":/img/icons/gear_orange_filled");
    case TypesystemRepository::Response:
        return QPixmap(":/img/icons/response_orange.png");
    case TypesystemRepository::Parametrization:
        return QPixmap(":/img/icons/parameters_orange.png");
    case TypesystemRepository::Item:
        return QPixmap(":/img/icons/item_orange.png");
    case TypesystemRepository::Struct:
        return QPixmap(":/img/icons/struct_orange.png");
    case TypesystemRepository::Object:
        return {};
    default:
        return {};
    }
}

QPixmap GetLetterPixmap(char letter)
{
    if (letter == 's')
    {
        return QPixmap(":/img/icons/s_black");

    }
    else if (letter == 'r')
    {
        return QPixmap(":/img/icons/r_black");
    }
    else if (letter == 'p')
    {
        return QPixmap(":/img/icons/p_black");
    }

    return {};
}

QPoint GetOffset(TypesystemRepository::DobBaseClass baseClass)
{
    switch (baseClass)
    {
    case TypesystemRepository::Entity:
        return QPoint{4, 13};
    case TypesystemRepository::Message:
        return QPoint{11, 5};
    case TypesystemRepository::Service:
        return QPoint{11, 7};
    default:
        return {};
    }
}
}

QIcon IconFactory::GetNamespaceIcon()
{
    return QIcon(":/img/icons/folder_orange.png");
}

QIcon IconFactory::GetEnumIcon()
{
    return QIcon(":/img/icons/enum_orange.png");
}

QIcon IconFactory::GetIcon(TypesystemRepository::DobBaseClass baseClass, char letter)
{
    auto hasLetter = letter != ' ';
    auto pixmap = GetIconPixmap(baseClass, hasLetter);
    if (hasLetter)
    {
        auto l = GetLetterPixmap(letter);
        auto offset = GetOffset(baseClass);
        QPainter painter(&pixmap);
        painter.drawPixmap(offset, l);
    }
    return QIcon(pixmap);
}
