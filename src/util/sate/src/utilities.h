/******************************************************************************
*
* Copyright Saab AB, 2025 (http://safirsdkcore.com)
*
* Created by: Lars Hagström
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

#include <Safir/Dob/Typesystem/EntityId.h>

#include <QString>

class Utilities
{
public:
    Utilities() = delete; //only static methods in this class

    static QString EntityIdToString(const Safir::Dob::Typesystem::EntityId& entityId);

    //first in pair is status of parse
    static std::pair<bool,Safir::Dob::Typesystem::EntityId> StringToEntityId(const QString& str);

    static QRegularExpression EntityIdRegex();

    static Safir::Dob::Typesystem::TypeId StringToTypeId(const QString& s);

    template <class T>
    static T StringToHashType(const QString& s)
    {
        bool ok;
        int64_t num = s.trimmed().toLongLong(&ok);
        return ok ? T(num) : T(s.trimmed().toStdWString());
    }

};
