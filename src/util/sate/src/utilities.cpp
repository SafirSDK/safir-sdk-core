/******************************************************************************
*
* Copyright Saab AB, 2025 (http://safirsdkcore.com)
*
* Created by: Lars Hagström / lars@foldspace.nu
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
#include "utilities.h"
#include <Safir/Dob/Typesystem/Operations.h>

#include <QRegularExpression>

namespace sdt = Safir::Dob::Typesystem;
namespace
{
    QString Str(const std::wstring& s) { return QString::fromStdWString(s); }
}

QString Utilities::EntityIdToString(const Safir::Dob::Typesystem::EntityId& entityId)
{
    QString typeStr;
    try
    {
        typeStr = Str(sdt::Operations::GetName(entityId.GetTypeId()));
    }
    catch (const sdt::IllegalValueException&)
    {
        typeStr.setNum(entityId.GetTypeId());
    }
    return QString("%1 : %2").arg(typeStr, Str(entityId.GetInstanceId().ToString()));

}

std::pair<bool, Safir::Dob::Typesystem::EntityId> Utilities::StringToEntityId(const QString& str)
{
    auto stringList = str.split(QRegularExpression("\\s*:\\s*"), Qt::SkipEmptyParts);
    if (stringList.size() != 2)
    {
        return std::make_pair(false, sdt::EntityId());
    }

    sdt::EntityId eid(StringToTypeId(stringList.at(0)), StringToHashType<sdt::InstanceId>(stringList.at(1)));
    return std::make_pair(true, eid);
}


QRegularExpression Utilities::EntityIdRegex()
{
    // EntityId are written as "TypeId : Instance" where the spaces before and after the colon are optional. No check that typeId exists.
    return QRegularExpression("[0-9A-Za-z.-]+\\s*:\\s*+[0-9A-Za-z.-]+");
}

sdt::TypeId Utilities::StringToTypeId(const QString& s)
{
    bool ok;
    sdt::TypeId typeId = s.trimmed().toLongLong(&ok, 10);
    return ok ? typeId : sdt::Operations::GetTypeId(s.trimmed().toStdWString());
}
