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
#include <random>

namespace sdt = Safir::Dob::Typesystem;
namespace
{
    QString Str(const std::wstring& s) { return QString::fromStdWString(s); }
    
    std::mt19937& GetRandomEngine()
    {
        static std::random_device rd;
        static std::mt19937 gen(rd());
        return gen;
    }
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

int Utilities::RandomInt32(int low, int high)
{
    std::uniform_int_distribution<int> dist(low, high);
    return dist(GetRandomEngine());
}

int64_t Utilities::RandomInt64(int64_t low, int64_t high)
{
    std::uniform_int_distribution<int64_t> dist(low, high);
    return dist(GetRandomEngine());
}

float Utilities::RandomFloat(float low, float high)
{
    std::uniform_real_distribution<float> dist(low, high);
    return dist(GetRandomEngine());
}

double Utilities::RandomDouble(double low, double high)
{
    std::uniform_real_distribution<double> dist(low, high);
    return dist(GetRandomEngine());
}

QString Utilities::RandomString(int length)
{
    static const char charset[] = 
        "0123456789"
        "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        "abcdefghijklmnopqrstuvwxyz";
    static const int charsetSize = sizeof(charset) - 1;
    
    std::uniform_int_distribution<int> dist(0, charsetSize - 1);
    
    QString result;
    result.reserve(length);
    for (int i = 0; i < length; ++i)
    {
        result.append(charset[dist(GetRandomEngine())]);
    }
    return result;
}
