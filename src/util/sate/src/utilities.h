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

#include <Safir/Time/TimeProvider.h>
#include <Safir/Dob/Typesystem/EntityId.h>
#include <QString>
#include <qmath.h>

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

    template <class SecondsT>
    static QString SecondsToTooltipText(SecondsT seconds)
    {
        try
        {
            return QString("Seconds:\t%1\n"
                           "UTC:\t%2\n"
                           "Local:\t%3")
                .arg(seconds)
                .arg(QString::fromStdString(boost::posix_time::to_iso_extended_string(Safir::Time::TimeProvider::ToPtime(seconds))).replace("T", " "))
                .arg(QString::fromStdString(boost::posix_time::to_iso_extended_string(Safir::Time::TimeProvider::ToLocalTime(seconds))).replace("T", " "));
        } catch (const boost::gregorian::bad_year&) {} // valid year range is year 1400 - 9999

        return QString("Seconds:\t%1").arg(seconds);
    }

    template <class RadiansT>
    static QString RadiansToTooltipText(RadiansT rad)
    {
        return QString("Radians:\t%1\nDegrees:\t%2").arg(rad).arg(QString::number(qRadiansToDegrees(rad), 'f', 2));
    }
};
