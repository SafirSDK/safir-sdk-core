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
#include "settings_manager.h"

#include <QStandardPaths>
#include "common.h"
#include <QDir>
#include <QFileInfo>
#include <QVariantList>
#include <QFile>
#include <Safir/Utilities/Internal/ConfigReader.h>

namespace
{
const char* kRulesKey  = "HighlightRules/list";
const char* kThemeKey   = "Ui/theme";
const char* kTouchModeKey = "Ui/touchMode";
const char* kColumnRoot = "LiveLog/columns";
} // namespace

SettingsManager::SettingsManager()
    : m_settings(QString::fromUtf8(Safir::Utilities::Internal::ConfigHelper::GetToolsConfigDirectory()
                                   + "/tracer_viewer.ini"),
                 QSettings::IniFormat)
{
    // Make sure the directory exists
    const QFileInfo fi(m_settings.fileName());
    QDir().mkpath(fi.absolutePath());

    if (m_settings.status() != QSettings::NoError)
        ShowStatusBarMessage(QObject::tr("Cannot read settings file: %1")
                             .arg(m_settings.fileName()));
}

// --------------------------------------------------------------------------
void SettingsManager::saveHighlightRules(const std::vector<HighlightRule>& rules)
{
    QVariantList list;
    list.reserve(static_cast<int>(rules.size()));
    for (const auto& r : rules)
    {
        QVariantMap m;
        m[QStringLiteral("pattern")] = r.regex.pattern();
        m[QStringLiteral("color")]   = r.color.name(QColor::HexArgb);
        list << m;
    }
    m_settings.setValue(QString::fromLatin1(kRulesKey), list);
    m_settings.sync();

    if (m_settings.status() == QSettings::AccessError)
        ShowStatusBarMessage(QObject::tr("Cannot write settings file: %1")
                             .arg(m_settings.fileName()));
}

// --------------------------------------------------------------------------
std::vector<HighlightRule> SettingsManager::loadHighlightRules() const
{
    std::vector<HighlightRule> rules;
    const QVariantList list = m_settings.value(QString::fromLatin1(kRulesKey)).toList();
    rules.reserve(list.size());

    for (const QVariant& v : list)
    {
        const QVariantMap m = v.toMap();
        HighlightRule r;
        r.regex = QRegularExpression(
            m.value(QStringLiteral("pattern")).toString(),
            QRegularExpression::CaseInsensitiveOption);
        r.color = QColor(m.value(QStringLiteral("color")).toString());
        rules.push_back(std::move(r));
    }
    return rules;
}

void SettingsManager::saveTheme(Theme t)
{
    m_settings.setValue(QString::fromLatin1(kThemeKey), static_cast<int>(t));
    m_settings.sync();

    if (m_settings.status() == QSettings::AccessError)
        ShowStatusBarMessage(QObject::tr("Cannot write settings file: %1")
                             .arg(m_settings.fileName()));
}

SettingsManager::Theme SettingsManager::loadTheme() const
{
    const int val = m_settings.value(QString::fromLatin1(kThemeKey),
                                     static_cast<int>(Theme::Light)).toInt();
    return static_cast<Theme>(val);
}

void SettingsManager::saveTouchMode(bool touchMode)
{
    m_settings.setValue(QString::fromLatin1(kTouchModeKey), touchMode);
    m_settings.sync();

    if (m_settings.status() == QSettings::AccessError)
        ShowStatusBarMessage(QObject::tr("Cannot write settings file: %1")
                             .arg(m_settings.fileName()));
}

bool SettingsManager::loadTouchMode() const
{
    return m_settings.value(QString::fromLatin1(kTouchModeKey), false).toBool();
}

void SettingsManager::clearAll()
{
    m_settings.clear();
    m_settings.sync();
    QFile::remove(m_settings.fileName());
}

// --------------------------------------------------------------------------
void SettingsManager::saveColumnState(const QString& id,
                                      const QList<int>& sizes,
                                      const QList<bool>& visibility)
{
    QVariantList sz;  for (int w  : sizes)      sz  << w;
    QVariantList vis; for (bool v : visibility) vis << v;

    const QString base = QString::fromLatin1(kColumnRoot) + id + '/';
    m_settings.setValue(base + "sizes",      sz);
    m_settings.setValue(base + "visibility", vis);
    m_settings.sync();

    if (m_settings.status() == QSettings::AccessError)
        ShowStatusBarMessage(QObject::tr("Cannot write settings file: %1")
                             .arg(m_settings.fileName()));
}

// --------------------------------------------------------------------------
bool SettingsManager::loadColumnState(const QString& id,
                                      QList<int>& sizes,
                                      QList<bool>& visibility) const
{
    const QString base = QString::fromLatin1(kColumnRoot) + id + '/';
    const QVariantList sz  = m_settings.value(base + "sizes").toList();
    const QVariantList vis = m_settings.value(base + "visibility").toList();

    if (sz.isEmpty() || vis.isEmpty() || sz.size() != vis.size())
        return false;

    sizes.clear();      visibility.clear();
    for (const QVariant& v : sz)  sizes      << v.toInt();
    for (const QVariant& v : vis) visibility << v.toBool();
    return true;
}
