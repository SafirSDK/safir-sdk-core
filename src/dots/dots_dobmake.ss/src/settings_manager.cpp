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

#include <QDir>
#include <QFileInfo>
#include <QVariantList>
#include <QFile>
#include <Safir/Utilities/Internal/ConfigReader.h>
#include <QMessageBox>

namespace
{
const char* kDouDirKey         = "Dobmake/douDirectory";
const char* kDebugKey          = "Dobmake/debug";
const char* kReleaseKey        = "Dobmake/release";
const char* kRelativeInstallKey= "Dobmake/relativeInstall";
const char* kAbsoluteInstallKey= "Dobmake/absoluteInstall";
const char* kShowLogKey        = "Dobmake/showLog";
const char* kDisableUnityBuildKey = "Dobmake/disableUnityBuild";
} // namespace

SettingsManager::SettingsManager()
    : m_settings(QString::fromUtf8(Safir::Utilities::Internal::ConfigHelper::GetToolsConfigDirectory())
                 + "/dobmake.ini",
                 QSettings::IniFormat)
{
    // Make sure the directory exists
    const QFileInfo fi(m_settings.fileName());
    QDir().mkpath(fi.absolutePath());

    if (m_settings.status() != QSettings::NoError)
        QMessageBox::critical(nullptr,
                              QObject::tr("Settings Error"),
                              QObject::tr("Cannot read settings file: %1")
                                  .arg(m_settings.fileName()));
}

void SettingsManager::clearAll()
{
    // Remove all key-value pairs held in memory …
    m_settings.clear();
    m_settings.sync();

    // …and also delete the backing INI file to ensure a clean slate
    QFile::remove(m_settings.fileName());
}

void SettingsManager::saveDouDirectory(const QString& path)
{
    m_settings.setValue(kDouDirKey, path);
}

void SettingsManager::saveDebug(bool enabled)
{
    m_settings.setValue(kDebugKey, enabled);
}

void SettingsManager::saveRelease(bool enabled)
{
    m_settings.setValue(kReleaseKey, enabled);
}

void SettingsManager::saveRelativeInstall(bool enabled)
{
    m_settings.setValue(kRelativeInstallKey, enabled);
}

void SettingsManager::saveAbsoluteInstall(bool enabled)
{
    m_settings.setValue(kAbsoluteInstallKey, enabled);
}

//-------------------------------------------------------------
// Save helpers
//-------------------------------------------------------------
void SettingsManager::saveShowLog(bool enabled)
{
    m_settings.setValue(kShowLogKey, enabled);
}

void SettingsManager::saveDisableUnityBuild(bool enabled)
{
    m_settings.setValue(kDisableUnityBuildKey, enabled);
}

//-------------------------------------------------------------
// Load helpers
//-------------------------------------------------------------
QString SettingsManager::loadDouDirectory() const
{
    return m_settings.value(kDouDirKey, "").toString();
}

bool SettingsManager::loadDebug() const
{
    return m_settings.value(kDebugKey, false).toBool();
}

bool SettingsManager::loadRelease() const
{
    return m_settings.value(kReleaseKey, false).toBool();
}

bool SettingsManager::loadRelativeInstall() const
{
    return m_settings.value(kRelativeInstallKey, true).toBool();
}

bool SettingsManager::loadAbsoluteInstall() const
{
    return m_settings.value(kAbsoluteInstallKey, false).toBool();
}

//-------------------------------------------------------------
// Load helpers
//-------------------------------------------------------------
bool SettingsManager::loadShowLog() const
{
    return m_settings.value(kShowLogKey, false).toBool();
}

bool SettingsManager::loadDisableUnityBuild() const
{
    return m_settings.value(kDisableUnityBuildKey, false).toBool();
}

