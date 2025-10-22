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
#include "settings_manager.h"

#include <QStandardPaths>
#include <QDir>
#include <QFileInfo>
#include <QVariantList>
#include <QFile>
#include <QMainWindow>
#include <QApplication>
#include <QStatusBar>
#include <QTimer>

#include <Safir/Utilities/Internal/ConfigReader.h>

namespace
{
    const char* kThemeKey   = "Ui/theme";
    const char* kTouchModeKey = "Ui/touchMode";

    inline void ShowStatusBarMessage(const QString& msg)
    {
        if (auto *mw = qobject_cast<QMainWindow*>(QApplication::activeWindow()))
        {
            if (mw->statusBar())
            {
                mw->statusBar()->showMessage(msg, 10000);
            }
        }
    }

} // namespace

SettingsManager::SettingsManager()
    : QObject()
    , m_settings(QString::fromStdString(Safir::Utilities::Internal::ConfigHelper::GetToolsConfigDirectory()
                                        + "/sate.ini"),
                 QSettings::IniFormat)
{
    // Make sure the directory exists
    const QFileInfo fi(m_settings.fileName());
    QDir().mkpath(fi.absolutePath());

    if (m_settings.status() != QSettings::NoError)
        ShowStatusBarMessage(QObject::tr("Cannot read settings file: %1")
                             .arg(m_settings.fileName()));

    // 500-ms single-shot timer to debounce disk writes
    m_flushTimer.setSingleShot(true);
    m_flushTimer.setInterval(500);
    connect(&m_flushTimer, &QTimer::timeout, this, &SettingsManager::flush);
}


void SettingsManager::saveTheme(Theme t)
{
    m_settings.setValue(QString::fromLatin1(kThemeKey), static_cast<int>(t));
    scheduleFlush();
}

SettingsManager::Theme SettingsManager::loadTheme() const
{
    const int val = m_settings.value(QString::fromLatin1(kThemeKey),
                                     static_cast<int>(Theme::Dark)).toInt();
    return static_cast<Theme>(val);
}

void SettingsManager::saveTouchMode(bool touchMode)
{
    m_settings.setValue(QString::fromLatin1(kTouchModeKey), touchMode);
    scheduleFlush();
}

bool SettingsManager::loadTouchMode() const
{
    return m_settings.value(QString::fromLatin1(kTouchModeKey), false).toBool();
}

void SettingsManager::scheduleFlush()
{
    m_flushTimer.start();   // restart 500-ms single-shot timer
}

void SettingsManager::flush()
{
    m_settings.sync();
    if (m_settings.status() == QSettings::AccessError)
        ShowStatusBarMessage(QObject::tr("Cannot write settings file: %1")
                             .arg(m_settings.fileName()));
}

void SettingsManager::clearAll()
{
    m_flushTimer.stop();          // cancel any pending flush

    // Remove all key-value pairs held in memory …
    m_settings.clear();
    m_settings.sync();

    // …and also delete the backing INI file to ensure a clean slate
    QFile::remove(m_settings.fileName());
}

