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
    const char* kTouchModeKey        = "Ui/touchMode";
    const char* kMainWindowGeometryKey = "Ui/mainWindowGeometry";
    const char* kDockLayoutKey         = "Ui/dockLayout";
    const char* kDispatchKey           = "DobHandler/dispatch";
    const char* kSendResponseKey       = "DobHandler/sendResponse";
    const char* kCreateEntitiesKey     = "DobHandler/createEntities";
    const char* kUpdateEntitiesKey     = "DobHandler/updateEntities";
    const char* kDeleteEntitiesKey     = "DobHandler/deleteEntities";
    const char* kResponseKey           = "DobHandler/response";
    const char* kStartupScriptKey      = "Script/startupScript";

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
    if (!m_savingEnabled)
        return;

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
    if (!m_savingEnabled)
        return;

    m_settings.setValue(QString::fromLatin1(kTouchModeKey), touchMode);
    scheduleFlush();
}

bool SettingsManager::loadTouchMode() const
{
    return m_settings.value(QString::fromLatin1(kTouchModeKey), false).toBool();
}

/* ----------  geometry / dock layout persistence ----------------- */
void SettingsManager::saveMainWindowGeometry(const QByteArray& geometry)
{
    if (!m_savingEnabled)
        return;

    m_settings.setValue(QString::fromLatin1(kMainWindowGeometryKey),
                        geometry);
    scheduleFlush();
}

QByteArray SettingsManager::loadMainWindowGeometry() const
{
    return m_settings
            .value(QString::fromLatin1(kMainWindowGeometryKey), QByteArray{})
            .toByteArray();
}

void SettingsManager::saveDockLayout(const QByteArray& layout)
{
    if (!m_savingEnabled)
        return;

    m_settings.setValue(QString::fromLatin1(kDockLayoutKey), layout);
    scheduleFlush();
}

QByteArray SettingsManager::loadDockLayout() const
{
    return m_settings
            .value(QString::fromLatin1(kDockLayoutKey), QByteArray{})
            .toByteArray();
}

void SettingsManager::saveDispatch(bool dispatch)
{
    if (!m_savingEnabled)
        return;

    m_settings.setValue(QString::fromLatin1(kDispatchKey), dispatch);
    scheduleFlush();
}

bool SettingsManager::loadDispatch() const
{
    return m_settings.value(QString::fromLatin1(kDispatchKey), true).toBool();
}

void SettingsManager::saveSendResponse(bool sendResponse)
{
    if (!m_savingEnabled)
        return;

    m_settings.setValue(QString::fromLatin1(kSendResponseKey), sendResponse);
    scheduleFlush();
}

bool SettingsManager::loadSendResponse() const
{
    return m_settings.value(QString::fromLatin1(kSendResponseKey), true).toBool();
}

void SettingsManager::saveCreateEntities(bool createEntities)
{
    if (!m_savingEnabled)
        return;

    m_settings.setValue(QString::fromLatin1(kCreateEntitiesKey), createEntities);
    scheduleFlush();
}

bool SettingsManager::loadCreateEntities() const
{
    return m_settings.value(QString::fromLatin1(kCreateEntitiesKey), true).toBool();
}

void SettingsManager::saveUpdateEntities(bool updateEntities)
{
    if (!m_savingEnabled)
        return;

    m_settings.setValue(QString::fromLatin1(kUpdateEntitiesKey), updateEntities);
    scheduleFlush();
}

bool SettingsManager::loadUpdateEntities() const
{
    return m_settings.value(QString::fromLatin1(kUpdateEntitiesKey), true).toBool();
}

void SettingsManager::saveDeleteEntities(bool deleteEntities)
{
    if (!m_savingEnabled)
        return;

    m_settings.setValue(QString::fromLatin1(kDeleteEntitiesKey), deleteEntities);
    scheduleFlush();
}

bool SettingsManager::loadDeleteEntities() const
{
    return m_settings.value(QString::fromLatin1(kDeleteEntitiesKey), true).toBool();
}

void SettingsManager::saveResponse(const QString& response)
{
    if (!m_savingEnabled)
        return;

    m_settings.setValue(QString::fromLatin1(kResponseKey), response);
    scheduleFlush();
}

QString SettingsManager::loadResponse() const
{
    return m_settings.value(QString::fromLatin1(kResponseKey), QString{}).toString();
}

void SettingsManager::saveStartupScript(const QString& script)
{
    if (!m_savingEnabled)
        return;

    m_settings.setValue(QString::fromLatin1(kStartupScriptKey), script);
    scheduleFlush();
}

QString SettingsManager::loadStartupScript() const
{
    return m_settings.value(QString::fromLatin1(kStartupScriptKey), QString{}).toString();
}

void SettingsManager::scheduleFlush()
{
    if (!m_savingEnabled)
        return;

    m_flushTimer.start();   // restart 500-ms single-shot timer
}

void SettingsManager::flush()
{
    if (!m_savingEnabled)
        return;

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

    // Disable any further saving during this session
    m_savingEnabled = false;
}

