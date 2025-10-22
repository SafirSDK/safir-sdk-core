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

#pragma once

#include <QSettings>
#include <vector>
#include <QObject>
#include <QTimer>

/**
 * Centralised application-wide settings access.
 *
 * Uses an INI file for storage.
 */
class SettingsManager : public QObject
{
    Q_OBJECT
public:
    explicit SettingsManager();

    //Will also stop saving settings after a call to this. App is expected to exit.
    void   clearAll();

    // ------------------------------------------------------------------
    // UI theme handling
    // ------------------------------------------------------------------
    enum class Theme { Light = 0, Dark = 1 };
    void   saveTheme(Theme);
    Theme  loadTheme() const;

    void saveTouchMode(bool touchMode);
    bool loadTouchMode() const;

    // --------------------------------------------------
    // Window / layout persistence
    // --------------------------------------------------
    void   saveMainWindowGeometry(const QByteArray& geometry);
    QByteArray loadMainWindowGeometry() const;

    void   saveDockLayout(const QByteArray& layout);
    QByteArray loadDockLayout() const;

private:
    void scheduleFlush();

private slots:
    void flush();

private:
    QSettings m_settings;                                       // INI backend
    QTimer    m_flushTimer;

    bool      m_savingEnabled { true };
};
