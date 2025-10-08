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

#include <QSettings>
#include <vector>
#include <QObject>
#include <QString>

/**
 * Centralised application-wide settings access.
 *
 * Uses an INI file for storage.
 */
class SettingsManager
{
public:
    explicit SettingsManager();

    void   clearAll();

    // ------------------------------------------------------------------
    // Persistence helpers for Dobmake ----------------------------------
    // ------------------------------------------------------------------
    void saveDouDirectory(const QString& path);
    void saveDebug(bool enabled);
    void saveRelease(bool enabled);
    void saveRelativeInstall(bool enabled);
    void saveAbsoluteInstall(bool enabled);
    void saveShowLog(bool enabled);
    void saveDisableUnityBuild(bool enabled);

    // ------------------------------------------------------------------
    // Load helpers ------------------------------------------------------
    // ------------------------------------------------------------------
    QString loadDouDirectory() const;
    bool    loadDebug() const;
    bool    loadRelease() const;
    bool    loadRelativeInstall() const;
    bool    loadAbsoluteInstall() const;
    bool    loadShowLog() const;
    bool    loadDisableUnityBuild() const;

private:
    QSettings m_settings;                                       // INI backend
};
