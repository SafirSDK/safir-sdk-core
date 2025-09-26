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

#include <QString>
#include <QApplication>
#include <QMainWindow>
#include <QStatusBar>

// Utility helper to show a message in the application status bar for a
// limited amount of time (default 5 s).  No-op if no active main window.
inline void ShowStatusBarMessage(const QString& msg, int timeoutMs = 5000)
{
    if (auto *mw = qobject_cast<QMainWindow*>(QApplication::activeWindow()))
    {
        if (mw->statusBar())
        {
            mw->statusBar()->showMessage(msg, timeoutMs);
        }
    }
}
