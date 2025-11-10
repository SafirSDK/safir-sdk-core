/******************************************************************************
*
* Copyright Saab AB, 2025 (http://safirsdkcore.com)
*
* Created by: Joel Ottosson
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

#include <QObject>
#include <QString>

class QCoreApplication;
class DobHandler;
class ScriptEngine;

class ScriptCli : public QObject
{
    Q_OBJECT
public:
    explicit ScriptCli(QCoreApplication* app, const QString& scriptFile, const QString& connectionName, const QString& websocketUrl, QObject* parent = nullptr);
    ~ScriptCli();

    // Execute the script and return exit code
    int Execute();

private slots:
    void OnIndexFinished(int index);

private:
    QCoreApplication* m_app;
    QString m_scriptFile;
    QString m_connectionName;
    QString m_websocketUrl;
    std::unique_ptr<DobHandler> m_dobHandler;
    std::unique_ptr<ScriptEngine> m_scriptEngine;
    int m_currentIndex;

    bool ExecuteInternal();
};
