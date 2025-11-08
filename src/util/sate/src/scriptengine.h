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
#include <QJsonDocument>
#include <QJsonObject>
#include <QJsonArray>
#include <QJsonValue>
#include <QJsonValueRef>
#include <QJsonValueConstRef>
#include <QTimer>
#include <Safir/Dob/Typesystem/Serialization.h>

class DobHandler; // Forward declaration

class ScriptEngine : public QObject
{
    Q_OBJECT
public:
    enum State
    {
        Stopped,
        Executing,
        Recording
    };

    ScriptEngine(DobHandler* dobHandler);

    void LoadScript(const QString& json);

    void Record();

    // Run script from currentIndex to the end. Steps up currentIndex for every execution step.
    void Execute();

    // Execute specific index without modifying currentIndex
    // Returns millisecs until next step should be executed.
    int ExecuteIndex(int index);

    // Stop execution and set currentIndex to 0
    void Reset();

    // Stop execution but do not reset currentIndes.
    void Pause();

    // List of error messages, emtpty if script is valid.
    bool IsValid() const;
    const QStringList& Errors() const;

    int Size() const;

    QJsonObject GetIndexObject(int index) const;
    
    void DeleteIndex(int index);
    
    QString GetScript() const;
    
    void AppendScriptItem(const QJsonObject& item);
    
    State CurrentState() const { return m_state; }

signals:    
    void IndexFinished(int index);
    void ItemRecorded(const QJsonObject& item);

private:
    State m_state = Stopped;
    int m_currentIndex = 0;
    QStringList m_errors;
    QJsonArray m_items;
    DobHandler* m_dobHandler = nullptr;

    void ExecuteNext();
    bool IsFinished() const;
};
