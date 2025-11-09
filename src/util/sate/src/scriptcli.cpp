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
#include "scriptcli.h"
#include "dobhandler.h"
#include "scriptengine.h"
#include <QCoreApplication>
#include <QFile>
#include <QTextStream>
#include <QTimer>
#include <cstdio>

ScriptCli::ScriptCli(QCoreApplication* app, const QString& scriptFile, const QString& connectionName, const QString& websocketUrl, QObject* parent)
    : QObject(parent)
    , m_app(app)
    , m_scriptFile(scriptFile)
    , m_connectionName(connectionName)
    , m_websocketUrl(websocketUrl)
    , m_dobHandler(nullptr)
    , m_scriptEngine(nullptr)
    , m_currentIndex(0)
{
}

ScriptCli::~ScriptCli()
{
}

int ScriptCli::Execute()
{
    QMetaObject::invokeMethod(this, [this]() { 
        ExecuteInternal();
    }, Qt::QueuedConnection);
    
    return m_app->exec();
}

bool ScriptCli::ExecuteInternal()
{
    // Load script file
    QFile file(m_scriptFile);
    if (!file.open(QIODevice::ReadOnly | QIODevice::Text))
    {
        fprintf(stderr, "Error: Failed to open script file: %s\n", m_scriptFile.toUtf8().constData());
        return false;
    }
    
    QTextStream in(&file);
    QString scriptContent = in.readAll();
    file.close();
    
    if (scriptContent.isEmpty())
    {
        fprintf(stderr, "Error: Script file is empty: %s\n", m_scriptFile.toUtf8().constData());
        return false;
    }
    
    printf("Running script file: %s\n", m_scriptFile.toUtf8().constData());
    
    // Create DobHandler
	m_dobHandler = std::make_unique<DobHandler>();

    // Create ScriptEngine
	m_scriptEngine = std::make_unique<ScriptEngine>(m_dobHandler.get());
    m_scriptEngine->LoadScript(scriptContent);

    // Check if script is valid
    if (!m_scriptEngine->IsValid())
    {
        fprintf(stderr, "Error: Script validation failed:\n");
        for (const auto& error : m_scriptEngine->Errors())
        {
            fprintf(stderr, "  %s\n", error.toUtf8().constData());
        }
        return false;
    }

    // Connect signal to track execution
    connect(m_scriptEngine.get(), &ScriptEngine::IndexFinished, this, &ScriptCli::OnIndexFinished);
    
    // Auto-connect if connection name is provided
    if (!m_connectionName.isEmpty())
    {
        connect(m_dobHandler.get(), &DobHandler::ConnectedToDob, this, [this](const QString&) {
			// Start script execution once connected
            printf("Starting script execution...\n");
            m_scriptEngine->Execute();
        });

        if (!m_websocketUrl.isEmpty())
        {
            // Split websocket URL into address and port
            QStringList parts = m_websocketUrl.split(':');
            if (parts.size() != 2)
            {
                fprintf(stderr, "Error: Invalid websocket URL format. Expected 'address:port', got: %s\n", m_websocketUrl.toUtf8().constData());
                return false;
            }
            
            QString address = parts[0];
            bool ok;
            int port = parts[1].toInt(&ok);
            if (!ok)
            {
                fprintf(stderr, "Error: Invalid port number in websocket URL: %s\n", parts[1].toUtf8().constData());
                return false;
            }
            
            printf("Opening DOB websocket connection: %s (address: %s, port: %d)\n", 
                   m_connectionName.toUtf8().constData(), 
                   address.toUtf8().constData(), 
                   port);
            m_dobHandler->OpenWebsocketConnection(address, port, m_connectionName, 0);
        }
        else
        {
            printf("Opening DOB connection: %s\n", m_connectionName.toUtf8().constData());
            m_dobHandler->OpenNativeConnection(m_connectionName, 0);
        }
    }
    else
    {
		// No auto-connect, start script execution directly
        m_scriptEngine->Execute();
    }
    
    return true;
}

void ScriptCli::OnIndexFinished(int index)
{
    m_currentIndex = index + 1;
    printf("Completed step %d/%d\n", m_currentIndex, m_scriptEngine->Size());
    
    if (m_currentIndex >= m_scriptEngine->Size())
    {
        printf("Script execution completed successfully.\n");
        
        // Close DOB connection before quitting
        m_dobHandler->Close();
        
        // Give some time for cleanup, then quit
        QTimer::singleShot(100, this, [this]() {
            m_app->quit();
        });
    }
}
