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
#include <iostream>
#include <Safir/Dob/Typesystem/Operations.h>
#include <Safir/Dob/Typesystem/Convenience.h>


ScriptCli::ScriptCli(QCoreApplication* app, const QString& scriptFile, const QString& connectionName, const QString& websocketUrl, bool verbose, QObject* parent)
    : QObject(parent)
    , m_app(app)
    , m_scriptFile(scriptFile)
    , m_connectionName(connectionName)
    , m_websocketUrl(websocketUrl)
    , m_verbose(verbose)
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
        std::cerr << "Error: Failed to open script file: " << m_scriptFile.toUtf8().constData() << std::endl;
        return false;
    }

    QTextStream in(&file);
    QString scriptContent = in.readAll();
    file.close();

    if (scriptContent.isEmpty())
    {
        std::cerr << "Error: Script file is empty: " << m_scriptFile.toUtf8().constData() << std::endl;
        return false;
    }

    if (m_verbose)
    {
        std::cout << "Running script file: " << m_scriptFile.toUtf8().constData() << std::endl;
    }

    // Create DobHandler
    m_dobHandler = std::make_unique<DobHandler>();

    if (m_verbose)
    {
        // Connect to output messages
        connect(m_dobHandler.get(), &DobHandler::Output, this, [this](const QString& message, QtMsgType) {
            std::cout << message.toUtf8().constData() << std::endl;
        });

        // Connect to OnMessage signal
        connect(m_dobHandler.get(), &DobHandler::OnMessage, this, [this](const sdt::ChannelId& channel, const Safir::Dob::MessagePtr& message) {
            std::cout << "OnMessage: " << Str(Safir::Dob::Typesystem::Operations::GetName(message->GetTypeId()))
                      << " on channel " << channel.GetRawValue() << std::endl;
        });

        // Connect to OnEntity signal
        connect(m_dobHandler.get(), &DobHandler::OnEntity, this, [this](const sdt::EntityId& entityId, const sdt::HandlerId&, const Safir::Dob::EntityPtr&, DobInterface::EntityOperation operation) {
            std::string opStr;
            switch(operation) {
                case DobInterface::NewEntity: opStr = "New"; break;
                case DobInterface::UpdatedEntity: opStr = "Update"; break;
                case DobInterface::DeletedEntity: opStr = "Delete"; break;
                default: opStr = "Unknown"; break;
            }
            std::cout << "OnEntity: " << opStr << " "
                      << Str(Safir::Dob::Typesystem::Operations::GetName(entityId.GetTypeId()))
                      << ", instance=" << entityId.GetInstanceId().GetRawValue() << std::endl;
        });

        // Connect to OnResponse signal
        connect(m_dobHandler.get(), &DobHandler::OnResponse, this, [this](const Safir::Dob::ResponsePtr& response) {
            std::cout << "OnResponse: " << Str(Safir::Dob::Typesystem::Operations::GetName(response->GetTypeId())) << std::endl;
        });

        // Connect to OnCreateRequest signal
        connect(m_dobHandler.get(), &DobHandler::OnCreateRequest, this, [this](const Safir::Dob::EntityPtr& request, const sdt::HandlerId&, const sdt::InstanceId& instance) {
            std::cout << "OnCreateRequest: " << Str(Safir::Dob::Typesystem::Operations::GetName(request->GetTypeId()))
                      << ", instance=" << instance.GetRawValue() << std::endl;
        });

        // Connect to OnUpdateRequest signal
        connect(m_dobHandler.get(), &DobHandler::OnUpdateRequest, this, [this](const Safir::Dob::EntityPtr& request, const sdt::HandlerId&, const sdt::InstanceId& instance) {
            std::cout << "OnUpdateRequest: " << Str(Safir::Dob::Typesystem::Operations::GetName(request->GetTypeId()))
                      << ", instance=" << instance.GetRawValue() << std::endl;
        });

        // Connect to OnDeleteRequest signal
        connect(m_dobHandler.get(), &DobHandler::OnDeleteRequest, this, [this](const Safir::Dob::Typesystem::EntityId& entityId, const sdt::HandlerId&) {
            std::cout << "OnDeleteRequest: " << Str(Safir::Dob::Typesystem::Operations::GetName(entityId.GetTypeId()))
                      << ", instance=" << entityId.GetInstanceId().GetRawValue() << std::endl;
        });

        // Connect to OnServiceRequest signal
        connect(m_dobHandler.get(), &DobHandler::OnServiceRequest, this, [this](const Safir::Dob::ServicePtr& request, const sdt::HandlerId&) {
            std::cout << "OnServiceRequest: " << Str(Safir::Dob::Typesystem::Operations::GetName(request->GetTypeId())) << std::endl;
        });
    }

    // Create ScriptEngine
    m_scriptEngine = std::make_unique<ScriptEngine>(m_dobHandler.get());
    m_scriptEngine->LoadScript(scriptContent);

    // Check if script is valid
    if (!m_scriptEngine->IsValid())
    {
        std::cerr << "Error: Script validation failed:" << std::endl;
        for (const auto& error : m_scriptEngine->Errors())
        {
            std::cerr << "  " << error.toUtf8().constData() << std::endl;
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
            if (m_verbose)
            {
                std::cout << "Starting script execution..." << std::endl;
            }
            m_scriptEngine->Execute();
        });

        if (!m_websocketUrl.isEmpty())
        {
            // Split websocket URL into address and port
            QStringList parts = m_websocketUrl.split(':');
            if (parts.size() != 2)
            {
                std::cerr << "Error: Invalid websocket URL format. Expected 'address:port', got: " << m_websocketUrl.toUtf8().constData() << std::endl;
                return false;
            }

            QString address = parts[0];
            bool ok;
            int port = parts[1].toInt(&ok);
            if (!ok)
            {
                std::cerr << "Error: Invalid port number in websocket URL: " << parts[1].toUtf8().constData() << std::endl;
                return false;
            }

            if (m_verbose)
            {
                std::cout << "Opening DOB websocket connection: " << m_connectionName.toUtf8().constData()
                          << " (address: " << address.toUtf8().constData() << ", port: " << port << ")" << std::endl;
            }
            m_dobHandler->OpenWebsocketConnection(address, port, m_connectionName, 0);
        }
        else
        {
            if (m_verbose)
            {
                std::cout << "Opening DOB connection: " << m_connectionName.toUtf8().constData() << std::endl;
            }
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

    if (m_currentIndex >= m_scriptEngine->Size())
    {
        if (m_verbose)
        {
            std::cout << "Script execution completed successfully." << std::endl;
        }

        // Close DOB connection before quitting
        m_dobHandler->Close();

        // Give some time for cleanup, then quit
        QTimer::singleShot(100, this, [this]() {
            m_app->quit();
        });
    }
}
