/******************************************************************************
*
* Copyright Saab AB, 2014, 2022 (http://safirsdkcore.com)
*
* Created by: Patrik Fundberg / patrik.fundberg@saabgroup.com
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
#include "EntityViewerApplication.h"

#include <iostream>
#include <thread>
#include <chrono>
#include "EntityView.h"
#include <cstdlib>

EntityViewerApplication::EntityViewerApplication(int &argc, char **argv)
    : QApplication(argc, argv)
{
    m_mainWindow = std::make_unique<EntityViewerMainWindow>();
    m_mainWindow->show();
    std::thread([this]
    {
        auto connection = std::make_unique<Safir::Dob::Connection>();
        connection->Open(L"EntityViewerApplication",
                         Safir::Dob::Typesystem::InstanceId::GenerateRandom().ToString(),
                         0,
                         this,
                         this);
        QMetaObject::invokeMethod(this, [this, connection = std::move(connection)] () mutable
                                        {
                                            m_connection.swap(connection);
                                            m_mainWindow->SetConnection(*m_connection);
                                        });
    }).detach();
}

EntityViewerApplication::~EntityViewerApplication()
{
    //If the connect is still pending we need to kill the program very forcefully, since there is no way
    //to interrupt the Open call.
    if (!m_connection)
    {
        std::quick_exit(0);
    }
}

void EntityViewerApplication::OnDoDispatch()
{
    QMetaObject::invokeMethod(this, [this]{m_connection->Dispatch();});
}

void EntityViewerApplication::OnStopOrder()
{
    QMetaObject::invokeMethod(this, []{quit();});
}
