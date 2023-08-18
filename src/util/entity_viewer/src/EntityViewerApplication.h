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
#pragma once

#include <QApplication>
#include <Safir/Dob/Connection.h>

#include "EntityViewerMainWindow.h"

class EntityViewerApplication 
    : public QApplication
    , public Safir::Dob::Dispatcher
    , public Safir::Dob::StopHandler
{
    Q_OBJECT
public:

    //! Constructor.
    EntityViewerApplication(int &argc, char **argv);
    //! Destructor.
    ~EntityViewerApplication() override;

    void OnDoDispatch() override;
    void OnStopOrder() override;

    bool notify(QObject* object, QEvent* event) override;
private:
    std::unique_ptr<Safir::Dob::Connection> m_connection;
    std::unique_ptr<EntityViewerMainWindow> m_mainWindow;
};

