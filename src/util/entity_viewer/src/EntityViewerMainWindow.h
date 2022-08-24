/******************************************************************************
*
* Copyright Saab AB, 2022 (http://safirsdkcore.com)
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

#include <Safir/Dob/Connection.h>

#include "ui_EntityViewerMainWindow.h"

class EntityViewerMainWindow
    : public QMainWindow
{
    Q_OBJECT
public:
    EntityViewerMainWindow();
    ~EntityViewerMainWindow() override;

    void SetConnection(Safir::Dob::Connection& connection);
private slots:
    void CloseTab(int index);
    void TreeItemActivated(QTreeWidgetItem* item);
    void FilterChanged(const QString& filter);
    void Second64SettingsChanged();
private:
    void PopulateTree();

    Ui::EntityViewerMainWindow m_ui;
    Safir::Dob::Connection* m_connection = nullptr;

};



