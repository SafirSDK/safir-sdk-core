/******************************************************************************
*
* Copyright Saab AB, 2024 (http://safirsdkcore.com)
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

#include <QMainWindow>
#include <qt6advanceddocking/DockManager.h>

#include "dobinterface.h"

class QTableView;
class TypesystemWidget;

namespace Ui { class SateMainWindow; }

class SateMainWindow : public QMainWindow
{
    Q_OBJECT

public:
    SateMainWindow(QWidget *parent = nullptr);
    ~SateMainWindow();

signals:
    void ConnectedToDobSignal();
    void DispatchSignal();

private:
    Ui::SateMainWindow *ui;

    ads::CDockManager* m_dockManager;
    ads::CDockAreaWidget* m_centralDockArea;
    
    std::unique_ptr<DobInterface> m_dob;
    bool m_connected = false;

    TypesystemWidget* m_typesystem;
    QTableView* m_received;

};
