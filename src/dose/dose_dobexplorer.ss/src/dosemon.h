/******************************************************************************
*
* Copyright Saab AB, 2008 (http://www.safirsdk.com)
*
* Created by: Lars Hagstr�m / stlrha
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
#ifndef DOSEMON_H
#define DOSEMON_H
#include "common_header.h"
#include "ui_dosemon.h"
#include <set>
#include <Safir/Dob/Internal/InternalFwd.h>
#include <Safir/Dob/Internal/StateDeleter.h>
#include <QTimer>

class DoseMon : public QWidget, private Ui::DoseMonDlg
{
    Q_OBJECT

public:
    DoseMon(QWidget *parent = 0);


public slots:
    void TreeItemActivated ( QTreeWidgetItem * item, int column );

    void CloseCurrentTab();
    void UpdateTreeWidget();

private:
    void AddConnection(const Safir::Dob::Internal::Connection & connection,
                       std::set<QString>& localConnectionNames,
                       std::set<QString>& remoteConnectionNames);
    bool ActivateTab(const QString& name);
    void AddEntitesToTreeWidget();

    QTimer m_updateTimer;
};


#endif
