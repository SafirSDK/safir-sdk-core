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
#include "dobobjecteditcontextmenuhandler.h"
#include <QPoint>
#include <QHeaderView>
#include <QMessageBox>
#include <QMenu>

DobObjectEditContextMenuHandler::DobObjectEditContextMenuHandler(QTreeView* parent)
    : m_treeView(parent)
{
    m_treeView->header()->setContextMenuPolicy(Qt::CustomContextMenu);
    connect(m_treeView->header(), &QTreeView::customContextMenuRequested, [this](const QPoint& point){

        int column = m_treeView->header()->logicalIndexAt(point);
        auto name =m_treeView->model()->headerData(column, Qt::Horizontal).toString();

        // Valid column names are: Member, Value, Null, Changed, Type,

        if (name == "Changed")
        {
            QMenu menu;

            auto* changeFlagsClear = new QAction(tr("Clear all change flags"), &menu);
            menu.addAction(changeFlagsClear);
            connect(changeFlagsClear, &QAction::triggered, this, [this]{ emit SetAllChangeFlags(false);});

            auto* changeFlagsSetAll = new QAction(tr("Set all change flags"), &menu);
            menu.addAction(changeFlagsSetAll);
            connect(changeFlagsSetAll, &QAction::triggered, this, [this]{ emit SetAllChangeFlags(true);});

            menu.exec(m_treeView->cursor().pos());
        }
    });
}
