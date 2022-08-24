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
#include "EntityView.h"

#include <QHeaderView>
#include <iostream>

EntityView::EntityView(QAbstractItemModel* entityModel, QWidget* parent)
    : QTableView(parent)
    , m_entityModel(entityModel)
{
    setModel(entityModel);
    setSortingEnabled(true);
    setSelectionBehavior(SelectRows);
    horizontalHeader()->setStretchLastSection(true);
    resizeColumnsToContents();
}

EntityView::~EntityView()
{
}

