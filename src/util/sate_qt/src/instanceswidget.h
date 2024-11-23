/******************************************************************************
*
* Copyright Saab AB, 2014, 2022, 2024 (http://safirsdkcore.com)
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

#include "dobhandler.h"
#include <QWidget>

class QTableView;
class QWidget;
class QLineEdit;
class QHBoxLayout;
class EntityInstancesModel;
class MessageInstancesModel;
class ColumnSortFilterProxyModel;

class InstancesWidget
    : public QWidget
{
    Q_OBJECT

public:
    //create an entity instances widget
    InstancesWidget(DobHandler* dob,
                    int64_t typeId,
                    bool includeSubclasses,
                    QWidget* parent);

    //create a message instances widget
    InstancesWidget(DobHandler* dob, int64_t typeId,
                    const Safir::Dob::Typesystem::ChannelId& channel,
                    bool includeSubclasses,
                    QWidget* parent);

    ~InstancesWidget() override;

signals:
    void OpenObjectEdit(int64_t typeId,
                        QString channelHandler,
                        int64_t instance,
                        const Safir::Dob::Typesystem::ObjectPtr& object);

private slots:
    void OnDoubleClicked(const QModelIndex &index);
    void OnFilterTextChanged(const int column, const QString &text);
    void OnSectionResized(const int logicalIndex, const int oldSize, const int newSize);
    void OnSectionCountChanged(const int oldCount, const int newCount);
    void OnCustomContextMenuRequestedHeader(const QPoint& pos);
    void OnCustomContextMenuRequestedTable(const QPoint& pos);
    void RunColumnContextMenu(const QPoint& globalPos, const int logicalIndex);
private:
    //common constructor
    InstancesWidget(QWidget* parent);

    QTableView* m_table;
    QWidget* m_filterArea;
    QHBoxLayout* m_filterAreaLayout;
    QList<QLineEdit*> m_filters;
    ColumnSortFilterProxyModel* m_proxyModel = nullptr;
    EntityInstancesModel* m_sourceModelEntities = nullptr;
    MessageInstancesModel* m_sourceModelMessages = nullptr;
};

