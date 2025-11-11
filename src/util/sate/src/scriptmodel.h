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
#pragma once

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning (disable: 4127)
#pragma warning (disable: 4244)
#endif

#include <QAbstractTableModel>
#include <QVector>

#ifdef _MSC_VER
#pragma warning(pop)
#endif

#include "scriptengine.h"

class ScriptModel : public QAbstractTableModel
{
    Q_OBJECT

public:
    struct ScriptRow
    {
        QString method;
        QString details;
        bool run;
    };

    explicit ScriptModel(const QVector<ScriptRow>& rows, QObject *parent);
    ~ScriptModel();

    enum Column
    {
        RowNumber = 0,
        Method = 1,
        Details = 2,
        Run = 3,
        ColumnCount = 4
    };

    // QAbstractItemModel interface
    int rowCount(const QModelIndex &parent = QModelIndex()) const override;
    int columnCount(const QModelIndex &parent = QModelIndex()) const override;
    QVariant data(const QModelIndex &index, int role = Qt::DisplayRole) const override;
    QVariant headerData(int section, Qt::Orientation orientation, int role = Qt::DisplayRole) const override;

    void setRowRun(int row, bool run);
    void deleteRow(int row);
    void addRow(const ScriptRow& row);

private:
    QVector<ScriptRow> m_rows;
};
