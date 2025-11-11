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
#include "scriptmodel.h"

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning (disable: 4127)
#pragma warning (disable: 4244)
#endif

#include <QColor>

#ifdef _MSC_VER
#pragma warning(pop)
#endif

ScriptModel::ScriptModel(const QVector<ScriptRow>& rows, QObject *parent)
    : QAbstractTableModel(parent)
    , m_rows(rows)
{
}

ScriptModel::~ScriptModel()
{
}

int ScriptModel::rowCount(const QModelIndex &parent) const
{
    if (parent.isValid())
        return 0;
    return static_cast<int>(m_rows.size());
}

int ScriptModel::columnCount(const QModelIndex &parent) const
{
    if (parent.isValid())
        return 0;
    return ColumnCount;
}

QVariant ScriptModel::data(const QModelIndex &index, int role) const
{
    if (!index.isValid() || index.row() >= m_rows.size())
        return QVariant();

    const ScriptRow &row = m_rows[index.row()];

    switch (role)
    {
        case Qt::DisplayRole:
        case Qt::EditRole:
        {
            switch (index.column())
            {
            case RowNumber:
                return index.row() + 1;
            case Method:
                return row.method;
            case Details:
                return row.details;
            case Run:
                return row.run ? QString::fromUtf8("\xE2\x9C\x93") : "";
            }
        }
        break;

        case Qt::ForegroundRole:
        {
            if (index.column() == Method)
            {
                return QColor(250, 185, 0); // Orange color for method names
            }
        }
        break;

        default:
            break;
    }

    return QVariant();
}

QVariant ScriptModel::headerData(int section, Qt::Orientation orientation, int role) const
{
    if (role != Qt::DisplayRole)
        return QVariant();

    if (orientation == Qt::Vertical)
        return QVariant();

    switch (section) {
    case RowNumber:
        return QString();
    case Method:
        return tr("Method");
    case Details:
        return tr("Details");
    case Run:
        return tr("Run");
    default:
        return QVariant();
    }
}

void ScriptModel::setRowRun(int row, bool run)
{
    if (row >= 0 && row < m_rows.size())
    {
        m_rows[row].run = run;
        QModelIndex idx = index(row, Run);
        emit dataChanged(idx, idx, {Qt::DisplayRole});
    }
}

void ScriptModel::deleteRow(int row)
{
    if (row >= 0 && row < m_rows.size())
    {
        beginRemoveRows(QModelIndex(), row, row);
        m_rows.removeAt(row);
        endRemoveRows();
    }
}

void ScriptModel::addRow(const ScriptRow& row)
{
    int newRow = static_cast<int>(m_rows.size());
    beginInsertRows(QModelIndex(), newRow, newRow);
    m_rows.append(row);
    endInsertRows();
}
