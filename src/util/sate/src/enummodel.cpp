#include "enummodel.h"
#include "qcolor.h"


EnumModel::EnumModel(int64_t typeId, QObject *parent)
    : QAbstractTableModel(parent)
{
    m_enum = TypesystemRepository::Instance().GetEnum(typeId);
}

QVariant EnumModel::headerData(int section, Qt::Orientation orientation, int role) const
{
    if (role == Qt::DisplayRole && orientation == Qt::Horizontal)
    {
        if (section == 0)
        {
            return "Ordinal";
        }
        if (section == 1)
        {
            return "Enum Value";
        }
    }

    return {};
}

int EnumModel::rowCount(const QModelIndex &parent) const
{
    if (parent.isValid())
        return 0;

    return static_cast<int>(m_enum->values.size());
}

int EnumModel::columnCount(const QModelIndex &parent) const
{
    if (parent.isValid())
        return 0;

    return 2;
}

QVariant EnumModel::data(const QModelIndex &index, int role) const
{
    if (!index.isValid())
        return QVariant();

    switch (role)
    {
    case Qt::DisplayRole:
    {
        if (index.column() == 0)
        {
            return index.row();
        }
        if (index.column() == 1)
        {
            return m_enum->values.at(static_cast<size_t>(index.row()));
        }
    }
    break;

    case Qt::TextAlignmentRole:
    {
        if (index.column() == 0)
        {
            return int(Qt::AlignRight | Qt::AlignVCenter);
        }
        if (index.column() == 1)
        {
            return Qt::AlignCenter;
        }
    }
    break;
    }

    return QVariant();
}
