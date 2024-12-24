#ifndef ENUMMODEL_H
#define ENUMMODEL_H

#include <QAbstractTableModel>
#include "typesystemrepository.h"

class EnumModel : public QAbstractTableModel
{
    Q_OBJECT

public:
    explicit EnumModel(int64_t typeId, QObject *parent);

    // Header:
    QVariant headerData(int section,
                        Qt::Orientation orientation,
                        int role = Qt::DisplayRole) const override;

    // Basic functionality:
    int rowCount(const QModelIndex &parent = QModelIndex()) const override;
    int columnCount(const QModelIndex &parent = QModelIndex()) const override;

    QVariant data(const QModelIndex &index, int role = Qt::DisplayRole) const override;

private:
    const TypesystemRepository::DobEnum* m_enum;

};

#endif // ENUMMODEL_H
