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
#pragma once

#include <QAbstractTableModel>
#include <QDateTime>
#include <QTimer>

#include <Safir/Dob/Typesystem/Defs.h>
#include <Safir/Dob/Consumer.h>
#include <Safir/Dob/Connection.h>

#include "columninfo.h"
#include "instancesmodelutils.h"
#include "dobhandler.h"

class MessageInstancesModel
    : public QAbstractTableModel
    , private InstancesModelUtils
{
    Q_OBJECT
public:
    MessageInstancesModel(DobHandler* dob,
                          const Safir::Dob::Typesystem::TypeId typeId,
                          const Safir::Dob::Typesystem::ChannelId& channel,
                          bool includeSubclasses,
                          QObject* parent);

    ~MessageInstancesModel() override;

    struct Info
    {
        QDateTime receiveTime;
        int64_t typeId;
        Safir::Dob::Typesystem::ChannelId channelId;
        Safir::Dob::MessagePtr message;
        std::chrono::steady_clock::time_point greenUntil; //green until now is after this
    };

    Info getRow(int row) const;
    QStringList statusBarInfo() const;
signals:
    void statusBarInfoChanged();
private:
    int rowCount(const QModelIndex& parent) const override;
    int columnCount(const QModelIndex& parent) const override;
    QVariant headerData(int section, Qt::Orientation orientation, int role) const override;
    QVariant data(const QModelIndex& index, int role) const override;

    void setupColumns();

private slots:
    void OnMessage(const sdt::ChannelId& channel,
                   const Safir::Dob::MessagePtr& message);

    void OnTimeout();
private:
    DobHandler* const m_dob;
    const Safir::Dob::Typesystem::TypeId m_typeId;
    const Safir::Dob::Typesystem::ChannelId m_channel;
    const bool m_includeSubclasses;
    ColumnInfoList m_columnInfoList;

    size_t m_maxRows = 1000;
    size_t m_numReceived = 0;
    std::deque<Info> m_messages;
    QTimer m_timer;
};
