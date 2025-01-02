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

#include <QWidget>
#include <QStringList>
#include <deque>
#include "dobhandler.h"

namespace Ui {
class OutputWidget;
}

class QToolButton;

class OutputWidget : public QWidget
{
    Q_OBJECT

public:
    explicit OutputWidget(DobHandler* dobHandler, QWidget *parent);
    ~OutputWidget();

    void Output(const QString& info, const QtMsgType msgType);

signals:
    void OpenObjectEdit(QString channelHandler,
                        int64_t instance,
                        const Safir::Dob::Typesystem::ObjectPtr& object);
private:
    Ui::OutputWidget *ui;
    QStringList m_pendingOutput;

    struct RecvData
    {
        uint64_t counter;
        Safir::Dob::Typesystem::ObjectPtr object;
        QString channelHandler;
        int64_t instance;
    };

    uint64_t m_counter = 0;
    size_t m_maxObjects = 25;
    std::deque<RecvData> m_objects;

    void OnMessage(const sdt::ChannelId& channel, const Safir::Dob::MessagePtr& message);
    void OnEntity(const sdt::EntityId& entityId, const sdt::HandlerId& handler, const Safir::Dob::EntityPtr& entity, DobInterface::EntityOperation operation);
    void OnResponse(const Safir::Dob::ResponsePtr& response);
    void OnCreateRequest(const Safir::Dob::EntityPtr& request, const sdt::HandlerId& handler, const sdt::InstanceId& instance);
    void OnUpdateRequest(const Safir::Dob::EntityPtr& request, const sdt::HandlerId& handler, const sdt::InstanceId& instance);
    void OnDeleteRequest(const Safir::Dob::Typesystem::EntityId& entityId, const sdt::HandlerId& handler);
    void OnServiceRequest(const Safir::Dob::ServicePtr& request, const sdt::HandlerId& handler);
    void OnReadEntity(const Safir::Dob::EntityPtr& entity, const sdt::InstanceId& instance);

    void StartTimer();
    QString AddLink(const Safir::Dob::Typesystem::ObjectPtr& obj, const QString& ch, int64_t inst);
    const RecvData* FindLink(const QString& link) const;

    void SetToolTip(QToolButton* btn);
};
