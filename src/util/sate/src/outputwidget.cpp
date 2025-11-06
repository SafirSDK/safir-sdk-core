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
#include "outputwidget.h"
#include "ui_outputwidget.h"
#include "dobhandler.h"

#include <QDateTime>
#include <QTimer>
#include <QScrollBar>
#include <QUrl>
#include <QDebug>

namespace
{
QString Str(int64_t typeId) {return QString::fromStdWString(sdt::Operations::GetName(typeId)); }
QString Str(const std::wstring& s) { return QString::fromStdWString(s); }
QString Timestamp() { return QString("<i style='color:grey'>%1</i>: ").arg(QDateTime::currentDateTime().toString("hh:mm:ss")); }
}

OutputWidget::OutputWidget(DobHandler* dob, QWidget *parent)
    : QWidget(parent)
    , ui(new Ui::OutputWidget)
{
    ui->setupUi(this);

    ui->output->setReadOnly(true);
    ui->output->setUndoRedoEnabled(false);
    ui->output->setAcceptRichText(false);
    ui->output->setOpenLinks(false);
    ui->jsonToolButton->setVisible(false);

    connect(ui->output, &QTextBrowser::anchorClicked, this, [this](const QUrl& url)
            {
                auto data = FindLink(url.toString());
                if (data != nullptr)
                {
                    emit OpenObjectEdit(data->channelHandler, data->instance, data->object);
                }
            });

    QStringList head;

    connect(dob, &DobHandler::ConnectedToDob, this, [this, dob](const QString&){ui->jsonToolButton->setVisible(!dob->IsNativeConnection());});
    connect(dob, &DobHandler::Output, this, &OutputWidget::Output);
    connect(dob, &DobHandler::OnEntity, this, &OutputWidget::OnEntity);
    connect(dob, &DobHandler::OnMessage, this, &OutputWidget::OnMessage);
    connect(dob, &DobHandler::OnResponse, this, &OutputWidget::OnResponse);
    connect(dob, &DobHandler::OnCreateRequest, this, &OutputWidget::OnCreateRequest);
    connect(dob, &DobHandler::OnUpdateRequest, this, &OutputWidget::OnUpdateRequest);
    connect(dob, &DobHandler::OnDeleteRequest, this, &OutputWidget::OnDeleteRequest);
    connect(dob, &DobHandler::OnServiceRequest, this, &OutputWidget::OnServiceRequest);
    connect(dob, &DobHandler::OnReadEntity, this, &OutputWidget::OnReadEntity);

    // ToolTip handling
    connect(ui->infoToolButton, &QToolButton::clicked, [this]{SetToolTip(ui->infoToolButton);});
    connect(ui->errorToolButton, &QToolButton::clicked, [this]{SetToolTip(ui->errorToolButton);});
    connect(ui->jsonToolButton, &QToolButton::clicked, [this]{SetToolTip(ui->jsonToolButton);});
    connect(ui->entitiesTtoolButton, &QToolButton::clicked, [this]{SetToolTip(ui->entitiesTtoolButton);});
    connect(ui->messagesToolButton, &QToolButton::clicked, [this]{SetToolTip(ui->messagesToolButton);});
    connect(ui->requestToolButton, &QToolButton::clicked, [this]{SetToolTip(ui->requestToolButton);});
    connect(ui->responseToolButton, &QToolButton::clicked, [this]{SetToolTip(ui->responseToolButton);});
    SetToolTip(ui->infoToolButton);
    SetToolTip(ui->errorToolButton);
    SetToolTip(ui->jsonToolButton);
    SetToolTip(ui->entitiesTtoolButton);
    SetToolTip(ui->messagesToolButton);
    SetToolTip(ui->requestToolButton);
    SetToolTip(ui->responseToolButton);

    connect(ui->clearToolButton, &QToolButton::clicked, [this] { ui->output->clear(); });
}

OutputWidget::~OutputWidget()
{
    delete ui;
}

void OutputWidget::Output(const QString& info, const QtMsgType msgType)
{
    if (!ui->infoToolButton->isChecked())
    {
        return;
    }

    auto wasEmpty = m_pendingOutput.empty();

    switch (msgType)
    {
    case QtDebugMsg:
    {
        if (ui->jsonToolButton->isChecked())
        {
            QStringList line;
            line << Timestamp() << info;
            m_pendingOutput << line.join("");
        }
    }
    break;
    case QtInfoMsg:
    {
        if (ui->infoToolButton->isChecked())
        {
            QStringList line;
            line << Timestamp() << info;
            m_pendingOutput << line.join("");
        }
    }
    break;
    case QtWarningMsg:
    {
        if (ui->errorToolButton->isChecked())
        {
            QStringList line;
            line << Timestamp() << QString("<b>%1</b>").arg(info);
            m_pendingOutput << line.join("");
        }
    }
    break;
    case QtCriticalMsg:
    case QtFatalMsg:
    {
        if (ui->errorToolButton->isChecked())
        {
            QStringList line;
            line << Timestamp() << QString("<span style='color:red'>%1</span>").arg(info);
            m_pendingOutput << line.join("");
        }
    }
    break;
    }

    if (wasEmpty && !m_pendingOutput.empty())
    {
        StartTimer();
    }
}

void OutputWidget::OnMessage(const sdt::ChannelId& channel, const Safir::Dob::MessagePtr& message)
{
    if (!ui->messagesToolButton->isChecked())
    {
        return;
    }
    const bool startTimer = m_pendingOutput.empty();

    auto link = AddLink(message, Str(channel.ToString()), -1);
    QStringList line;
    line << Timestamp()
         << "&nbsp;<img src=':/img/icons/message_orange' width='12' height='12'/>&nbsp;"
         << QString("OnMessage <a href='%1' style='color:#78c4fc'>%2</a> on channel %3").arg(link, Str(message->GetTypeId()), Str(channel.ToString()));

    m_pendingOutput << line.join("");

    if (startTimer)
    {
        StartTimer();
    }
}

void OutputWidget::OnEntity(const sdt::EntityId& entityId, const sdt::HandlerId& handler, const Safir::Dob::EntityPtr& entity, DobInterface::EntityOperation operation)
{
    if (!ui->entitiesTtoolButton->isChecked())
    {
        return;
    }
    const bool startTimer = m_pendingOutput.empty();

    QStringList line;
    line << Timestamp() << "&nbsp;<img src=':/img/icons/entity_orange' width='12' height='12'/>&nbsp;";

    switch (operation) {
    case DobInterface::NewEntity:
    {
        auto link = AddLink(entity, Str(handler.ToString()), entityId.GetInstanceId().GetRawValue());
        line << QString("OnNewEntity <a href='%1' style='color:#78c4fc'>%2</a> from handler %3").arg(link, Str(entityId.ToString()), Str(handler.ToString()));
    }
    break;
    case DobInterface::UpdatedEntity:
    {
        auto link = AddLink(entity, Str(handler.ToString()), entityId.GetInstanceId().GetRawValue());
        line << QString("OnUpdatedEntity <a href='%1' style='color:#78c4fc'>%2</a> from handler %3").arg(link, Str(entityId.ToString()), Str(handler.ToString()));
    }
    break;

    case DobInterface::DeletedEntity:
    {
        line << QString("OnDeletedEntity %1 from handler %2").arg(Str(entityId.ToString()), Str(handler.ToString()));
    }
    break;
    }

    m_pendingOutput << line.join("");

    if (startTimer)
    {
        StartTimer();
    }
}

void OutputWidget::OnResponse(const Safir::Dob::ResponsePtr& response)
{
    if (!ui->responseToolButton->isChecked())
    {
        return;
    }
    const bool startTimer = m_pendingOutput.empty();

    auto link = AddLink(response, "", -1);
    QStringList line;
    line << Timestamp()
         << "&nbsp;<img src=':/img/icons/response_orange' width='12' height='12'/>&nbsp;"
         << QString("OnResponse <a href='%1' style='color:#78c4fc'>%2</a>").arg(link, Str(response->GetTypeId()));

    m_pendingOutput << line.join("");

    if (startTimer)
    {
        StartTimer();
    }
}

void OutputWidget::OnCreateRequest(const Safir::Dob::EntityPtr& request, const sdt::HandlerId& handler, const sdt::InstanceId& instance)
{
    if (!ui->requestToolButton->isChecked())
    {
        return;
    }
    const bool startTimer = m_pendingOutput.empty();

    auto link = AddLink(request, Str(handler.ToString()), instance.GetRawValue());
    QStringList line;
    if (instance == sdt::InstanceId())
    {
        line << Timestamp()
             << "&nbsp;<img src=':/img/icons/gear_orange' width='12' height='12'/>&nbsp;"
             << QString("OnCreateEntityRequest <a href='%1' style='color:#78c4fc'>%2</a>, handler: %3").arg(link, Str(request->GetTypeId()), Str(handler.ToString()));
    }
    else
    {
        line << Timestamp()
             << "&nbsp;<img src=':/img/icons/gear_orange' width='12' height='12'/>&nbsp;"
             << QString("OnCreateEntityRequest <a href='%1' style='color:#78c4fc'>%2</a>, inst: %3 handler: %4").arg(link, Str(request->GetTypeId()), Str(instance.ToString()), Str(handler.ToString()));
    }

    m_pendingOutput << line.join("");

    if (startTimer)
    {
        StartTimer();
    }
}

void OutputWidget::OnUpdateRequest(const Safir::Dob::EntityPtr& request, const sdt::HandlerId& handler, const sdt::InstanceId& instance)
{
    if (!ui->requestToolButton->isChecked())
    {
        return;
    }
    const bool startTimer = m_pendingOutput.empty();

    auto link = AddLink(request, Str(handler.ToString()), instance.GetRawValue());
    QStringList line;
    line << Timestamp()
         << "&nbsp;<img src=':/img/icons/gear_orange' width='12' height='12'/>&nbsp;"
         << QString("OnUpdateEntityRequest <a href='%1' style='color:#78c4fc'>%2</a>, handler: %3").arg(link, Str(request->GetTypeId()), Str(handler.ToString()));

    m_pendingOutput << line.join("");

    if (startTimer)
    {
        StartTimer();
    }
}

void OutputWidget::OnDeleteRequest(const Safir::Dob::Typesystem::EntityId& entityId, const sdt::HandlerId& handler)
{
    if (!ui->requestToolButton->isChecked())
    {
        return;
    }
    const bool startTimer = m_pendingOutput.empty();

    QStringList line;
    line << Timestamp()
         << "&nbsp;<img src=':/img/icons/gear_orange' width='12' height='12'/>&nbsp;"
         << QString("OnDeleteEntityRequest %1, handler: %2").arg(Str(entityId.ToString()), Str(handler.ToString()));

    m_pendingOutput << line.join("");

    if (startTimer)
    {
        StartTimer();
    }
}
void OutputWidget::OnServiceRequest(const Safir::Dob::ServicePtr& request, const sdt::HandlerId& handler)
{
    if (!ui->requestToolButton->isChecked())
    {
        return;
    }
    const bool startTimer = m_pendingOutput.empty();

    auto link = AddLink(request, Str(handler.ToString()), -1);
    QStringList line;
    line << Timestamp()
         << "&nbsp;<img src=':/img/icons/gear_orange' width='12' height='12'/>&nbsp;"
         << QString("OnServiceRequest <a href='%1' style='color:#78c4fc'>%2</a>, handler: %3").arg(link, Str(request->GetTypeId()), Str(handler.ToString()));

    m_pendingOutput << line.join("");

    if (startTimer)
    {
        StartTimer();
    }
}

void OutputWidget::OnReadEntity(const Safir::Dob::EntityPtr& entity, const sdt::InstanceId& instance)
{
    if (!ui->entitiesTtoolButton->isChecked())
    {
        return;
    }
    const bool startTimer = m_pendingOutput.empty();

    QStringList line;
    line << Timestamp() << "&nbsp;<img src=':/img/icons/entity_orange' width='12' height='12'/>&nbsp;";

    sdt::EntityId entityId(entity->GetTypeId(), instance);
    auto link = AddLink(entity, QString(), instance.GetRawValue());
    line << QString("Read entity result <a href='%1' style='color:#78c4fc'>%2</a>").arg(link, Str(entityId.ToString()));

    m_pendingOutput << line.join("");

    if (startTimer)
    {
        StartTimer();
    }
}

void OutputWidget::StartTimer()
{
    QTimer::singleShot(300, [this] //vs2015 cannot understand std::chorono::milliseconds
                       {
                           ui->output->setUpdatesEnabled(false);
                           ui->output->append(m_pendingOutput.join("<br>"));
                           ui->output->setUpdatesEnabled(true);
                           m_pendingOutput.clear();
                           auto *sb = ui->output->verticalScrollBar();
                           sb->setValue(sb->maximum());
                       });
}

QString OutputWidget::AddLink(const Safir::Dob::Typesystem::ObjectPtr& obj, const QString& ch, int64_t inst)
{
    m_objects.emplace_back(RecvData{m_counter++, obj, ch, inst});
    while (m_objects.size() > m_maxObjects)
    {
        m_objects.pop_front();
    }

    return QString::number(m_counter-1);
}

const OutputWidget::RecvData* OutputWidget::FindLink(const QString& link) const
{
    bool ok;
    auto counter = link.toULongLong(&ok);

    if (!ok)
    {
        return nullptr;
    }

    for (const auto& r : m_objects)
    {
        if (r.counter == counter)
        {
            return &r;
        }
    }

    return nullptr;
}

void OutputWidget::SetToolTip(QToolButton* btn)
{
    if (btn == ui->infoToolButton)
    {
        if (!btn->isChecked())
        {
            btn->setToolTip("<p>Informational logging is <b>disabled</b>.</p><p><i>Informational logging is any output that is not an error and doesn't have any object attached.</i></p>");
        }
        else
        {
            btn->setToolTip("<p>Informational logging is <b>enabled</b>.</p><p><i>Informational logging is any output that is not an error and doesn't have any object attached.</i></p>");
        }
    }
    else if (btn == ui->errorToolButton)
    {
        if (!btn->isChecked())
        {
            btn->setToolTip("<p>Error logging is <b>disabled</b>.</p><p><i>Error logging is output from exceptions or any other errors that occurs.</i></p>");
        }
        else
        {
            btn->setToolTip("<p>Error logging is <b>enabled</b>.</p><p><i>Error logging is output from exceptions or any other errors that occurs.</i></p>");
        }
    }
    else if (btn == ui->jsonToolButton)
    {
        if (!btn->isChecked())
        {
            btn->setToolTip("<p>Websocket logging is <b>disabled</b>.</p><p><i>Websocket logging will output all messages sent and received on the socket.</i></p>");
        }
        else
        {
            btn->setToolTip("<p>Websocket logging is <b>enabled</b>.</p><p><i>Websocket logging will output all messages sent and received on the socket.</i></p>");
        }
    }
    else if (btn == ui->entitiesTtoolButton)
    {
        if (!btn->isChecked())
        {
            btn->setToolTip("<p>Entity logging is <b>disabled</b>.</p><p><i>Logging when the Dob event OnNewEntity, OnUpdatedEntity, OnDeletedEntity etc occurs. The log has an attached Entity.</i></p>");
        }
        else
        {
            btn->setToolTip("<p>Entity logging is <b>enabled</b>.</p><p><i>Logging when the Dob event OnNewEntity, OnUpdatedEntity, OnDeletedEntity etc occurs. The log has an attached Entity.</i></p>");
        }
    }
    else if (btn == ui->messagesToolButton)
    {
        if (!btn->isChecked())
        {
            btn->setToolTip("<p>Message logging is <b>disabled</b>.</p><p><i>Logging when Dob event OnMessage occurs. The log has an attached Message.</i></p>");
        }
        else
        {
            btn->setToolTip("<p>Message logging is <b>enabled</b>.</p><p><i>Logging when Dob event OnMessage occurs. The log has an attached Message.</i></p>");
        }
    }
    else if (btn == ui->requestToolButton)
    {
        if (!btn->isChecked())
        {
            btn->setToolTip("<p>Request logging is <b>disabled</b>.</p><p><i>Logging when a request is received (create, update, delete or service). The log has the request attached.</i></p>");
        }
        else
        {
            btn->setToolTip("<p>Request logging is <b>enabled</b>.</p><p><i>Logging when a request is received (create, update, delete or service). The log has the request attached.</i></p>");
        }
    }
    else if (btn == ui->responseToolButton)
    {
        if (!btn->isChecked())
        {
            btn->setToolTip("<p>Response logging is <b>disabled</b>.</p><p><i>Logging when a response is received. The log has the response attached.</i></p>");
        }
        else
        {
            btn->setToolTip("<p>Response logging is <b>enabled</b>.</p><p><i>Logging when a response is received. The log has the response attached.</i></p>");
        }
    }

}
