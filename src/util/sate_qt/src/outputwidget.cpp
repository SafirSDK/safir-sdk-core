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
    connect(ui->output, &QTextBrowser::anchorClicked, this, [this](const QUrl& url)
    {
        auto data = FindLink(url.toString());
        if (data != nullptr)
        {
            emit OpenObjectEdit(data->object->GetTypeId(), data->channelHandler, data->instance, data->object);
        }
    });

    QStringList head;

    connect(dob, &DobHandler::ConnectedToDob, this, [this, dob](const QString&){ui->jsonToolButton->setVisible(!dob->IsNativeConnection());});
    connect(dob, &DobHandler::Info, this, &OutputWidget::OnInfo);
    connect(dob, &DobHandler::OnEntity, this, &OutputWidget::OnEntity);
    connect(dob, &DobHandler::OnMessage, this, &OutputWidget::OnMessage);
    connect(dob, &DobHandler::OnResponse, this, &OutputWidget::OnResponse);
    connect(dob, &DobHandler::OnCreateRequest, this, &OutputWidget::OnCreateRequest);
    connect(dob, &DobHandler::OnUpdateRequest, this, &OutputWidget::OnUpdateRequest);
    connect(dob, &DobHandler::OnDeleteRequest, this, &OutputWidget::OnDeleteRequest);
    connect(dob, &DobHandler::OnServiceRequest, this, &OutputWidget::OnServiceRequest);
}

OutputWidget::~OutputWidget()
{
    delete ui;
}

void OutputWidget::OnInfo(const QString& info, const QtMsgType msgType)
{
    if (ui->infoToolButton->isChecked())
    {
        return;
    }
    const bool startTimer = m_pendingOutput.empty();

    QStringList line;
    line += Timestamp();
    switch (msgType)
    {
    case QtDebugMsg:
    case QtInfoMsg:
        line +=info;
        break;
    case QtWarningMsg:
        line+= QString("<b>%1</b>").arg(info);
        break;
    case QtCriticalMsg:
    case QtFatalMsg:
        line += QString("<span style='color:red'>%1</span>").arg(info);
        break;
    }

    line += "<br>";
    m_pendingOutput += line.join("");

    if (startTimer)
    {
        StartTimer();
    }
}

void OutputWidget::OnMessage(const sdt::ChannelId& channel, const Safir::Dob::MessagePtr& message)
{
    if (ui->messagesToolButton->isChecked())
    {
        return;
    }
    const bool startTimer = m_pendingOutput.empty();

    auto link = AddLink(message, Str(channel.ToString()), 0);
    QStringList line;
    line += Timestamp();
    line += "&nbsp;<img src=':/img/icons/message_orange' width='12' height='12'/>&nbsp;";
    line += QString("<a href='%1' style='color:#78c4fc'>OnMessage %2 on channel %3</a><br>").arg(link, Str(message->GetTypeId()), Str(channel.ToString()));

    m_pendingOutput += line.join("");

    if (startTimer)
    {
        StartTimer();
    }
}

void OutputWidget::OnEntity(const sdt::EntityId& entityId, const sdt::HandlerId& handler, const Safir::Dob::EntityPtr& entity, DobInterface::EntityOperation operation)
{
    if (ui->entitiesTtoolButton->isChecked())
    {
        return;
    }
    const bool startTimer = m_pendingOutput.empty();

    QStringList line;
    line += Timestamp();
    line += "&nbsp;<img src=':/img/icons/entity_orange' width='12' height='12'/>&nbsp;";

    switch (operation) {
    case DobInterface::NewEntity:
    {
        auto link = AddLink(entity, Str(handler.ToString()), entityId.GetInstanceId().GetRawValue());
        line += QString("<a href='%1' style='color:#78c4fc'>OnNewEntity %2 from handler %3</a><br>").arg(link, Str(entityId.ToString()), Str(handler.ToString()));
    }
        break;
    case DobInterface::UpdatedEntity:
    {
        auto link = AddLink(entity, Str(handler.ToString()), entityId.GetInstanceId().GetRawValue());
        line += QString("<a href='%1' style='color:#78c4fc'>OnUpdatedEntity %2 from handler %3</a><br>").arg(link, Str(entityId.ToString()), Str(handler.ToString()));
    }
        break;

    case DobInterface::DeletedEntity:
    {
        line += QString("OnDeletedEntity %1 from handler %2<br>").arg(Str(entityId.ToString()), Str(handler.ToString()));
    }
        break;
    }

    m_pendingOutput += line.join("");

    if (startTimer)
    {
        StartTimer();
    }
}

void OutputWidget::OnResponse(const Safir::Dob::ResponsePtr& response)
{
    if (ui->responseToolButton->isChecked())
    {
        return;
    }
    const bool startTimer = m_pendingOutput.empty();

    QStringList line;
    line += Timestamp();
    line += "&nbsp;<img src=':/img/icons/response_orange' width='12' height='12'/>&nbsp;";

    auto link = AddLink(response, "", -1);
    line += QString("<a href='%1' style='color:#78c4fc'>OnResponse %2</a><br>").arg(link, Str(response->GetTypeId()));

    m_pendingOutput += line.join("");

    if (startTimer)
    {
        StartTimer();
    }
}

void OutputWidget::OnCreateRequest(const Safir::Dob::EntityPtr& request, const sdt::HandlerId& handler, const sdt::InstanceId& instance)
{
    if (ui->requestToolButton->isChecked())
    {
        return;
    }
    const bool startTimer = m_pendingOutput.empty();

    QStringList line;
    line += Timestamp();
    line += "&nbsp;<img src=':/img/icons/gear_orange' width='12' height='12'/>&nbsp;";

    auto link = AddLink(request, Str(handler.ToString()), instance.GetRawValue());
    line += QString("<a href='%1' style='color:#78c4fc'>OnCreateEntityRequest %2, handler: %3</a><br>").arg(link, Str(request->GetTypeId()), Str(handler.ToString()));

    m_pendingOutput += line.join("");

    if (startTimer)
    {
        StartTimer();
    }
}

void OutputWidget::OnUpdateRequest(const Safir::Dob::EntityPtr& request, const sdt::HandlerId& handler, const sdt::InstanceId& instance)
{
    if (ui->requestToolButton->isChecked())
    {
        return;
    }
    const bool startTimer = m_pendingOutput.empty();

    QStringList line;
    line += Timestamp();
    line += "&nbsp;<img src=':/img/icons/gear_orange' width='12' height='12'/>&nbsp;";

    auto link = AddLink(request, Str(handler.ToString()), instance.GetRawValue());
    line += QString("<a href='%1' style='color:#78c4fc'>OnUpdateEntityRequest %2, handler: %3</a><br>").arg(link, Str(request->GetTypeId()), Str(handler.ToString()));

    m_pendingOutput += line.join("");

    if (startTimer)
    {
        StartTimer();
    }
}

void OutputWidget::OnDeleteRequest(const Safir::Dob::Typesystem::EntityId& entityId, const sdt::HandlerId& handler)
{
    if (ui->requestToolButton->isChecked())
    {
        return;
    }
    const bool startTimer = m_pendingOutput.empty();

    QStringList line;
    line += Timestamp();
    line += "&nbsp;<img src=':/img/icons/gear_orange' width='12' height='12'/>&nbsp;";
    line += QString("OnDeleteEntityRequest %1, handler: %2<br>").arg(Str(entityId.ToString()), Str(handler.ToString()));

    m_pendingOutput += line.join("");

    if (startTimer)
    {
        StartTimer();
    }
}
void OutputWidget::OnServiceRequest(const Safir::Dob::ServicePtr& request, const sdt::HandlerId& handler)
{
    if (ui->requestToolButton->isChecked())
    {
        return;
    }
    const bool startTimer = m_pendingOutput.empty();

    QStringList line;
    line += Timestamp();
    line += "&nbsp;<img src=':/img/icons/gear_orange' width='12' height='12'/>&nbsp;";

    auto link = AddLink(request, Str(handler.ToString()), -1);
    line += QString("<a href='%1' style='color:#78c4fc'>OnServiceRequest %2, handler: %3</a><br>").arg(link, Str(request->GetTypeId()), Str(handler.ToString()));

    m_pendingOutput += line.join("");

    if (startTimer)
    {
        StartTimer();
    }
}

void OutputWidget::StartTimer()
{
    QTimer::singleShot(300, [this] //milliseconds
       {
           ui->output->setUpdatesEnabled(false);
           ui->output->insertHtml(m_pendingOutput.join(""));
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
