#ifndef OUTPUTWIDGET_H
#define OUTPUTWIDGET_H

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

    void StartTimer();
    QString AddLink(const Safir::Dob::Typesystem::ObjectPtr& obj, const QString& ch, int64_t inst);
    const RecvData* FindLink(const QString& link) const;

    void SetToolTip(QToolButton* btn);
};

#endif // OUTPUTWIDGET_H
