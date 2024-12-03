#ifndef SUBSCRIBEDIALOG_H
#define SUBSCRIBEDIALOG_H

#include <QDialog>
#include <Safir/Dob/Typesystem/ChannelId.h>
#include <Safir/Dob/Typesystem/HandlerId.h>
#include <Safir/Dob/Typesystem/InstanceId.h>

namespace Ui {
class SubscribeDialog;
}

class SubscribeDialog : public QDialog
{
    Q_OBJECT

public:
    explicit SubscribeDialog(QWidget *parent);
    ~SubscribeDialog();

    void Show(int64_t typeId, bool registrationSub = false);

    int64_t TypeId() const;
    bool RegistrationsSubscription() const;
    bool IncludeSubclasses() const;
    Safir::Dob::Typesystem::ChannelId Channel() const;
    Safir::Dob::Typesystem::HandlerId Handler() const;
    Safir::Dob::Typesystem::InstanceId Instance() const;

private:
    Ui::SubscribeDialog *ui;
    int64_t m_typeId;
    bool m_registrationSub;
};

#endif // SUBSCRIBEDIALOG_H
