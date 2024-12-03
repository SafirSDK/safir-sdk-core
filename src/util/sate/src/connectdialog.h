#ifndef CONNECTDIALOG_H
#define CONNECTDIALOG_H

#include <QDialog>

namespace Ui {
class ConnectDialog;
}

class ConnectDialog : public QDialog
{
    Q_OBJECT

public:
    explicit ConnectDialog(QWidget *parent);
    ~ConnectDialog();

    bool NativeConnection() const;
    QString ConnectionName() const;
    int Context() const;
    QString WsAddress() const;
    int WsPort() const;

private:
    Ui::ConnectDialog *ui;
};

#endif // CONNECTDIALOG_H
