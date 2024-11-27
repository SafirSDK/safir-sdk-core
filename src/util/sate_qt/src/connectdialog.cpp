#include "connectdialog.h"
#include "ui_connectdialog.h"

ConnectDialog::ConnectDialog(QWidget *parent)
    : QDialog(parent)
    , ui(new Ui::ConnectDialog)
{
    ui->setupUi(this);
    ui->wsPortEdit->setValidator(new QIntValidator(0, 65535, this));
}

ConnectDialog::~ConnectDialog()
{
    delete ui;
}

bool ConnectDialog::NativeConnection() const
{
    return !ui->wsGroupBox->isChecked();
}

QString ConnectDialog::ConnectionName() const
{
    auto text = ui->connectionNameEdit->text();
    return text.isEmpty() ? "SATE" : text;
}

int ConnectDialog::Context() const
{
    return ui->contextSpinBox->value();
}

QString ConnectDialog::WsAddress() const
{
    return ui->wsAddressEdit->text();
}

int ConnectDialog::WsPort() const
{
    return ui->wsPortEdit->text().toInt();
}
