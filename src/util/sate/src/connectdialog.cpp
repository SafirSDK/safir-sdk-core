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
#include "connectdialog.h"
#include "ui_connectdialog.h"

ConnectDialog::ConnectDialog(QWidget *parent)
    : QDialog(parent)
    , ui(new Ui::ConnectDialog)
{
    ui->setupUi(this);
    setModal(true);
    setFixedSize(width(), height());
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
