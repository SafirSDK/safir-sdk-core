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
#include "operationswidget.h"

namespace
{
    template <class T>
    T ToHashType(const QString& s)
    {
        if (s.isEmpty())
        {
            return T();
        }

        bool ok;
        int64_t num = s.toLongLong(&ok, 10);
        return ok ? T(num) : T(s.toStdWString());
    }
}

OperationsWidget::OperationsWidget(QWidget *parent)
    : QWidget(parent)
    //, ui(new Ui::OperationsWidget)
{
    //ui->setupUi(this);
    setupUi(this);
}

OperationsWidget::~OperationsWidget()
{
    //delete ui;
}

void OperationsWidget::SetConfiguration(TypesystemRepository::DobBaseClass bc)
{
    switch (bc) {
    case TypesystemRepository::Entity:
    {
        if (idEdit->text().isEmpty())
        {
            idEdit->setText("DEFAULT_HANDLER");
        }
        sendMsgBtn->setVisible(false);
        sendReqBtn->setVisible(false);
    }
    break;

    case TypesystemRepository::Message:
    {
        idLabel->setText("Channel Id");
        if (idEdit->text().isEmpty())
        {
            idEdit->setText("DEFAULT_CHANNEL");
        }
        setChangesBtn->setVisible(false);
        setAllBtn->setVisible(false);
        deleteBtn->setVisible(false);
        createReqBtn->setVisible(false);
        updateReqBtn->setVisible(false);
        deleteReqBtn->setVisible(false);
        sendReqBtn->setVisible(false);
        instanceLabel->setVisible(false);
        instanceEdit->setVisible(false);
        liveUpdateCheckBox->setVisible(false);
        copyEntityIdBtn->setVisible(false);
    }
    break;

    case TypesystemRepository::Service:
    {
        if (idEdit->text().isEmpty())
        {
            idEdit->setText("DEFAULT_HANDLER");
        }
        setChangesBtn->setVisible(false);
        setAllBtn->setVisible(false);
        deleteBtn->setVisible(false);
        createReqBtn->setVisible(false);
        updateReqBtn->setVisible(false);
        deleteReqBtn->setVisible(false);
        sendMsgBtn->setVisible(false);
        instanceLabel->setVisible(false);
        instanceEdit->setVisible(false);
        liveUpdateCheckBox->setVisible(false);
        copyEntityIdBtn->setVisible(false);
    }
    break;

    case TypesystemRepository::Response:
    {
        setChangesBtn->setVisible(false);
        setAllBtn->setVisible(false);
        deleteBtn->setVisible(false);
        createReqBtn->setVisible(false);
        updateReqBtn->setVisible(false);
        deleteReqBtn->setVisible(false);
        sendReqBtn->setVisible(false);
        sendMsgBtn->setVisible(false);
        idLabel->setVisible(false);
        idEdit->setVisible(false);
        instanceLabel->setVisible(false);
        instanceEdit->setVisible(false);
        liveUpdateCheckBox->setVisible(false);
        copyEntityIdBtn->setVisible(false);
    }
    break;

    case TypesystemRepository::Parametrization:
    case TypesystemRepository::Item:
    case TypesystemRepository::Struct:
    case TypesystemRepository::Object:
    {
        setChangesBtn->setVisible(false);
        setAllBtn->setVisible(false);
        deleteBtn->setVisible(false);
        createReqBtn->setVisible(false);
        updateReqBtn->setVisible(false);
        deleteReqBtn->setVisible(false);
        sendReqBtn->setVisible(false);
        sendMsgBtn->setVisible(false);
        idLabel->setVisible(false);
        idEdit->setVisible(false);
        instanceLabel->setVisible(false);
        instanceEdit->setVisible(false);
        liveUpdateCheckBox->setVisible(false);
        copyEntityIdBtn->setVisible(false);
    }
    break;
    }
}

Safir::Dob::Typesystem::ChannelId OperationsWidget::Channel() const
{
    return ToHashType<Safir::Dob::Typesystem::ChannelId>(idEdit->text());
}

Safir::Dob::Typesystem::HandlerId OperationsWidget::Handler() const
{
    return ToHashType<Safir::Dob::Typesystem::HandlerId>(idEdit->text());
}

Safir::Dob::Typesystem::InstanceId OperationsWidget::Instance() const
{
    if (instanceEdit->text().isEmpty())
    {
        instanceEdit->setText(QString::fromStdWString(Safir::Dob::Typesystem::InstanceId::GenerateRandom().ToString()));
    }
    return ToHashType<Safir::Dob::Typesystem::InstanceId>(instanceEdit->text());
}

