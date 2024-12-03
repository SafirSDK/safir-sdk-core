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

#include "ui_operationswidget.h"
#include <QWidget>
#include "typesystemrepository.h"
#include <Safir/Dob/Typesystem/ChannelId.h>
#include <Safir/Dob/Typesystem/HandlerId.h>
#include <Safir/Dob/Typesystem/InstanceId.h>

class OperationsWidget : public QWidget, public Ui::OperationsWidget
{
    Q_OBJECT

public:
    explicit OperationsWidget(QWidget *parent);
    ~OperationsWidget();

    void SetConfiguration(TypesystemRepository::DobBaseClass bc);

    Safir::Dob::Typesystem::ChannelId Channel() const;
    Safir::Dob::Typesystem::HandlerId Handler() const;
    Safir::Dob::Typesystem::InstanceId Instance() const;

private:
    //Ui::OperationsWidget *ui;
};
