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
