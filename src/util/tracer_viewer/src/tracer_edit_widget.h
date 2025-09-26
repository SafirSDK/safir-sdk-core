/******************************************************************************
*
* Copyright Saab AB, 2025 (http://safirsdkcore.com)
*
* Created by: Lars Hagström / lars@foldspace.nu
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
#include <Safir/Dob/Connection.h>
#include <Safir/Dob/Consumer.h>
#include <QCloseEvent>
#include <Safir/Application/TracerStatus.h>
#include <cstdint>

namespace Ui {
class TracerEditWidget;
}

class TypesystemFilterProxyModel;
class QSortFilterProxyModel;

class TracerEditWidget
    : public QWidget
    , public Safir::Dob::Requestor
    , public Safir::Dob::EntitySubscriber
{
    Q_OBJECT

public:
    explicit TracerEditWidget(Safir::Dob::Connection& dobConnection,
                              const std::int64_t instanceId,
                              QWidget *parent);
    ~TracerEditWidget() override;

    // Safir::Dob::Requestor interface
    void OnResponse(const Safir::Dob::ResponseProxy responseProxy) override;
    void OnNotRequestOverflow() override;

    // Safir::Dob::EntitySubscriber interface
    void OnNewEntity(const Safir::Dob::EntityProxy entityProxy) override;
    void OnUpdatedEntity(const Safir::Dob::EntityProxy entityProxy) override;
    void OnDeletedEntity(const Safir::Dob::EntityProxy entityproxy, const bool deprecated) override;

private slots:
    void sendUpdate();

private:
    Ui::TracerEditWidget *ui;
    Safir::Dob::Connection& m_dobConnection;
    const std::int64_t m_instanceId;

    enum PrefixColumns
    {
        PrefixColumn   = 0,   // text
        EnabledColumn  = 1    // checkbox
    };

    /* Refresh all widget controls from a TracerStatus entity */
    void Refresh(const Safir::Application::TracerStatusConstPtr& status);
};
