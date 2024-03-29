/******************************************************************************
*
* Copyright Saab AB, 2023 (http://safirsdkcore.com)
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
#include "registrations.h"
#include "ui_registrations.h"

#include <wchar.h>
#include <sstream>
#include <Safir/Dob/Internal/Connections.h>

namespace
{
    // Create read-only table item
    QTableWidgetItem* TableItem(const QString& text, bool alignCenter = false)
    {
        auto item = new QTableWidgetItem(text);
        item->setFlags(item->flags() &  ~Qt::ItemIsEditable);
        if (alignCenter)
        {
            item->setTextAlignment(Qt::AlignCenter);
        }
        return item;
    }

    QString Handler(const std::wstring& name, int64_t val)
    {
        if (name.size() > 0)
        {
            return QString::fromWCharArray(name.c_str());
        }
        return QString::number(val);
    }

    template<typename RegT>
    std::shared_ptr<Registrations::RegData> ToRegData(const Safir::Dob::Internal::ConnectionPtr& connectionPtr, const RegT& regInfo, const Safir::Dob::Typesystem::Internal::DistributionScopeReader& distributionScopeReader)
    {
        auto r = std::make_shared<Registrations::RegData>();
        r->connectionName = connectionPtr->NameWithCounter();
        r->typeId = regInfo.typeId;
        r->typeName = QString::fromWCharArray(Safir::Dob::Typesystem::Operations::GetName(regInfo.typeId).c_str());
        r->handler = Handler(regInfo.handlerId.GetRawString(), regInfo.handlerId.GetRawValue());
        r->context = connectionPtr->Id().m_contextId;
        r->pending = false;
        r->scope = distributionScopeReader.GetDistributionScope(regInfo.typeId);
        r->detached = connectionPtr->IsDetached();

        std::ostringstream os;
        os << r->connectionName.toStdString() << " " << r->typeName.toStdString() << " " << r->typeId << " "
                   << r->handler.toStdString() << (r->pending ? " pending" : "") << (r->detached ? " detached" : "");
        r->content = os.str().c_str();
        return r;
    }
}


Registrations::Registrations(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::registrations),
    m_timer(this),
    m_distributionScopeReader()
{
    ui->setupUi(this);

    ui->tableWidget->resizeRowsToContents();
    ui->tableWidget->resizeColumnsToContents();
    ui->tableWidget->setAlternatingRowColors(true);

    connect(ui->filterEdit,
            SIGNAL(textChanged(const QString &)),
            this,
            SLOT(FilterChanged(const QString&)));

    Update();

    ui->tableWidget->resizeColumnsToContents();

    connect(&m_timer, SIGNAL(timeout()), this, SLOT(Update()));
    m_timer.start(3000);
}

Registrations::~Registrations()
{
    delete ui;
}

void Registrations::FilterChanged(const QString&)
{
    ApplyFilter();
    UpdateGui();
}

void Registrations::Update()
{
    UpdateRegistartionData();
    ApplyFilter();
    UpdateGui();
}

void Registrations::ApplyFilter()
{
    m_regdataFiltered.clear();
    auto filterText = ui->filterEdit->text().trimmed();
    if (filterText.isEmpty())
    {
        m_regdataFiltered.insert(begin(m_regdataFiltered), begin(m_regdata), end(m_regdata));
        return;
    }

    for (const auto& ptr : m_regdata)
    {
        if (ptr->content.contains(filterText, Qt::CaseInsensitive))
        {
            m_regdataFiltered.push_back(ptr);
        }
    }
}

void Registrations::UpdateGui()
{
    ui->tableWidget->setSortingEnabled(false);
    ui->tableWidget->setRowCount(static_cast<int>(m_regdataFiltered.size()));

    auto hasDetached = std::find_if(begin(m_regdata), end(m_regdata), [](const auto& r){return r->detached;}) != end(m_regdata);
    if (hasDetached)
    {
        ui->tableWidget->showColumn(6);
    }
    else
    {
        ui->tableWidget->hideColumn(6);
    }

    int row = 0;
    for (const auto& r : m_regdataFiltered)
    {
        QString scope = r->scope != Safir::Dob::DistributionScope::Global ? QString::fromWCharArray(Safir::Dob::DistributionScope::ToString(r->scope).c_str()) : "";
        ui->tableWidget->setItem(row, 0, TableItem(r->typeName));
        ui->tableWidget->setItem(row, 1, TableItem(r->handler));
        ui->tableWidget->setItem(row, 2, TableItem(r->connectionName));
        ui->tableWidget->setItem(row, 3, TableItem(QString::number(r->context), true));
        ui->tableWidget->setItem(row, 4, TableItem(r->pending ? QString::fromStdWString(L"\u2713") : "", true)); // 2713 is the utf-8 code for 'Check Mark'
        ui->tableWidget->setItem(row, 5, TableItem(scope, true));
        ui->tableWidget->setItem(row, 6, TableItem(r->detached ? QString::fromStdWString(L"\u2713") : "", true));
        ++row;
    }
    ui->tableWidget->setSortingEnabled(true);
}

void Registrations::UpdateRegistartionData()
{
    m_regdata.clear();
    Safir::Dob::Internal::Connections::Instance().ForEachConnectionPtr([this](const auto& connectionPtr)
    {
        // Active registrations
        for (const auto& regInfo : connectionPtr->GetRegisteredHandlers())
        {
            auto r = ToRegData(connectionPtr, regInfo, m_distributionScopeReader);
            m_regdata.push_back(r);
        }

        // Pending registrations
        for (const auto& pendingReg : connectionPtr->GetPendingRegistrations())
        {
            if (!pendingReg.accepted && !pendingReg.remove)
            {
                auto r = ToRegData(connectionPtr, pendingReg, m_distributionScopeReader);
                r->pending = true;
                m_regdata.push_back(r);
            }
        }
    });
}
