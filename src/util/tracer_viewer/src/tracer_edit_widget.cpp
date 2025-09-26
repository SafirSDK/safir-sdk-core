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
#include "tracer_edit_widget.h"
#include "ui_tracer_edit_widget.h"
#include <Safir/Application/TracerStatus.h>
#include <QHeaderView>
#include <QAbstractItemView>
#include <QTimer>
#include <QDialogButtonBox>
#include <QSignalBlocker>
#include <QPushButton>
#include <QCloseEvent>
#include <QLineEdit>
#include <QTableWidgetItem>
#include <Safir/Dob/ResponseProxy.h>
#include "common.h"
#include <unordered_map>


TracerEditWidget::TracerEditWidget(Safir::Dob::Connection& dobConnection,
                                   const std::int64_t instanceId,
                                   QWidget *parent)
    : QWidget(parent)
    , ui(new Ui::TracerEditWidget)
    , m_dobConnection(dobConnection)
    , m_instanceId(instanceId)
{
    ui->setupUi(this);

    // Immediate update signals
    connect(ui->logToStdout,      &QCheckBox::toggled,    this, &TracerEditWidget::sendUpdate);
    connect(ui->logToSafirLogging,&QCheckBox::toggled,    this, &TracerEditWidget::sendUpdate);
    connect(ui->logToTracer,      &QCheckBox::toggled,    this, &TracerEditWidget::sendUpdate);
    connect(ui->prefixesTable, &QTableWidget::itemChanged, this,
            [this](QTableWidgetItem* item)
            {
                if (item->column() == TracerEditWidget::EnabledColumn)
                    sendUpdate();
            });

    m_dobConnection.SubscribeEntity(Safir::Dob::Typesystem::EntityId(Safir::Application::TracerStatus::ClassTypeId,
                                                                     Safir::Dob::Typesystem::InstanceId(m_instanceId)),
                                    true,
                                    true,
                                    this);

    /*---------------------------------------------------------------------
     * Disallow sorting on the second (checkbox) column by automatically
     * redirecting any attempt to sort column 1 back to column 0 while
     * preserving the requested order.
     *--------------------------------------------------------------------*/
    connect(ui->prefixesTable->horizontalHeader(),
            &QHeaderView::sortIndicatorChanged,
            this,
            [this](int logicalIndex, Qt::SortOrder order)
            {
                if (logicalIndex == TracerEditWidget::EnabledColumn) // checkbox column
                {
                    ui->prefixesTable->horizontalHeader()->setSortIndicator(TracerEditWidget::PrefixColumn, order);
                    ui->prefixesTable->sortByColumn(TracerEditWidget::PrefixColumn, order);
                }
            });

    ui->prefixesTable->setSelectionMode(QAbstractItemView::NoSelection);
}

/*------------------------------------------------------------------------------
 * Refresh widgets from given TracerStatus
 *----------------------------------------------------------------------------*/
void TracerEditWidget::Refresh(const Safir::Application::TracerStatusConstPtr& status)
{
    // Prevent feedback-loops while we update the controls
    QSignalBlocker blockerStdout(ui->logToStdout);
    QSignalBlocker blockerSafir(ui->logToSafirLogging);
    QSignalBlocker blockerTracer(ui->logToTracer);
    QSignalBlocker blockerTable(ui->prefixesTable);
    /* Target label --------------------------------------------------------*/
    const QString target = QString::fromStdWString(status->ProgramName())
                           + " @ " +
                           QString::fromStdWString(status->NodeName());
    ui->target->setText(target);
    ui->target->setToolTip(target);

    /* Log-flags -----------------------------------------------------------*/
    ui->logToStdout->setChecked(status->LogToStdout());
    ui->logToSafirLogging->setChecked(status->LogToSafirLogging());
    ui->logToTracer->setChecked(status->LogToTracer());

    /* Prefix table --------------------------------------------------------*/
    ui->prefixesTable->clearContents();
    ui->prefixesTable->setRowCount(static_cast<int>(status->Prefixes().size()));
    ui->prefixesTable->setColumnCount(2);
    ui->prefixesTable->setHorizontalHeaderLabels({tr("Prefix"), tr("On")});

    QHeaderView* hh = ui->prefixesTable->horizontalHeader();
    hh->setSectionResizeMode(TracerEditWidget::PrefixColumn,  QHeaderView::Stretch);
    hh->setSectionResizeMode(TracerEditWidget::EnabledColumn, QHeaderView::Fixed);
    ui->prefixesTable->setColumnWidth(TracerEditWidget::EnabledColumn, 32);
    ui->prefixesTable->verticalHeader()->setVisible(false);
    ui->prefixesTable->setShowGrid(false);
    ui->prefixesTable->setSortingEnabled(false);   // will enable after population

    int row = 0;
    for (const auto& kv : status->Prefixes())
    {
        const QString key = QString::fromStdWString(kv.first);
        const bool enabled = kv.second.GetVal();

        auto* keyItem = new QTableWidgetItem(key);
        keyItem->setFlags(keyItem->flags() & ~Qt::ItemIsEditable);
        ui->prefixesTable->setItem(row, TracerEditWidget::PrefixColumn, keyItem);

        auto* valueItem = new QTableWidgetItem();
        valueItem->setFlags(Qt::ItemIsUserCheckable | Qt::ItemIsEnabled);
        valueItem->setCheckState(enabled ? Qt::Checked : Qt::Unchecked);
        ui->prefixesTable->setItem(row, TracerEditWidget::EnabledColumn, valueItem);

        ++row;
    }
    ui->prefixesTable->setSortingEnabled(true);    // allow user sorting
    ui->prefixesTable->sortByColumn(TracerEditWidget::PrefixColumn, Qt::AscendingOrder); // default ascending
}

void TracerEditWidget::OnResponse(const Safir::Dob::ResponseProxy responseProxy)
{
    if (!responseProxy.IsSuccess())
    {
        ShowStatusBarMessage(tr("Failed to set tracer, got an error response."));
    }
}

void TracerEditWidget::OnNotRequestOverflow()
{
    ShowStatusBarMessage(tr("Got an overflow, try again."));
}

TracerEditWidget::~TracerEditWidget()
{
    if (m_dobConnection.IsOpen())
    {
        m_dobConnection.UnsubscribeEntity(Safir::Application::TracerStatus::ClassTypeId, this);
    }
    delete ui;
}

void TracerEditWidget::sendUpdate()
{
    /* ---------- Build entity id and fetch fresh TracerStatus -------------*/
    const Safir::Dob::Typesystem::EntityId entityId(
        Safir::Application::TracerStatus::ClassTypeId,
        Safir::Dob::Typesystem::InstanceId(m_instanceId));

    Safir::Application::TracerStatusPtr request;
    try
    {
        request = std::static_pointer_cast<Safir::Application::TracerStatus>(
            m_dobConnection.Read(entityId).GetEntity());
    }
    catch (const std::exception& ex)
    {
        ShowStatusBarMessage(tr("Failed to read tracer entity: %1").arg(ex.what()));
        return;
    }

    /* ---------- copy log-flags from the dialog -------------------------- */
    request->LogToStdout().SetVal(ui->logToStdout->isChecked());
    request->LogToSafirLogging().SetVal(ui->logToSafirLogging->isChecked());
    request->LogToTracer().SetVal(ui->logToTracer->isChecked());

    /* ---------- get prefix states from the table -------------------- */
    std::unordered_map<std::wstring, bool> uiPrefixes;
    for (int row = 0; row < ui->prefixesTable->rowCount(); ++row)
    {
        const auto prefix  = ui->prefixesTable->item(row, TracerEditWidget::PrefixColumn)->text().toStdWString();
        const auto enabled = ui->prefixesTable->item(row, TracerEditWidget::EnabledColumn)->checkState() == Qt::Checked;

        const auto findIt = request->Prefixes().find(prefix);
        if (findIt != request->Prefixes().end())
        {
            findIt->second.SetVal(enabled);
        }
        else
        {
            ShowStatusBarMessage(tr("Prefix list mismatch – refresh the dialog and try again."));
            return;
        }
    }

    /* ---------- send update request ------------------------------------- */
    m_dobConnection.UpdateRequest(request,
                                  Safir::Dob::Typesystem::InstanceId(m_instanceId),
                                  this);
}


/*------------------------------------------------------------------------------
 * Safir::Dob::EntitySubscriber callbacks
 *----------------------------------------------------------------------------*/
void TracerEditWidget::OnNewEntity(const Safir::Dob::EntityProxy entityProxy)
{
    Refresh(std::static_pointer_cast<const Safir::Application::TracerStatus>(entityProxy.GetEntity()));
}

void TracerEditWidget::OnUpdatedEntity(const Safir::Dob::EntityProxy entityProxy)
{
    Refresh(std::static_pointer_cast<const Safir::Application::TracerStatus>(entityProxy.GetEntity()));
}

void TracerEditWidget::OnDeletedEntity(const Safir::Dob::EntityProxy /*entityproxy*/, const bool /*deprecated*/)
{
    deleteLater();
}
