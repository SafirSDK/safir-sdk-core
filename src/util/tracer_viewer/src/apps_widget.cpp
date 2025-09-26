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
#include "apps_widget.h"
#include "ui_apps_widget.h"
#include "tracer_status_model.h"
#include <QHeaderView>
#include <QAbstractItemView>
#include <QDebug>
#include <QTimer>
#include "edit_button_delegate.h"
#include "enabled_checkbox_delegate.h"
#include <QSortFilterProxyModel>
#include <QSignalBlocker>

namespace {
/* Proxy that refuses to sort the two right-most columns
 * (StateColumn / EditColumn) but otherwise behaves like the
 * standard QSortFilterProxyModel.
 */
class AppSortFilterProxyModel : public QSortFilterProxyModel
{
public:
    using QSortFilterProxyModel::QSortFilterProxyModel;

protected:
    void sort(int column,
              Qt::SortOrder order = Qt::AscendingOrder) override
    {
        if (column == TracerStatusModel::StateColumn ||
            column == TracerStatusModel::EditColumn)
        {
            // Ignore sort requests for the last two columns
            return;
        }
        QSortFilterProxyModel::sort(column, order);
    }
};

} // namespace

AppsWidget::AppsWidget(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::AppsWidget)
{
    ui->setupUi(this);
    ui->filterLineEdit->setPlaceholderText("Filter");
    ui->appsView->verticalHeader()->setVisible(false);
    ui->appsView->setSelectionBehavior(QAbstractItemView::SelectRows);
    ui->appsView->setSelectionMode(QAbstractItemView::SingleSelection);
    ui->appsView->setShowGrid(false);
    ui->appsView->setMouseTracking(true);   // needed so delegate receives hover state

    ui->appsView->setSelectionMode(QAbstractItemView::NoSelection);
}

AppsWidget::~AppsWidget()
{
    delete ui;
}

void AppsWidget::Initialize(Safir::Dob::Connection& dobConnection)
{
    auto* model = new TracerStatusModel(this, dobConnection);

    auto* proxyModel = new AppSortFilterProxyModel(this);
    proxyModel->setSourceModel(model);
    proxyModel->setDynamicSortFilter(true);
    proxyModel->setFilterCaseSensitivity(Qt::CaseInsensitive);
    proxyModel->setFilterRole(Qt::DisplayRole);
    proxyModel->setFilterKeyColumn(TracerStatusModel::ProgramNameColumn);

    ui->appsView->setModel(proxyModel);
    ui->appsView->setSortingEnabled(true);

    // Clear sort indicator for the Enabled / Edit columns whenever they
    // receive a sort indicator (covers both user clicks and programmatic calls)
    auto* header = ui->appsView->horizontalHeader();
    connect(header, &QHeaderView::sortIndicatorChanged,
            this, [header](int section, Qt::SortOrder) {
                if (section == TracerStatusModel::StateColumn ||
                    section == TracerStatusModel::EditColumn) {
                    QSignalBlocker blocker(header);           // avoid recursion
                    header->setSortIndicator(-1, Qt::AscendingOrder);
                }
            });

    // show/hide the error message label on registration change
    connect(model, &TracerStatusModel::registrationStatusChanged,
            this, [this](const bool registered){ui->errorLabel->setHidden(registered);});

    // forward edit button click signal
    connect(model, &TracerStatusModel::editRequested,
            this, &AppsWidget::EditTracerClicked);

    // Live filter on the Program column
    connect(ui->filterLineEdit, &QLineEdit::textChanged,
            proxyModel, &QSortFilterProxyModel::setFilterFixedString);

    // Put a real push-button delegate in the Edit column
    ui->appsView->setItemDelegateForColumn(
        TracerStatusModel::EditColumn,
        new EditButtonDelegate(ui->appsView));

    // Styled checkbox delegate for the "On" column
    ui->appsView->setItemDelegateForColumn(
        TracerStatusModel::StateColumn,
        new EnabledCheckBoxDelegate(ui->appsView));

    // Align header text to the left for all columns
    ui->appsView->horizontalHeader()->setDefaultAlignment(Qt::AlignLeft | Qt::AlignVCenter);

    // Do not stretch the last section – we want explicit control of the two right-most columns.
    ui->appsView->horizontalHeader()->setStretchLastSection(false);

    // Make the *second* column (index 1) absorb any extra space when the view is resized.
    ui->appsView->horizontalHeader()->setSectionResizeMode(1, QHeaderView::Stretch);

    // -----------------------------------------------------------------
    // Node column: set a minimum width equal to its header so it cannot
    // be shrunk to an unreadable size.
    // -----------------------------------------------------------------
    const int nodeHeaderWidth =
        ui->appsView->horizontalHeader()->sectionSizeHint(TracerStatusModel::NodeNameColumn);
    ui->appsView->horizontalHeader()->resizeSection(TracerStatusModel::NodeNameColumn, nodeHeaderWidth);
    // Keep the global minimum section size unchanged so the Enabled/Edit columns
    // remain at their intended fixed widths.

    // First column: interactive; auto-resize once data is available.
    ui->appsView->horizontalHeader()->setSectionResizeMode(0, QHeaderView::Interactive);

    // Schedule an initial resize after the event loop runs
    QTimer::singleShot(0, this, [this]() { ui->appsView->resizeColumnToContents(0); });

    // Keep the first column width updated when the model changes
    connect(proxyModel, &QAbstractItemModel::modelReset, this,
            [this]() { ui->appsView->resizeColumnToContents(0); });
    connect(proxyModel, &QAbstractItemModel::rowsInserted, this,
            [this](const QModelIndex&, int, int) { ui->appsView->resizeColumnToContents(0); });

    // Keep the "On" column at a fixed width.
    ui->appsView->horizontalHeader()->setSectionResizeMode(
        TracerStatusModel::StateColumn, QHeaderView::Fixed);
    ui->appsView->setColumnWidth(TracerStatusModel::StateColumn, 32);

    // Keep the "Edit" column sized to its contents.
    ui->appsView->horizontalHeader()->setSectionResizeMode(
        TracerStatusModel::EditColumn, QHeaderView::ResizeToContents);
}


void AppsWidget::SetSearchFocus()
{
    ui->filterLineEdit->setFocus();
}

