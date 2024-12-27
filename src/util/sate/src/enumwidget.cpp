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
#include "enumwidget.h"
#include "ui_enumwidget.h"
#include "enummodel.h"

#include <QTimer>
#include <QSortFilterProxyModel>

class EnumSortFilterProxyModel : public QSortFilterProxyModel
{
public:
    EnumSortFilterProxyModel(QWidget* parent) : QSortFilterProxyModel(parent)
    {
    }

    void setFilterRegularExpression(const int column, QRegularExpression&& regex)
    {
        m_filters[column] = std::move(regex);
        invalidateFilter();
    }

protected:
    bool filterAcceptsRow(int sourceRow, const QModelIndex& sourceParent) const override
    {
        for (int i = 0; i < 2; i++)
        {
            if (m_filters[i].isValid())
            {
                auto ix = sourceModel()->index(sourceRow, i, sourceParent);
                auto data = sourceModel()->data(ix, filterRole()).toString();

                if (!m_filters[i].match(data).hasMatch())
                {
                    return false;
                }
            }
        }

        return true;
    }

private:
    QRegularExpression m_filters[2];
};

EnumWidget::EnumWidget(int64_t typeId, const QString& currentItem, QWidget *parent)
    : QWidget(parent)
    , ui(new Ui::EnumWidget)
{
    ui->setupUi(this);

    auto srcModel = new EnumModel(typeId, this);
    auto proxyModel = new EnumSortFilterProxyModel(this);
    proxyModel->setSourceModel(srcModel);
    ui->tableView->setModel(proxyModel);
    ui->tableView->setColumnWidth(0, 75);
    ui->tableView->resizeColumnToContents(1);
    ui->tableView->verticalHeader()->setVisible(false);

    connect(ui->tableView->horizontalHeader(), &QHeaderView::sectionResized, this, &EnumWidget::OnSectionResized);
    QTimer::singleShot(1, [this]{
        OnSectionResized(0, 0, ui->tableView->columnWidth(0));
        OnSectionResized(1, 0, ui->tableView->columnWidth(1));
    });

    // Handle filter changes
    auto placeholder = QString("%1 Filter").arg(QString::fromUtf8("\xF0\x9F\x94\x8D")); // utf-8 Left-Pointing Magnifying Glass
    ui->ordinalFilter->setPlaceholderText(placeholder);
    ui->valueFilter->setPlaceholderText(placeholder);
    connect(ui->ordinalFilter, &QLineEdit::textChanged, this, [this](const QString& f) {ApplyFilter(f, 0, ui->ordinalFilter); });
    connect(ui->valueFilter, &QLineEdit::textChanged, this, [this](const QString& f) {ApplyFilter(f, 1, ui->valueFilter); });

    if (!currentItem.isEmpty())
    {
        auto ix = proxyModel->match(proxyModel->index(0, 1), Qt::DisplayRole, QVariant(currentItem), 1, Qt::MatchFlags(Qt::MatchExactly | Qt::MatchWrap));
        if (ix.size() > 0 && ix.constFirst().isValid())
        {
            ui->tableView->setCurrentIndex(ix.first());
        }
    }
}

EnumWidget::~EnumWidget()
{
    delete ui;
}

void EnumWidget::OnSectionResized(int index, int /*oldSize*/, int newSize)
{
    auto size = newSize - 2;
    switch (index)
    {
    case 0:
        ui->ordinalFilter->setFixedWidth(size);
        break;
    case 1:
        ui->valueFilter->setFixedWidth(size);
        break;
    }
}

void EnumWidget::ApplyFilter(const QString& filterText, int column, QWidget* filterWidget)
{
    auto proxyModel = static_cast<EnumSortFilterProxyModel*>(ui->tableView->model());
    if (filterText.isEmpty())
    {
        proxyModel->setFilterRegularExpression(column, QRegularExpression());
    }
    else
    {
        QRegularExpression regex(filterText, QRegularExpression::CaseInsensitiveOption);
        if (regex.isValid())
        {
            proxyModel->setFilterRegularExpression(column, std::move(regex));
        }
        else
        {
            filterWidget->setStyleSheet("background:red;");
            filterWidget->setToolTip(regex.errorString());
            return;
        }
    }

    filterWidget->setStyleSheet("");
    filterWidget->setToolTip("");
}
