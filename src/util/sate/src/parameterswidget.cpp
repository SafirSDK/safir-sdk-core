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
#include "parameterswidget.h"
#include "ui_parameterswidget.h"
#include "parametersmodel.h"

#include <QSortFilterProxyModel>
#include <QTimer>
#include <QDebug>

class ParametersSortFilterProxyModel : public QSortFilterProxyModel
{
public:
    ParametersSortFilterProxyModel(QWidget* parent) : QSortFilterProxyModel(parent)
    {
        setRecursiveFilteringEnabled(true); // show parent nodes when a child node is matching a search filter
    }

    void setFilterRegularExpression(const int column, QRegularExpression&& regex)
    {
        m_filters[column] = std::move(regex);
        invalidateFilter();
    }

protected:
    bool filterAcceptsRow(int sourceRow, const QModelIndex& sourceParent) const override
    {
        for (int i = 0; i < 3; i++)
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
    QRegularExpression m_filters[3];
};

ParametersWidget::ParametersWidget(int64_t typeId, const QString& currentItem, QWidget *parent)
    : QWidget(parent)
    , ui(new Ui::ParametersWidget)
{
    ui->setupUi(this);

    auto srcModel = new ParametersModel(typeId, this);
    auto proxyModel = new ParametersSortFilterProxyModel(this);
    proxyModel->setSourceModel(srcModel);
    proxyModel->setFilterRole(ParametersModel::FilterRole);
    ui->parametersTreeView->setModel(proxyModel);

    if (!currentItem.isEmpty())
    {
        auto ix = proxyModel->match(proxyModel->index(0, 0), Qt::DisplayRole, currentItem, 1, Qt::MatchFlags(Qt::MatchExactly | Qt::MatchWrap));
        if (ix.size() > 0 && ix.constFirst().isValid())
        {
            ui->parametersTreeView->setCurrentIndex(ix.first());
        }
    }

    ui->parametersTreeView->resizeColumnToContents(0);
    ui->parametersTreeView->resizeColumnToContents(1);
    ui->parametersTreeView->resizeColumnToContents(2);
    if (ui->parametersTreeView->columnWidth(0) < 100)
    {
        ui->parametersTreeView->setColumnWidth(0, 100); // Min start width 100 to make it look better
    }
    if (ui->parametersTreeView->columnWidth(1) < 100)
    {
        ui->parametersTreeView->setColumnWidth(1, 100); // Min start width 100 to make it look better
    }

    connect(ui->parametersTreeView->header(), &QHeaderView::sectionResized, this, &ParametersWidget::OnSectionResized);
    QTimer::singleShot(1, [this]{
        OnSectionResized(0, 0, ui->parametersTreeView->columnWidth(0));
        OnSectionResized(1, 0, ui->parametersTreeView->columnWidth(1));
        OnSectionResized(2, 0, ui->parametersTreeView->columnWidth(2));
    });

    // Handle filter changes
    auto placeholder = QString("%1 Filter").arg(QString::fromUtf8("\xF0\x9F\x94\x8D")); // utf-8 Left-Pointing Magnifying Glass
    ui->nameFilterEdit->setPlaceholderText(placeholder);
    ui->valueFilterEdit->setPlaceholderText(placeholder);
    ui->typeFilterEdit->setPlaceholderText(placeholder);
    connect(ui->nameFilterEdit, &QLineEdit::textChanged, this, [this](const QString& f) {ApplyFilter(f, 0, ui->nameFilterEdit); });
    connect(ui->valueFilterEdit, &QLineEdit::textChanged, this, [this](const QString& f) {ApplyFilter(f, 1, ui->valueFilterEdit); });
    connect(ui->typeFilterEdit, &QLineEdit::textChanged, this, [this](const QString& f) {ApplyFilter(f, 2, ui->typeFilterEdit); });
}

ParametersWidget::~ParametersWidget()
{
    delete ui;
}

void ParametersWidget::OnSectionResized(int index, int /*oldSize*/, int newSize)
{
    auto size = newSize - 2;
    switch (index)
    {
    case 0:
        ui->nameFilterEdit->setFixedWidth(size);
        break;
    case 1:
        ui->valueFilterEdit->setFixedWidth(size);
        break;
    case 2:
        ui->typeFilterEdit->setFixedWidth(size);
        break;
    }
}

void ParametersWidget::ApplyFilter(const QString& filterText, int column, QWidget* filterWidget)
{
    auto proxyModel = static_cast<ParametersSortFilterProxyModel*>(ui->parametersTreeView->model());
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
            ui->parametersTreeView->expandAll();
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
