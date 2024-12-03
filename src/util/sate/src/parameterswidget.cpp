#include "parameterswidget.h"
#include "ui_parameterswidget.h"
#include "parametersmodel.h"

#include <QSortFilterProxyModel>
#include <QTimer>

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
        for (int i = 0; i < 5; i++)
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
    QRegularExpression m_filters[5];
};

ParametersWidget::ParametersWidget(int64_t typeId, QWidget *parent)
    : QWidget(parent)
    , ui(new Ui::ParametersWidget)
{
    ui->setupUi(this);

    auto srcModel = new ParametersModel(typeId, this);
    auto proxyModel = new ParametersSortFilterProxyModel(this);
    proxyModel->setSourceModel(srcModel);
    ui->parametersTreeView->setModel(proxyModel);

    ui->parametersTreeView->resizeColumnToContents(0);
    ui->parametersTreeView->resizeColumnToContents(1);
    ui->parametersTreeView->resizeColumnToContents(2);

    connect(ui->parametersTreeView->header(), &QHeaderView::sectionResized, this, &ParametersWidget::OnSectionResized);
    QTimer::singleShot(1, [this]{
        OnSectionResized(0, 0, ui->parametersTreeView->columnWidth(0));
        OnSectionResized(1, 0, ui->parametersTreeView->columnWidth(1));
        OnSectionResized(2, 0, ui->parametersTreeView->columnWidth(2));
    });

    // Handle filter changes
    connect(ui->nameFilterEdit, &QLineEdit::textChanged, this, [this](const QString& f) {ApplyFilter(f, 0, ui->nameFilterEdit); });
    connect(ui->valueFilterEdit, &QLineEdit::textChanged, this, [this](const QString& f) {ApplyFilter(f, 1, ui->valueFilterEdit); });
    connect(ui->typeFilterEdit, &QLineEdit::textChanged, this, [this](const QString& f) {ApplyFilter(f, 4, ui->typeFilterEdit); });
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
