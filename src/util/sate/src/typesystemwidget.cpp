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
#include "typesystemwidget.h"
#include "ui_typesystemwidget.h"
#include "typesystemrepository.h"
#include "typesysteminheritancemodel.h"
#include "typesystemnamespacemodel.h"
#include "iconfactory.h"
#include <QLineEdit>
#include <QDebug>
#include <QTreeView>
#include <QModelIndex>
#include <QRadioButton>
#include <QMenu>
#include <QStandardItemModel>

#include <Safir/Dob/Typesystem/Parameters.h>
#include <Safir/Dob/Parametrization.h>

// -----------------------------------------------------
// TypesystemFilterProxyModel
// -----------------------------------------------------
class TypesystemFilterProxyModel : public QSortFilterProxyModel
{
public:
    TypesystemFilterProxyModel(QObject* parent) : QSortFilterProxyModel(parent)
    {
        setRecursiveFilteringEnabled(true); // show parent nodes when a child node is matching a search filter
        setFilterCaseSensitivity(Qt::CaseInsensitive);
    }

    void SetFilter(const QString& filter)
    {
        m_filter = filter;
        invalidateFilter();
    }

protected:
    bool filterAcceptsRow(int sourceRow, const QModelIndex& sourceParent) const override
    {
        if (m_filter.isEmpty())
        {
            return true;
        }
        auto ix = sourceModel()->index(sourceRow, 0, sourceParent);
        if (ix.isValid())
        {
            QString val = ix.data().toString();
            return val.contains(m_filter, Qt::CaseInsensitive);
        }
        return false;
    }

    QVariant data(const QModelIndex &index, int role = Qt::DisplayRole) const override
    {
        if (role == Qt::BackgroundRole && !m_filter.isEmpty() && index.data().toString().contains(m_filter, Qt::CaseInsensitive))
        {
            return QColor(128,128,128); //Gray. The QColorConstants don't seem to work in vs2015...
        }

        return QSortFilterProxyModel::data(index, role);
    }

private:
    QString m_filter;
};

// -----------------------------------------------------
// TypesystemWidget
// -----------------------------------------------------
TypesystemWidget::TypesystemWidget(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::TypesystemWidget)
{
    ui->setupUi(this);
    ui->treeView->header()->hide();
}

TypesystemWidget::~TypesystemWidget()
{
    delete ui;
}

void TypesystemWidget::Initialize(DobHandler* dob)
{
    m_dob = dob;
    m_inheritanceProxyModel = new TypesystemFilterProxyModel(this);
    m_inheritanceProxyModel->setSourceModel(new TypesystemInheritanceModel(m_dob, this));
    m_namespaceProxyModel = new TypesystemFilterProxyModel(this);
    m_namespaceProxyModel->setSourceModel(new TypesystemNamespaceModel(m_dob, this));

    SetupFilterEdit();
    SetTreeViewModel(true);

    connect(ui->treeView, &QTreeView::doubleClicked, this, [this](const QModelIndex& ix)
    {
        if (!ix.isValid())
        {
            return;
        }

        auto typeIdVal = ix.data(TypesystemRepository::DobTypeIdRole);
        auto baseClassVal = ix.data(TypesystemRepository::DobBaseClassRole);
        if (typeIdVal.isValid())
        {
            if (baseClassVal.isValid())
            {
                const auto baseClass = static_cast<TypesystemRepository::DobBaseClass>(baseClassVal.toInt()); 
                const bool ctrlKey = qApp->keyboardModifiers().testFlag(Qt::ControlModifier);
                const bool shiftKey = qApp->keyboardModifiers().testFlag(Qt::ShiftModifier);

                if (baseClass == TypesystemRepository::Parametrization)
                {
                    emit OpenParameterViewer(typeIdVal.toLongLong(), QString());
                }
                else if (ctrlKey && !shiftKey)
                {
                    emit OpenObjectEdit(typeIdVal.toLongLong());
                }
                else if (shiftKey && !ctrlKey)
                {
                    if (baseClass == TypesystemRepository::Entity)
                    {
                        emit OpenEntityInstanceViewer(typeIdVal.toLongLong(), true);
                        m_dob->SubscribeEntity(typeIdVal.toLongLong(), sdt::InstanceId(), true);
                    }
                    else if (baseClass == TypesystemRepository::Message)
                    {
                        emit OpenMessageInstanceViewer(typeIdVal.toLongLong(), Safir::Dob::Typesystem::ChannelId::ALL_CHANNELS, true);
                        m_dob->SubscribeMessage(typeIdVal.toLongLong(), Safir::Dob::Typesystem::ChannelId::ALL_CHANNELS, true);
                    }
                }
                else if (!shiftKey && !ctrlKey)
                {
                    if (baseClass == TypesystemRepository::Entity)
                    {
                        emit OpenEntityInstanceViewer(typeIdVal.toLongLong(), false);
                        m_dob->SubscribeEntity(typeIdVal.toLongLong(), sdt::InstanceId(), false);
                    }
                    else if (baseClass == TypesystemRepository::Message)
                    {
                        emit OpenMessageInstanceViewer(typeIdVal.toLongLong(), Safir::Dob::Typesystem::ChannelId::ALL_CHANNELS, false);
                        m_dob->SubscribeMessage(typeIdVal.toLongLong(), Safir::Dob::Typesystem::ChannelId::ALL_CHANNELS, false);
                    }
                }
            }
            else
            {
                // TypeId but no baseClass, must be an Enum
                emit OpenEnumViewer(typeIdVal.toLongLong(), QString());
            }
        }
    });

    connect(ui->inheritanceRadioButton, &QRadioButton::toggled, this, [this](bool checked)
    {
        SetTreeViewModel(checked);
    });

    m_contextMenuHandler = new TypesystemContextMenuHandler(m_dob, ui->treeView);
    connect(m_contextMenuHandler, &TypesystemContextMenuHandler::OpenObjectEdit, this, &TypesystemWidget::OpenObjectEdit);
    connect(m_contextMenuHandler, &TypesystemContextMenuHandler::OpenEntityInstanceViewer, this, &TypesystemWidget::OpenEntityInstanceViewer);
    connect(m_contextMenuHandler, &TypesystemContextMenuHandler::OpenMessageInstanceViewer, this, &TypesystemWidget::OpenMessageInstanceViewer);
    connect(m_contextMenuHandler, &TypesystemContextMenuHandler::OpenDouFile, this, &TypesystemWidget::OpenDouFile);
}

void TypesystemWidget::SetupFilterEdit()
{
    auto searchAction = new QAction(this);
    searchAction->setIcon(IconFactory::GetSearchIcon());
    auto menu = new QMenu();

    m_typeTreeFilterAction = menu->addAction("Filter");
    m_parameterSearchAction = menu->addAction("Find parameter");
    m_enumValueSearchAction = menu->addAction("Find enum value");


    m_typeTreeFilterAction->setCheckable(true);
    m_parameterSearchAction->setCheckable(true);
    m_enumValueSearchAction->setCheckable(true);
    m_typeTreeFilterAction->setChecked(true);

    connect(m_typeTreeFilterAction, &QAction::triggered, this, &TypesystemWidget::SetTypeTreeSearch);
    connect(m_parameterSearchAction, &QAction::triggered, this, &TypesystemWidget::SetParameterSearch);
    connect(m_enumValueSearchAction, &QAction::triggered, this, &TypesystemWidget::SetEnumValueSearch);

    searchAction->setMenu(menu);
    ui->filterLineEdit->addAction(searchAction, QLineEdit::LeadingPosition);
    connect(searchAction, &QAction::triggered, this, [this, searchAction](bool)
    {
        auto pos = mapToGlobal(ui->filterLineEdit->pos());
        searchAction->menu()->move(QPoint{pos.x(), pos.y() + ui->filterLineEdit->height()});
        searchAction->menu()->show();
    });

    connect(ui->filterLineEdit, &QLineEdit::textChanged, this, &TypesystemWidget::ApplyFilter);
    connect(ui->filterLineEdit, &TypeAheadWidget::ItemSelected, this, [this](const QModelIndex& selIx)
    {
        if (ui->filterLineEdit->Model() == nullptr || !selIx.isValid())
        {
            return;
        }
        auto typeId = ui->filterLineEdit->Model()->data(selIx, TypesystemRepository::DobTypeIdRole).toLongLong();
        auto fullName = ui->filterLineEdit->Model()->data(selIx, Qt::DisplayRole).toString();
        auto name = fullName.mid(fullName.lastIndexOf('.') + 1);
        if (m_parameterSearchAction->isChecked())
        {
            emit OpenParameterViewer(typeId, name);
        }
        else if (m_enumValueSearchAction->isChecked())
        {
            emit OpenEnumViewer(typeId, name);
        }
    });
}

void TypesystemWidget::SetTypeTreeSearch(bool)
{
    m_typeTreeFilterAction->setChecked(true);
    m_parameterSearchAction->setChecked(false);
    m_enumValueSearchAction->setChecked(false);
    ui->filterLineEdit->SetModel(nullptr);
    ui->filterLineEdit->setPlaceholderText("Filter");
}

void TypesystemWidget::SetParameterSearch(bool)
{
    m_typeTreeFilterAction->setChecked(false);
    m_parameterSearchAction->setChecked(true);
    m_enumValueSearchAction->setChecked(false);
    ui->filterLineEdit->setPlaceholderText("Find parameter");

    if (m_parameterSearchModel == nullptr)
    {
        InitParameterSearchModel();
    }
    ui->filterLineEdit->SetModel(m_parameterSearchModel);
}

void TypesystemWidget::SetEnumValueSearch(bool)
{
    m_typeTreeFilterAction->setChecked(false);
    m_parameterSearchAction->setChecked(false);
    m_enumValueSearchAction->setChecked(true);
    ui->filterLineEdit->setPlaceholderText("Find enum value");

    if (m_enumValueSearchModel == nullptr)
    {
        InitEnumValueSearchModel();
    }
    ui->filterLineEdit->SetModel(m_enumValueSearchModel);
}

void TypesystemWidget::InitParameterSearchModel()
{
    namespace sdt = Safir::Dob::Typesystem;
    auto model = new QStandardItemModel(this);
    model->setColumnCount(1);
    auto typeIds = sdt::Operations::GetClassTree(Safir::Dob::Parametrization::ClassTypeId);
    for (auto tid : typeIds)
    {
        auto className = QString::fromStdWString(sdt::Operations::GetName(tid));
        auto num = sdt::Parameters::GetNumberOfParameters(tid);
        for (auto p = 0; p < num; ++p)
        {
            auto paramName = QString::fromStdWString(sdt::Parameters::GetName(tid, p));
            if (!paramName.contains("@"))
            {
                auto item = new QStandardItem(QString("%1.%2").arg(className, paramName));
                item->setData(QVariant::fromValue(tid), TypesystemRepository::DobTypeIdRole);
                model->invisibleRootItem()->appendRow(item);
            }
        }
    }

    m_parameterSearchModel = new QSortFilterProxyModel(this);
    m_parameterSearchModel->setSourceModel(model);
    m_parameterSearchModel->sort(0);
}

void TypesystemWidget::InitEnumValueSearchModel()
{
    namespace sdt = Safir::Dob::Typesystem;
    auto model = new QStandardItemModel(this);
    model->setColumnCount(1);
    for (const auto e : TypesystemRepository::Instance().EnumsSorted())
    {
        auto enumName = QString::fromStdWString(sdt::Operations::GetName(e->typeId));
        for (const auto& v : e->values)
        {
            auto item = new QStandardItem(QString("%1.%2").arg(enumName, v));
            item->setData(QVariant::fromValue(e->typeId), TypesystemRepository::DobTypeIdRole);
            model->invisibleRootItem()->appendRow(item);
        }
    }

    m_enumValueSearchModel = new QSortFilterProxyModel(this);
    m_enumValueSearchModel->setSourceModel(model);
    m_enumValueSearchModel->sort(0);
}

void TypesystemWidget::ExpandTo(const QModelIndex& index)
{
    QModelIndex i = index;
    while (i.isValid())
    {
        ui->treeView->expand(i);
        i = i.parent();
    }

    ui->treeView->selectionModel()->setCurrentIndex(index, QItemSelectionModel::SelectCurrent);
}

void TypesystemWidget::SetTreeViewModel(bool inheritanceModel)
{
    // Get the currently selected type before changing model.
    auto currentTypeId = ui->treeView->selectionModel() != nullptr ?
        ui->treeView->model()->data(ui->treeView->selectionModel()->currentIndex(), TypesystemRepository::DobTypeIdRole) : QVariant{};

    // Toggle the model inheritance <--> namespace
    if (inheritanceModel)
    {
        // Create inheritance model
        ui->treeView->setModel(m_inheritanceProxyModel);
        ui->treeView->expand(m_inheritanceProxyModel->index(0, 0, {})); // Always expand the Object root node
    }
    else
    {
        // Create Namepspacemodel
        ui->treeView->setModel(m_namespaceProxyModel);
    }

    // Apply the same filter again after the model has changed.
    ApplyFilter(ui->filterLineEdit->text());

    // If a type was selected before we changed model, set it as selected again in this model.
    if (currentTypeId.isValid())
    {
        auto ix = ui->treeView->model()->match(ui->treeView->model()->index(0, 0), TypesystemRepository::DobTypeIdRole, currentTypeId, 1, Qt::MatchFlags(Qt::MatchExactly | Qt::MatchRecursive));
        if (ix.size() > 0 && ix.constFirst().isValid())
        {
            ExpandTo(ix.constFirst());
        }
    }
}

void TypesystemWidget::ApplyFilter(const QString& filterText)
{
    if (!m_typeTreeFilterAction->isChecked())
    {
        return;
    }

    if (filterText.length() > 2)
    {
        m_inheritanceProxyModel->SetFilter(filterText);
        m_namespaceProxyModel->SetFilter(filterText);
        ui->treeView->expandAll();
    }
    else
    {

        m_inheritanceProxyModel->SetFilter("");
        m_namespaceProxyModel->SetFilter("");
    }
}

void TypesystemWidget::LocateType(int64_t typeId)
{
    auto startIndex = ui->treeView->model()->index(0, 0);
    auto ix = ui->treeView->model()->match(startIndex, TypesystemRepository::DobTypeIdRole, QVariant::fromValue(typeId), 1, Qt::MatchFlags(Qt::MatchExactly | Qt::MatchRecursive));
    if (ix.size() > 0 && ix.constFirst().isValid())
    {
        ui->filterLineEdit->clear();
        ui->treeView->collapseAll();
        ExpandTo(ix.constFirst());
    }
}

void TypesystemWidget::SetSearchFocus()
{
    ui->filterLineEdit->setFocus();
}
