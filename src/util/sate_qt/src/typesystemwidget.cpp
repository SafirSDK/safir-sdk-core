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
#include <QLineEdit>
#include <QDebug>
#include <QTreeView>
#include <QModelIndex>
#include <QRadioButton>


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
    m_inheritanceProxyModel->setSourceModel(new TypesystemInheritanceModel(this));
    m_namespaceProxyModel = new TypesystemFilterProxyModel(this);
    m_namespaceProxyModel->setSourceModel(new TypesystemNamespaceModel(this));

    SetTreeViewModel(true);

    connect(ui->filterLineEdit, &QLineEdit::textChanged, this, [this](const QString& f)
    {
            ApplyFilter(f);
    });

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

                if (ctrlKey && !shiftKey)
                {
                    emit OpenObjectEdit(typeIdVal.toLongLong());
                }
                else if (shiftKey && !ctrlKey)
                {
                    if (baseClass == TypesystemRepository::Entity)
                    {
                        emit OpenEntityInstanceViewer(typeIdVal.toLongLong(), true);
                    }
                    //TODO do other ctrl double click actions
                }
                else if (!shiftKey && !ctrlKey)
                {
                    if (baseClass == TypesystemRepository::Entity)
                    {
                        emit OpenEntityInstanceViewer(typeIdVal.toLongLong(), false);
                    }
                    else if (baseClass == TypesystemRepository::Message)
                    {
                        emit OpenMessageInstanceViewer(typeIdVal.toLongLong(),
                                                       Safir::Dob::Typesystem::ChannelId::ALL_CHANNELS,
                                                       true);
                    }
                    //TODO do other double click actions
                }
            }
            else
            {
                // TypeId but no baseClass, must be an Enum
                emit OpenDouFile(typeIdVal.toLongLong());
            }
        }
    });

    connect(ui->inheritanceRadioButton, &QRadioButton::toggled, this, [this](bool checked)
    {
        SetTreeViewModel(checked);
    });

    // TODO: remove std functions and use signals
    m_contextMenuHandler = new TypesystemContextMenuHandler(m_dob, ui->treeView);
    connect(m_contextMenuHandler, &TypesystemContextMenuHandler::OpenObjectEdit, this, &TypesystemWidget::OpenObjectEdit);
    connect(m_contextMenuHandler, &TypesystemContextMenuHandler::OpenEntityInstanceViewer, this, &TypesystemWidget::OpenEntityInstanceViewer);
    connect(m_contextMenuHandler, &TypesystemContextMenuHandler::OpenMessageInstanceViewer, this, &TypesystemWidget::OpenMessageInstanceViewer);
    connect(m_contextMenuHandler, &TypesystemContextMenuHandler::OpenDouFile, this, &TypesystemWidget::OpenDouFile);
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
