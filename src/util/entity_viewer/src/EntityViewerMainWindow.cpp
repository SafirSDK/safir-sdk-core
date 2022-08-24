/******************************************************************************
*
* Copyright Saab AB, 2022 (http://safirsdkcore.com)
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
#include "EntityViewerMainWindow.h"
#include <Safir/Dob/Typesystem/Operations.h>
#include <Safir/Dob/Typesystem/Utilities.h>
#include <Safir/Dob/Entity.h>
#include <Safir/Utilities/Internal/Expansion.h>
#include <set>
#include <QCloseEvent>
#include <QSortFilterProxyModel>

#include "EntityModel.h"
#include "EntityView.h"

namespace
{
    void ExpandParents(QTreeWidgetItem *item)
    {
        auto parent = item->parent();
        while (parent != nullptr)
        {
            parent->setExpanded(true);
            parent->setHidden(false);
            parent = parent->parent();
        }
    }

    void ApplyFilter(const QString& filter, QTreeWidgetItem* item)
    {
        static const QBrush defaultBackground;
        item->setBackground(0, defaultBackground);
        item->setHidden(false);

        // require at least 3 characters before filter is applied
        if (filter.count() > 2)
        {
            auto filterHit = item->text(0).contains(filter, Qt::CaseInsensitive);
            if (filterHit)
            {
                item->setBackground(0, Qt::yellow);
                ExpandParents(item); // Make item visible in tree by expanding all parent nodes.
            }
            else
            {
                item->setHidden(true);
            }
        }

        for (int i = 0; i < item->childCount(); i++)
        {
            QTreeWidgetItem *child = item->child(i);
            ApplyFilter(filter, child);
        }
    }
}

EntityViewerMainWindow::EntityViewerMainWindow()
{
    m_ui.setupUi(this);
    m_ui.treeWidget->setDisabled(true);
    m_ui.splitter->setSizes({150,500});
    connect(m_ui.tabWidget, &QTabWidget::tabCloseRequested, this, &EntityViewerMainWindow::CloseTab);
    connect(m_ui.treeWidget,&QTreeWidget::itemActivated, this, &EntityViewerMainWindow::TreeItemActivated);
    connect(m_ui.filterEdit, &QLineEdit::textChanged, this, &EntityViewerMainWindow::FilterChanged);
    connect(m_ui.actionSecond64AsFloatingPoint, &QAction::toggled, this, &EntityViewerMainWindow::Second64SettingsChanged);
    connect(m_ui.actionSecond64AsLocalTime, &QAction::toggled, this, &EntityViewerMainWindow::Second64SettingsChanged);
    connect(m_ui.actionSecond64AsUtcTime, &QAction::toggled, this, &EntityViewerMainWindow::Second64SettingsChanged);

    m_ui.statusbar->addPermanentWidget(new QLabel(tr("SAFIR_INSTANCE: %1").
                                                  arg(Safir::Utilities::Internal::Expansion::GetSafirInstance())));

    m_ui.statusbar->showMessage(tr("Loading types..."));
    PopulateTree();
    m_ui.statusbar->showMessage(tr("Connecting..."));
}


EntityViewerMainWindow::~EntityViewerMainWindow()
{

}

void EntityViewerMainWindow::SetConnection(Safir::Dob::Connection& connection)
{
    m_connection = &connection;
    m_ui.statusbar->showMessage(tr("Connected"));
    m_ui.treeWidget->setEnabled(m_connection != nullptr);
}


void EntityViewerMainWindow::PopulateTree()
{
    using namespace Safir::Dob::Typesystem;
    using namespace Safir::Dob::Typesystem::Operations;
    using namespace Safir::Dob::Typesystem::Utilities;

    typedef std::set<TypeId> TypeIdSet;
    TypeIdSet entities;

    {
        TypeIdVector ent = GetClassTree(Safir::Dob::Entity::ClassTypeId);
        entities.insert(ent.begin(),ent.end());
    }

    while (!entities.empty())
    {
        for (TypeIdSet::iterator it = entities.begin();
             it != entities.end();)
        {
            bool erase = false;
            if (*it == Safir::Dob::Entity::ClassTypeId)
            {
                erase = true;
            }
            else
            {
                if (GetParentType(*it) == Safir::Dob::Entity::ClassTypeId)
                {
                    m_ui.treeWidget->addTopLevelItem(new QTreeWidgetItem(QStringList(ToUtf8(GetName(*it)).c_str())));
                    erase = true;
                }
                else
                {
                    const std::wstring parentName = GetName(GetParentType(*it));
                    QList<QTreeWidgetItem *> items = m_ui.treeWidget->findItems(ToUtf8(parentName).c_str(), Qt::MatchExactly|Qt::MatchRecursive);
                    if (!items.empty())
                    {
                        items.front()->addChild(new QTreeWidgetItem(QStringList(ToUtf8(GetName(*it)).c_str())));
                        erase = true;
                    }
                }
            }

            if (erase)
            {
                TypeIdSet::iterator oldIt = it;
                ++it;
                entities.erase(oldIt);
            }
            else
            {
                ++it;
            }
        }
    }
    m_ui.treeWidget->sortItems(0,Qt::AscendingOrder);
}


void EntityViewerMainWindow::CloseTab(int index)
{
    QWidget* widget = m_ui.tabWidget->widget(index);
    QCloseEvent event;
    QCoreApplication::sendEvent(widget,&event);
    if (event.isAccepted())
    {
        widget->deleteLater();
        EntityView* view = dynamic_cast<EntityView*>(m_ui.tabWidget->widget(index));
        if (view != nullptr)
        {
            view->model()->deleteLater();
        }
        m_ui.tabWidget->removeTab(index);
    }
}

void EntityViewerMainWindow::TreeItemActivated(QTreeWidgetItem* item)
{
    if (m_connection == nullptr)
    {
        return;
    }
    const auto text = item->text(0);
    for(int i = 0; i < m_ui.tabWidget->count(); ++i)
    {
        if (m_ui.tabWidget->tabText(i) == text)
        {
            m_ui.tabWidget->setCurrentIndex(i);
            return;
        }
    }

    const Safir::Dob::Typesystem::TypeId typeId = Safir::Dob::Typesystem::Operations::GetTypeId(Safir::Dob::Typesystem::Utilities::ToWstring(text.toStdString()));
    auto model = new EntityModel(typeId, *m_connection, this);

    if (m_ui.actionSecond64AsFloatingPoint->isChecked())
    {
        model->setSecond64Format(EntityModel::FloatingPoint);
    }
    else if (m_ui.actionSecond64AsLocalTime->isChecked())
    {
        model->setSecond64Format(EntityModel::LocalTime);
    }
    else if (m_ui.actionSecond64AsUtcTime->isChecked())
    {
        model->setSecond64Format(EntityModel::UtcTime);
    }

    auto proxyModel = new QSortFilterProxyModel(this);
    proxyModel->setSourceModel(model);
    auto view = new EntityView(proxyModel, this);
    auto newTab = m_ui.tabWidget->addTab(view, text);
    m_ui.tabWidget->setCurrentIndex(newTab);
}

void EntityViewerMainWindow::FilterChanged(const QString& filter)
{
    ApplyFilter(filter, m_ui.treeWidget->invisibleRootItem());
}

void EntityViewerMainWindow::Second64SettingsChanged()
{
    for(int i = 0; i < m_ui.tabWidget->count(); ++i)
    {
        auto entityView = dynamic_cast<EntityView*>(m_ui.tabWidget->widget(i));
        if (entityView == nullptr)
        {
            continue;
        }

        auto model = static_cast<EntityModel*>(static_cast<QSortFilterProxyModel*>(entityView->model())->sourceModel());

        if (m_ui.actionSecond64AsFloatingPoint->isChecked())
        {
            model->setSecond64Format(EntityModel::FloatingPoint);
        }
        else if (m_ui.actionSecond64AsLocalTime->isChecked())
        {
            model->setSecond64Format(EntityModel::LocalTime);
        }
        else if (m_ui.actionSecond64AsUtcTime->isChecked())
        {
            model->setSecond64Format(EntityModel::UtcTime);
        }

    }
}
