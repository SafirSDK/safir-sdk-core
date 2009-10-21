/******************************************************************************
*
* Copyright Saab AB, 2008 (http://www.safirsdk.com)
*
* Created by: Lars Hagström / stlrha
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

#include "common_header.h"
#include "dosemon.h"
#include <sstream>
#include "memgraph.h"
#include "entitystats.h"
#include "nodestatus.h"
#include "dosecominfo.h"
#include "connectionstats.h"
#include <Safir/Dob/Typesystem/Operations.h>
#include <Safir/Dob/Typesystem/Utilities.h>
#include <Safir/Dob/Entity.h>
#include <Safir/Dob/Internal/Connections.h>
#include <set>

const int EntityWidgetType = 10;
const int ConnectionWidgetType = 11;
const int RemoteConnectionWidgetType = 11;

DoseMon::DoseMon(QWidget * /*parent*/)
{
    setupUi(this); // this sets up GUI

    treeWidget->setSortingEnabled(true);
    treeWidget->sortByColumn(0,Qt::AscendingOrder);

    tabWidget->clear();

    QPushButton * closeButton = new QPushButton("X");
    tabWidget->setCornerWidget(closeButton);
    tabWidget->addTab(new MemGraph(this),"Memory");

    connect(closeButton,
            SIGNAL(clicked()),
            this,
            SLOT(CloseCurrentTab()));

    connect(treeWidget,
            SIGNAL(itemActivated(QTreeWidgetItem*,int)),
            this,
            SLOT(TreeItemActivated(QTreeWidgetItem*, int)));

    connect(&m_updateTimer,SIGNAL(timeout()), this, SLOT(UpdateTreeWidget()));
    m_updateTimer.start(3000);
    UpdateTreeWidget();

    AddEntitesToTreeWidget();
}

bool DoseMon::ActivateTab(const QString& name)
{
    for (int i = 0; i < tabWidget->count(); ++i)
    {
        if (tabWidget->tabText(i) == name)
        {
            tabWidget->setCurrentIndex(i);
            return true;
        }
    }
    return false;
}

void DoseMon::TreeItemActivated ( QTreeWidgetItem * item, int /*column*/ )
{
    if (ActivateTab(item->text(0)))
    {
        return;
    }

    int newTab = -1;
    if (item->text(0) == "Memory")
    {
        newTab = tabWidget->addTab(new MemGraph(this),"Memory");
    }
    else if (item->text(0) == "Node Statuses")
    {
        newTab = tabWidget->addTab(new NodeStatus(this),"Node Statuses");
    }
    else if (item->text(0) == "DoseCom Info")
    {
        newTab = tabWidget->addTab(new DoseComInfo(this),"DoseCom Info");
    }
    else if (item->parent() == NULL)
    {
        return;
    }
    else if (item->type() == EntityWidgetType)
    {
        const std::wstring text = Safir::Dob::Typesystem::Utilities::ToWstring(item->text(0).toStdString());
        const Safir::Dob::Typesystem::TypeId typeId = Safir::Dob::Typesystem::Operations::GetTypeId(text);
        newTab = tabWidget->addTab(new EntityStats(this,typeId),item->text(0));
    }
    else if (item->type() == ConnectionWidgetType)
    {
        newTab = tabWidget->addTab(new ConnectionStats(this, item->text(0)), item->text(0));
    }
    else
    {
        newTab = tabWidget->addTab(new QFrame(this),item->text(0));
    }

    if (newTab != -1)
    {
        tabWidget->setCurrentIndex(newTab);
    }
}


void DoseMon::CloseCurrentTab()
{
    QWidget * currentTab = tabWidget->currentWidget();
    tabWidget->removeTab(tabWidget->currentIndex());
    delete currentTab;
}


void DoseMon::AddEntitesToTreeWidget()
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
                const std::wstring parentName = GetName(GetParentType(*it));

                if (parentName == GetName(Safir::Dob::Entity::ClassTypeId))
                {
                    QList<QTreeWidgetItem *> items = treeWidget->findItems("Entities",Qt::MatchExactly);

                    items.front()->addChild(new QTreeWidgetItem(QStringList(ToUtf8(GetName(*it)).c_str()),EntityWidgetType));
                    erase = true;
                }
                else
                {
                    QList<QTreeWidgetItem *> items = treeWidget->findItems(ToUtf8(parentName).c_str(), Qt::MatchExactly|Qt::MatchRecursive);
                    if (!items.empty())
                    {
                        items.front()->addChild(new QTreeWidgetItem(QStringList(ToUtf8(GetName(*it)).c_str()),EntityWidgetType));
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
}

void DoseMon::AddConnection(const Safir::Dob::Internal::Connection & connection,
                            std::set<QString>& localConnectionNames,
                            std::set<QString>& remoteConnectionNames)
{
    if (connection.IsLocal())
    {
        localConnectionNames.insert(connection.NameWithCounter());
    }
    else
    {
        remoteConnectionNames.insert(connection.NameWithCounter());
    }
}

void DoseMon::UpdateTreeWidget()
{
    using namespace Safir::Dob::Internal;


    QTreeWidgetItem* connectionItem = NULL;
    QTreeWidgetItem* remoteConnectionItem = NULL;

    {
        QList<QTreeWidgetItem *> items = treeWidget->findItems("Connections", Qt::MatchExactly|Qt::MatchRecursive);
        assert(items.size() == 1);
        connectionItem = items.front();
    }

    {
        QList<QTreeWidgetItem *> items = treeWidget->findItems("Remote Connections", Qt::MatchExactly|Qt::MatchRecursive);
        assert(items.size() == 1);
        remoteConnectionItem = items.front();
    }


    std::set<QString> localConnectionNames;
    std::set<QString> remoteConnectionNames;

    Connections::Instance().ForEachConnection(boost::bind(&DoseMon::AddConnection,this,_1,boost::ref(localConnectionNames),boost::ref(remoteConnectionNames)));

    /*** local connections ***/
    {
        int numItems = connectionItem->childCount();
        for (int i = 0; i < numItems;)
        {
            QTreeWidgetItem *item = connectionItem->child(i);

            std::set<QString>::iterator findIt = localConnectionNames.find(item->text(0));
            if (findIt == localConnectionNames.end())
            {
                connectionItem->takeChild(i);
                --numItems;
                delete item;
            }
            else
            {
                localConnectionNames.erase(item->text(0));
                ++i;
            }
        }

        for (std::set<QString>::iterator it = localConnectionNames.begin();
             it != localConnectionNames.end(); ++it)
        {
            connectionItem->addChild(new QTreeWidgetItem(QStringList(*it),ConnectionWidgetType));
        }
    }

    /*** remote connections ***/
    {
        int numItems = remoteConnectionItem->childCount();
        for (int i = 0; i < numItems;)
        {
            QTreeWidgetItem *item = remoteConnectionItem->child(i);

            std::set<QString>::iterator findIt = remoteConnectionNames.find(item->text(0));
            if (findIt == remoteConnectionNames.end())
            {
                remoteConnectionItem->takeChild(i);
                --numItems;
                delete item;
            }
            else
            {
                remoteConnectionNames.erase(item->text(0));
                ++i;
            }
        }

        for (std::set<QString>::iterator it = remoteConnectionNames.begin();
             it != remoteConnectionNames.end(); ++it)
        {
            remoteConnectionItem->addChild(new QTreeWidgetItem(QStringList(*it),RemoteConnectionWidgetType));
        }
    }
}
