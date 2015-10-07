/******************************************************************************
*
* Copyright Saab AB, 2008-2013 (http://safir.sourceforge.net)
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
#include "About.h"
#include "memgraph.h"
#include "entitystats.h"
#include "connectionstats.h"
#include "SystemPicturePage.h"
#include "numberofentities.h"
#include "RawStatisticsPage.h"
#include <Safir/Dob/Internal/Initialize.h>
#include <Safir/Dob/Typesystem/Operations.h>
#include <Safir/Dob/Typesystem/Utilities.h>
#include <Safir/Dob/Entity.h>
#include <Safir/Dob/Internal/Connections.h>
#include <set>

namespace
{
    const int EntityWidgetType = 10;
    const int ConnectionWidgetType = 11;
    const int RemoteConnectionWidgetType = 11;

    void SortChildrenRecursive(QTreeWidgetItem* item)
    {
        for (int i = 0; i < item->childCount(); ++i)
        {
            SortChildrenRecursive(item->child(i));
            item->child(i)->sortChildren(0,Qt::AscendingOrder);
        }
    }
}



DoseMon::DoseMon(QWidget * /*parent*/)
    : m_doseInternalInitialized(false)
    , m_entitiesAdded(false)
{
    setupUi(this); // this sets up GUI

    m_doseInternalInitializer = boost::thread([this]
                                              {
                                                  Safir::Dob::Internal::InitializeDoseInternalFromApp();
                                                  m_doseInternalInitialized = true;
                                              });

    while (tabWidget->currentIndex() != -1)
    {
        tabWidget->removeTab(tabWidget->currentIndex());
    }

    tabWidget->setTabsClosable(true);
    connect(tabWidget,
            SIGNAL(tabCloseRequested(int)),
            this,
            SLOT(CloseTab(int)));

    int newTab = tabWidget->addTab(new About(this),"About");
    tabWidget->setTabToolTip(newTab,tabWidget->widget(newTab)->toolTip());

    connect(treeWidget,
            SIGNAL(itemActivated(QTreeWidgetItem*,int)),
            this,
            SLOT(TreeItemActivated(QTreeWidgetItem*, int)));

    connect(&m_updateTimer,SIGNAL(timeout()), this, SLOT(UpdateTreeWidget()));
    m_updateTimer.start(1000);
    UpdateTreeWidget();
}


void DoseMon::closeEvent(QCloseEvent* event)
{
    if (m_doseInternalInitializer != boost::thread())
    {
        m_doseInternalInitializer.interrupt();
        m_doseInternalInitializer.join();
    }

    while (tabWidget->count() != 0)
    {
        QWidget* w = tabWidget->widget(0);
        QCloseEvent evt;
        QCoreApplication::sendEvent(w,&evt);
        if (evt.isAccepted())
        {
            tabWidget->removeTab(0);
            delete w;
        }
        else
        {
            event->ignore();
            return;
        }
    }
    event->accept();
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
    if (item->text(0) == "About")
    {
        newTab = tabWidget->addTab(new About(this),"About");
        tabWidget->setTabToolTip(newTab,tabWidget->widget(newTab)->toolTip());
    }
    if (item->text(0) == "Memory")
    {
        newTab = tabWidget->addTab(new MemGraph(this),"Memory");
        tabWidget->setTabToolTip(newTab,tabWidget->widget(newTab)->toolTip());
    }
    else if (item->text(0) == "Raw Node Statistics")
    {
        newTab = tabWidget->addTab(new RawStatisticsPage(this),"Raw Node Statistics");
    }
    else if (item->text(0) == "System Picture")
    {
        newTab = tabWidget->addTab(new SystemPicturePage(this),"System Picture");
    }
    else if (item->text(0) == "Entity Statistics")
    {
        if (m_doseInternalInitialized)
        {
            newTab = tabWidget->addTab(new NumberOfEntities(this),"Entity Statistics");
        }
        else
        {
            QMessageBox::information(this,"Not initialized","dose_internal not yet initialized, cannot open.");
        }
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
        tabWidget->setTabToolTip(newTab,"Information about entity " + item->text(0));
    }
    else if (item->type() == ConnectionWidgetType)
    {
        newTab = tabWidget->addTab(new ConnectionStats(this, item->text(0)), item->text(0));
        tabWidget->setTabToolTip(newTab,"Information about connection " + item->text(0));
    }
    else
    {
        return;
        //This code used to be here, don't really understand why...
        //newTab = tabWidget->addTab(new QFrame(this),item->text(0));
    }

    if (newTab != -1)
    {
        tabWidget->setCurrentIndex(newTab);
    }
}


void DoseMon::CloseTab(int index)
{
    QWidget * widget = tabWidget->widget(index);
    QCloseEvent event;
    QCoreApplication::sendEvent(widget,&event);
    if (event.isAccepted())
    {
        tabWidget->removeTab(index);
        delete widget;
    }
}


void DoseMon::AddEntitesToTreeWidget()
{
    if (!m_doseInternalInitialized || m_entitiesAdded)
    {
        return;
    }

    m_entitiesAdded = true;

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
    QList<QTreeWidgetItem *> items = treeWidget->findItems("Entities",Qt::MatchExactly);
    SortChildrenRecursive(items.front());

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
    if (!m_doseInternalInitialized)
    {
        return;
    }


    AddEntitesToTreeWidget();

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

    connectionItem->sortChildren(0, Qt::AscendingOrder);
    remoteConnectionItem->sortChildren(0, Qt::AscendingOrder);
}

