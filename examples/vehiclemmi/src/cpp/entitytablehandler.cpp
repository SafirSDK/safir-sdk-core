/******************************************************************************
*
* Copyright Saab AB, 2008-2013 (http://safir.sourceforge.net)
*
* Created by: Petter Lönnstedt / stpeln
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

#include "entitytablehandler.h"
#include "qtworkaround.h"
#include <Capabilities/Vehicles/Vehicle.h>

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning (disable: 4127)
#pragma warning (disable: 4251)
#endif

#include <QHeaderView>

#ifdef _MSC_VER
#pragma warning(pop)
#endif

namespace VehicleMmiCppQt
{

    EntityTableHandler::EntityTableHandler(QTableWidget* pTable, const Safir::Dob::Typesystem::TypeId& rcClassTypeId)
    {
        m_pTable = pTable;
        m_classTypeID = rcClassTypeId;
        m_rowHeight = 20;

        if(m_pTable)
        {
            // Connect signals and slots.
            connect(m_pTable,SIGNAL(itemSelectionChanged()),this,SIGNAL(SigSelectionChanged()));
        }

        // Attach to the secondary Dob connection.
        m_secDobConnection.Attach();
    }


    EntityTableHandler::~EntityTableHandler()
    {
        // Dob will automatically be disconnected.
    }


    void EntityTableHandler::Init()
    {
        // Hide the vertical header.
        m_pTable->verticalHeader()->hide();
        // Make the columns movable by the user.
        m_pTable->horizontalHeader()->setSectionsMovable(true);
        // Read only.
        m_pTable->setEditTriggers(QAbstractItemView::NoEditTriggers);

        // Subscribe for vehicle.
         m_secDobConnection.SubscribeEntity(m_classTypeID, this);
    }


    Safir::Dob::Typesystem::EntityId EntityTableHandler::GetSelectedEntityId()
    {
        Safir::Dob::Typesystem::EntityId entityId;

        QList<QTableWidgetItem*> selectedItems = m_pTable->selectedItems();
        if(!selectedItems.isEmpty())
        {
            // We are only interested in what row an item is selected
            // on. To only save one item per row.
            QList<QTableWidgetItem*> firstColumnItems;

            for(QList<QTableWidgetItem*>::const_iterator it = selectedItems.constBegin() ;
                it != selectedItems.constEnd() ;
                it++)
            {
                if(*it)
                {
                    if((*it)->column() == 0)
                    {
                        firstColumnItems.append(*it);
                    }
                }
            }

            if(firstColumnItems.count() == 1)
            {
                ItemToObjectIdMap::const_iterator it = m_itemMap.find(firstColumnItems.takeFirst());
                if(it != m_itemMap.end())
                {
                    return (*it).second;
                }
            }
        }
        return entityId;
    }


    int EntityTableHandler::GetNrOfSelectedRows()
    {
        QList<QTableWidgetItem*> firstColumnItems;
        QList<QTableWidgetItem*> selectedItems = m_pTable->selectedItems();
        if(!selectedItems.isEmpty())
        {
            for(QList<QTableWidgetItem*>::const_iterator it = selectedItems.constBegin() ;
                it != selectedItems.constEnd() ;
                it++)
            {
                if(*it)
                {
                    if((*it)->column() == 0)
                    {
                        firstColumnItems.append(*it);
                    }
                }
            }

        }
        return firstColumnItems.count();
    }

    QTableWidgetItem* EntityTableHandler::CreateRow(int row)
    {
        QTableWidgetItem* pFirstItem = NULL;
        for(int column = 0 ; column < m_pTable->columnCount() ; column++)
        {
            QTableWidgetItem* pItem = new QTableWidgetItem();
            if(pItem)
            {
                m_pTable->setItem(row,column,pItem);
                if(pFirstItem == NULL)
                {
                    pFirstItem = pItem;
                }
            }
        }

        return pFirstItem;
    }

    void EntityTableHandler::UpdateRow(int row, Safir::Dob::EntityPtr entity)
    {
        const Capabilities::Vehicles::VehiclePtr pVehiclePtr 
            = boost::static_pointer_cast<Capabilities::Vehicles::Vehicle>(entity);

        // Name
        if(pVehiclePtr->Identification().IsChanged())
        {
            QString qstrIdentification("");
            if(!pVehiclePtr->Identification().IsNull())
            {
                qstrIdentification = QtWorkaround::StdWStringToQString(pVehiclePtr->Identification().GetVal());
            }

            QTableWidgetItem* pItem = m_pTable->item(row,0);
            if(pItem)
            {
                pItem->setText(qstrIdentification);
            }
        }

        // Id
        if(pVehiclePtr->VehicleCategory().IsChanged())
        {
            QString qstrVehicleCategory("");

            if(!pVehiclePtr->VehicleCategory().IsNull())
            {
                qstrVehicleCategory = QtWorkaround::StdWStringToQString(Capabilities::Vehicles::VehicleCategoryCode::ToString(pVehiclePtr->VehicleCategory().GetVal()));
            }

            QTableWidgetItem* pItem = m_pTable->item(row, 1);
            if(pItem)
            {
                pItem->setText(qstrVehicleCategory);
            }
        }

        // Speed
        if(pVehiclePtr->Speed().IsChanged())
        {
            QString qstrSpeed("");
            if(!pVehiclePtr->Speed().IsNull())
            {
                qstrSpeed.setNum(pVehiclePtr->Speed().GetVal());
            }
            else
            {
                qstrSpeed = "-";
            }

            QTableWidgetItem* pItem = m_pTable->item(row,2);
            if(pItem)
            {
                pItem->setText(qstrSpeed);
            }
        }

        // Position
        if(pVehiclePtr->Position().IsChanged())
        {
            if(!pVehiclePtr->Position().IsNull())
            {
                QString qstrPosLat("");
                QString qstrPosLong("");
                if(!pVehiclePtr->Position()->Latitude().IsNull())
                {
                    qstrPosLat.setNum(pVehiclePtr->Position()->Latitude().GetVal());
                }
                else
                {
                    qstrPosLat = "-";
                }

                QTableWidgetItem* pItem = m_pTable->item(row,3);
                if(pItem)
                {
                    pItem->setText(qstrPosLat);
                }

                if(!pVehiclePtr->Position()->Longitude().IsNull())
                {
                    qstrPosLong.setNum(pVehiclePtr->Position()->Longitude().GetVal());
                }
                else
                {
                    qstrPosLong = "-";
                }

                pItem = m_pTable->item(row,4);
                if(pItem)
                {
                    pItem->setText(qstrPosLong);
                }
            }
        }
    }
  
    void EntityTableHandler::OnNewEntity(const Safir::Dob::EntityProxy entityProxy)
    {
        OnNewEntityInternal(entityProxy);
    }
    
    void EntityTableHandler::OnUpdatedEntity(const Safir::Dob::EntityProxy entityProxy)
    {
        OnUpdatedEntityInternal(entityProxy);
    }

    void EntityTableHandler::OnDeletedEntity(const Safir::Dob::EntityProxy entityProxy, const bool /*deprecated*/)
    {
        OnDeletedEntityInternal(entityProxy);
    }

    void EntityTableHandler::OnNewEntityInternal(const Safir::Dob::EntityProxy &entityProxy)
    {
        if(entityProxy.GetTypeId() == m_classTypeID)
        {
            EntityIdToItemMap::const_iterator it = m_entityIdMap.find(entityProxy.GetEntityId());
            if(it == m_entityIdMap.end())
            {
                // Append row.
                int nRow = m_pTable->rowCount();
                m_pTable->setRowCount(nRow+1);
                m_pTable->setRowHeight(nRow,m_rowHeight);

                // Create
                QTableWidgetItem* pTableItem = CreateRow(nRow);
                if(pTableItem)
                {
                    m_entityIdMap[entityProxy.GetEntityId()] = pTableItem;
                    m_itemMap[pTableItem] = entityProxy.GetEntityId();
                }
                else
                {
                    m_pTable->setRowCount(nRow);
                }
            }
            // Update data.
            OnUpdatedEntityInternal(entityProxy);
        }
    }

    void EntityTableHandler::OnUpdatedEntityInternal(const Safir::Dob::EntityProxy &entityProxy)
    {
        if(entityProxy.GetTypeId() == m_classTypeID)
        {
            // Get matching row.
            EntityIdToItemMap::const_iterator it = m_entityIdMap.find(entityProxy.GetEntityId());
            if(it != m_entityIdMap.end())
            {
                if((*it).second)
                {
                    // Update data.
                    UpdateRow((*it).second->row(), entityProxy.GetEntityWithChangeInfo());
                }
            }
        }
    }

    void EntityTableHandler::OnDeletedEntityInternal(const Safir::Dob::EntityProxy &entityProxy)
    {
        EntityIdToItemMap::iterator itEntityId = m_entityIdMap.find(entityProxy.GetEntityId());
        if(itEntityId != m_entityIdMap.end())
        {
            if((*itEntityId).second)
            {
                ItemToObjectIdMap::iterator itItem = m_itemMap.find((*itEntityId).second);
                if(itItem != m_itemMap.end())
                {
                    m_itemMap.erase(itItem);
                }
                m_pTable->removeRow((*itEntityId).second->row());
            }
            m_entityIdMap.erase(itEntityId);
        }
    }

} // namespace VehicleMmiCppQt
