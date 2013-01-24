/******************************************************************************
*
* Copyright Saab AB, 2008-2009 (http://www.safirsdk.com)
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

#ifndef __ENTITYTABLEHANDLER_H
#define __ENTITYTABLEHANDLER_H

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning (disable: 4127)
#pragma warning (disable: 4251)
#endif

#include <QTableWidget>

#ifdef _MSC_VER
#pragma warning(pop)
#endif

#include <Safir/Dob/Typesystem/Defs.h>
#include <Safir/Dob/Consumer.h>
#include <Safir/Dob/Connection.h>

namespace VehicleMmiCppQt
{

    class EntityTableHandler :
        public QObject,
        // Allows this class to subscribe for entity changes
        public Safir::Dob::EntitySubscriber
    {
        Q_OBJECT

    public:

        /**
        *  Attach to Dob.
        */  
        EntityTableHandler(QTableWidget* pTable, const Safir::Dob::Typesystem::TypeId& rcClassTypeId);
        
        /**
        *  Destructor
        */
        virtual ~EntityTableHandler();

        /**
        *  Setup vehicle table and subscribe for Vehicle entity changes.
        */      
        void Init();

        /**
        * Returns object id for last selected row in vehicle table.
        */      
        Safir::Dob::Typesystem::EntityId GetSelectedEntityId();

        /**
        * Returns number of rows that are selected in vehicle table.
        */      
        int GetNrOfSelectedRows();

        /**
        * Overrides Safir::Dob::EntityTableHandler. Called by Dob when a new 
        * entity is available.
        */
        virtual void OnNewEntity(const Safir::Dob::EntityProxy entityProxy);
    
        /**
        * Overrides Safir::Dob::EntityTableHandler. Called by Dob when an entity 
        * is updated.
        */
        virtual void OnUpdatedEntity(const Safir::Dob::EntityProxy entityProxy);

        /**
        * Overrides Safir::Dob::EntityTableHandler. Called when an entity is removed.
        */
        virtual void OnDeletedEntity(const Safir::Dob::EntityProxy entityProxy, const bool deletedByOwner);

    private:
        /**
        * Internal method to handle a new entity
        */
        void OnNewEntityInternal(const Safir::Dob::EntityProxy &entityProxy);
        
        /**
        * Internal method to handle an updated entity
        */
        void OnUpdatedEntityInternal(const Safir::Dob::EntityProxy &entityProxy);
        
        /**
        * Internal method to handle a deleted entity
        */
        void OnDeletedEntityInternal(const Safir::Dob::EntityProxy &entityProxy);

    protected:
        
        // Methods to create and update vehicle entities in table.
        QTableWidgetItem* CreateRow(int row);
        void UpdateRow(int row, Safir::Dob::EntityPtr entity);

        // Map definitions to bind a Dob object id to a row id in vehicle table.
        // Used when sending entity requests and receiving entity data.  
        typedef std::map<Safir::Dob::Typesystem::EntityId,QTableWidgetItem*> EntityIdToItemMap;
        typedef std::map<QTableWidgetItem*,Safir::Dob::Typesystem::EntityId> ItemToObjectIdMap;

        // Member variables
        QTableWidget*                   m_pTable;
        Safir::Dob::Typesystem::TypeId  m_classTypeID;
        EntityIdToItemMap               m_entityIdMap;
        ItemToObjectIdMap               m_itemMap;
        Safir::Dob::SecondaryConnection m_secDobConnection;
        int                             m_rowHeight;

    signals:
        void SigSelectionChanged();
    };

} // namespace VehicleMmiCppQt


#endif // __ENTITYTABLEHANDLER_H
