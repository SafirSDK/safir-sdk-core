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
#ifndef __ENTITYDIALOG_H
#define __ENTITYDIALOG_H

#include "ui_entitydialog.h"
#include "entitytablehandler.h"
#include <QDialog>
#include <Safir/Dob/Connection.h>

namespace VehicleMmiCppQt
{
    /** 
    * Implements dialog to present detailed information of a vehicle.
    * The dialog sends create and update requests for vehicles.
    */
    class EntityDialog : 
        public QDialog,         
        // Allows this class to receive response on sent requests
        public Safir::Dob::Requestor
    {
        Q_OBJECT

    public:
        /**
        * Initiates dialog and connects to Dob.
        */
        EntityDialog(QWidget *parent = 0);

        /**
        * Detach Dob connection.
        */
        ~EntityDialog();
        
        /**
        * Opens dialog to create a new vehicle vehicle.
        */
        void CreateVehicle();

        /**
        * Opens dialog to update an existing vehicle.
        */
        void UpdateVehicle(Safir::Dob::Typesystem::EntityId objId);

        /** 
         * Overrides Safir.Dob.Requestor. Called by the Dob 
         * when a response is received on a sent request.
         */
        virtual void OnResponse(const Safir::Dob::ResponseProxy responseProxy);

        /** 
         * Overrides Safir.Dob.Requestor. Called by the Dob 
         * to indicate that it is meningful to make a retry 
         * after an overflow situation
         */
        virtual void OnNotRequestOverflow();

        
    private:
        void ClearAllControls();    // Empties all controls in this dialog
        void Show();                // Show this dialog

        EntityTableHandler* m_pEntityTableHandler;
         
        Safir::Dob::SecondaryConnection m_secDobConnection;
        Safir::Dob::Typesystem::EntityId m_vehicleObjectId;

        bool m_clickedOKButton;     // Indicates if OK button pressed
        bool m_clickedApplyButton;  // Indicates if Apply button pressed
        bool m_isCreate;            // Indicates if operator made create Vehicle choice

        Ui::EntityDialogClass ui;

    private slots:
        void on_pushButtonApply_clicked();
        void on_pushButtonOK_clicked();
    };
}
#endif // __ENTITYDIALOG_H
