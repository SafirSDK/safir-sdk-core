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
#ifndef CATEGORYINFODIALOG_H
#define CATEGORYINFODIALOG_H

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning (disable: 4127)
#pragma warning (disable: 4251)
#endif

#include <QDialog>
#include "ui_categoryinfodialog.h"

#ifdef _MSC_VER
#pragma warning(pop)
#endif

#include <Capabilities/Vehicles/VehicleCategoryInfo.h>
#include <Safir/Dob/Connection.h>

namespace VehicleMmiCppQt
{
    /** 
    * Implements dialog to present detailed information of a category.
    * The opening commande presents data in dialog (if data exists) for 
    * a particular category from a database. It is possible to change or 
    * remove data from this dialog.
    */
    class CategoryInfoDialog : 
        public QDialog,
        // Allows this class to receive response on sent requests
        public Safir::Dob::Requestor
    {
        Q_OBJECT

    public:
        /**
        * Initiates dialog and connects to Dob.
        */
        CategoryInfoDialog(QWidget *parent = 0);
        /**
        * Detach Dob connection.
        */
        ~CategoryInfoDialog();

        /**
        * Open dialog to send CalculateSpeedDifference service request
        */
        void Open(Safir::Dob::Typesystem::EntityId objId);

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

        Safir::Dob::SecondaryConnection                     m_secDobConnection;
        Capabilities::Vehicles::VehicleCategoryInfoPtr      m_pVehicleCatInfo;

        Ui::CategoryInfoDialogClass ui;

    private slots:
        void on_pushButtonApply_clicked();
    };
} 
#endif // CATEGORYINFODIALOG_H
