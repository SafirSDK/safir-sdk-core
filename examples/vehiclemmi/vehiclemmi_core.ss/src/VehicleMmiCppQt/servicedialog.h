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

#ifndef __SERVICEDIALOG_H
#define __SERVICEDIALOG_H

#include "ui_servicedialog.h"

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning (disable: 4127)
#endif

#include <QDialog>

#ifdef _MSC_VER
#pragma warning(pop)
#endif

#include <Capabilities/Vehicles/Vehicle.h>
#include <Safir/Dob/Connection.h>

namespace VehicleMmiCppQt
{
    /** 
    * Implements dialog to use service CalculateSpeedDifference.
    * The dialog sends a service request and receives the 
    * service response.
    */
    class ServiceDialog : 
        public QDialog, 
        // Allows this class to receive response on sent requests
        public Safir::Dob::Requestor
    {
        Q_OBJECT

    public:
        ServiceDialog(QWidget *parent = 0);
        ~ServiceDialog();
        
        /**
        * Open dialog to send CalculateSpeedDifference service request
        */
        void Open(Safir::Dob::Typesystem::EntityId objId);

        /**
        * Overrides Safir.Dob.Requestor. Called when a response is 
        * received on a sent request
        */
        virtual void OnResponse(const Safir::Dob::ResponseProxy responseProxy);

        /**
        * Overrides Safir.Dob.Requestor. Called to indicate that it 
        * is meningful to make a retry after an overflow situation
        */
        virtual void OnNotRequestOverflow();

    private:
        void Show(); // Show this dialog

        Safir::Dob::SecondaryConnection         m_secDobConnection;
        Capabilities::Vehicles::VehiclePtr      m_pVehicle;

        Ui::ServiceDialogClass                  ui;

    private slots:
        void on_pushButtonApply_clicked();
    };
}
#endif // __SERVICEDIALOG_H
