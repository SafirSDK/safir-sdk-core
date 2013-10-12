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

#include "servicedialog.h"
#include "qtworkaround.h"
#include <Capabilities/CalculateSpeedDifference.h>
#include <Capabilities/CalculateSpeedDifferenceResponse.h>
#include <Safir/Dob/OverflowException.h>

namespace VehicleMmiCppQt
{

    ServiceDialog::ServiceDialog(QWidget *parent)
        : QDialog(parent, Qt::WindowTitleHint|Qt::WindowSystemMenuHint) // Remove help button        
    {
        ui.setupUi(this);
        m_secDobConnection.Attach();
    }


    ServiceDialog::~ServiceDialog()
    {
        // Dob will automatically be disconnected.
    }


    void ServiceDialog::Show()
    {
        if(isHidden())
        {
            show();
        }
        else
        {
            activateWindow();
        }
    }


    void ServiceDialog::Open(Safir::Dob::Typesystem::EntityId entityId)
    {
        window()->setWindowTitle("Calculate Speed");
        ui.lineEditIdentification->setText("");
        ui.lineEditCurrentSpeed->setText("");
        ui.lineEditNewSpeed->setText("");
        ui.lineEditSpeedDiff->setText("");
        ui.lineEditStatusBar->setText("OK");
        ui.lineEditNewSpeed->setFocus();

        // Get data from Dob and fill dialog
         m_pVehicle =
            boost::static_pointer_cast<Capabilities::Vehicles::Vehicle>(m_secDobConnection.Read(entityId).GetEntity());

        if(!m_pVehicle->Identification().IsNull() )
        {
            QString qstrName = QtWorkaround::StdWStringToQString(m_pVehicle->Identification().GetVal());
            ui.lineEditIdentification->setText(qstrName);
        }

        if(!m_pVehicle->Speed().IsNull() )
        {
            float speed = m_pVehicle->Speed().GetVal();
            QString qstrSpeed("");
            qstrSpeed.setNum(speed);
            ui.lineEditCurrentSpeed->setText(qstrSpeed);
        }
        else
        {
            ui.lineEditStatusBar->setText("Current speed must exist.");
        }

        Show();
    }


    void ServiceDialog::on_pushButtonApply_clicked()
    {
        // The dialog is open until operator selects Close button.
        if(ui.lineEditCurrentSpeed->text() == NULL)
        {
            ui.lineEditStatusBar->setText("Current speed must exist.");
            return;
        }
        else
        {
            ui.lineEditStatusBar->clear();

            // Create service request
            Capabilities::CalculateSpeedDifferencePtr req = Capabilities::CalculateSpeedDifference::Create();

            // Set service request values 
            /*const Capabilities::CalculateSpeedDifferencePtr calcSpeedRefPtr = 
                boost::static_pointer_cast<Capabilities::CalculateSpeedDifference>(m_secDobConnection.Read(m_pVehicle).GetEntityId()));*/
                        
            req->ObjectWithSpeed().SetPtr(boost::static_pointer_cast<Safir::Dob::Entity>(m_pVehicle));
            req->Speed().SetVal(ui.lineEditNewSpeed->text().toFloat());

            try
            {
                // Send service request
                m_secDobConnection.ServiceRequest(req, Safir::Dob::Typesystem::HandlerId(), this);
            }
            catch(Safir::Dob::OverflowException)
            {
                ui.lineEditStatusBar->setText("Overflow when sending, please wait!");
            }
        }
    }


    void ServiceDialog::OnResponse(const Safir::Dob::ResponseProxy responseProxy)
    {
        if (responseProxy.GetRequestTypeId() == Capabilities::CalculateSpeedDifference::ClassTypeId)
        {
            Capabilities::CalculateSpeedDifferenceResponsePtr response = Capabilities::CalculateSpeedDifferenceResponse::Create();
            response = boost::static_pointer_cast<Capabilities::CalculateSpeedDifferenceResponse>(responseProxy.GetResponse());
           
            if(!response->SpeedDifference().IsNull())
            {
                float speed = response->SpeedDifference().GetVal();
                QString qstrSpeed("");
                qstrSpeed.setNum(speed);
                ui.lineEditSpeedDiff->setText(qstrSpeed);              
                ui.lineEditStatusBar->setText("OK"); 
            }
            else
            {      
                ui.lineEditStatusBar->setText("Could not calculate speed - speed difference has no value.");    
            }
        }
        else
        {
            ui.lineEditStatusBar->setText("Error");
        }    
    }


    void ServiceDialog::OnNotRequestOverflow()
    {
        // No automatic resending is made. 
       ui.lineEditStatusBar->setText("Overflow situation solved. Make request again!");
    }

}
