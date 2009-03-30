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

#include "entitydialog.h"
#include "qtworkaround.h"
#include <Capabilities/Vehicles/Vehicle.h>
#include <Safir/Dob/OverflowException.h>
#include <Safir/Dob/SuccessResponse.h>

namespace VehicleMmiCppQt
{

    EntityDialog::EntityDialog(QWidget *parent) :
        QDialog(parent, Qt::WindowTitleHint|Qt::WindowSystemMenuHint), // Remove help button
        m_clickedOKButton(false), 
        m_clickedApplyButton(false), 
        m_isCreate(true)
    {
        ui.setupUi(this);

        // Fill combo-box with values for identity
        for(int i = 0; i < Capabilities::Vehicles::VehicleCategoryCode::Size(); i++)
        {
            ui.comboBoxCategory->addItem("");
            ui.comboBoxCategory->setItemText(i, QtWorkaround::StdWStringToQString(Capabilities::Vehicles::VehicleCategoryCode::ToString((Capabilities::Vehicles::VehicleCategoryCode::Enumeration)(i))));
        }

        m_secDobConnection.Attach();
    }


    EntityDialog::~EntityDialog()
    {
        // Dob will automatically be disconnected.
    }


    void EntityDialog::ClearAllControls()
    {
        ui.lineEditIdentification->clear();
        ui.lineEditIdentification->setEnabled(true);
        ui.comboBoxCategory->setCurrentIndex(0);
        ui.lineEditSpeed->clear();
        ui.lineEditPositionLat->clear();
        ui.lineEditPositionLong->clear();
        ui.lineEditStatusBar->setText("OK");

        m_clickedOKButton = false;
        m_clickedApplyButton = false;
    }


    void EntityDialog::Show()
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


    void EntityDialog::CreateVehicle()
    {
        window()->setWindowTitle("Create Vehicle");
        ClearAllControls();
        ui.lineEditIdentification->setFocus();
        m_isCreate = true;
        Show();
    }


    void EntityDialog::UpdateVehicle(Safir::Dob::Typesystem::EntityId entityId)
    {
        window()->setWindowTitle("Update Vehicle");
        ClearAllControls();
        ui.lineEditIdentification->setEnabled(false); // Not allowed to update callsign.
        ui.comboBoxCategory->setFocus();
      
        m_vehicleObjectId = entityId;       
        m_isCreate = false;

        // Get data from Dob and fill dialog
        Capabilities::Vehicles::VehiclePtr pVehicle =
            boost::static_pointer_cast<Capabilities::Vehicles::Vehicle>(m_secDobConnection.Read(entityId).GetEntity());

        if(!pVehicle->Identification().IsNull() )
        {
            QString qstrName = QtWorkaround::StdWStringToQString(pVehicle->Identification().GetVal());
            ui.lineEditIdentification->setText(qstrName);
        }

        if(!pVehicle->VehicleCategory().IsNull() )
        {
            int index = ui.comboBoxCategory->findText(QtWorkaround::StdWStringToQString(Capabilities::Vehicles::VehicleCategoryCode::ToString(pVehicle->VehicleCategory().GetVal())));
            ui.comboBoxCategory->setCurrentIndex(index);
        }

        if(!pVehicle->Speed().IsNull() )
        {
            float speed = pVehicle->Speed().GetVal();
            QString qstrSpeed("");
            qstrSpeed.setNum(speed);
            ui.lineEditSpeed->setText(qstrSpeed);
        }
    
        if(!pVehicle->Position().IsNull() )
        {
            if(!pVehicle->Position()->Latitude().IsNull())
            {
                float posLat = pVehicle->Position()->Latitude().GetVal();
                QString qstrPosLat("");
                qstrPosLat.setNum(posLat);
                ui.lineEditPositionLat->setText(qstrPosLat);
            }

            if(!pVehicle->Position()->Longitude().IsNull())
            {
                float posLong = pVehicle->Position()->Longitude().GetVal();
                QString qstrPosLong("");
                qstrPosLong.setNum(posLong);
                ui.lineEditPositionLong->setText(qstrPosLong);
            }
        }

        Show();
    }

    
    void EntityDialog::OnResponse(const Safir::Dob::ResponseProxy responseProxy)
    {
        Safir::Dob::SuccessResponsePtr success = boost::dynamic_pointer_cast<Safir::Dob::SuccessResponse>(responseProxy.GetResponse());
        if(success)
        {
            if(m_clickedOKButton)
            {
                accept(); // Close dialog
            }
            ui.lineEditStatusBar->setText("OK");
        }
        else
        {
            ui.lineEditStatusBar->setText("Error");
        }
        m_clickedOKButton = false;
        m_clickedApplyButton = false;
    }


    void EntityDialog::OnNotRequestOverflow()
    {
        // No automatic resending is made. 
       ui.lineEditStatusBar->setText("Overflow situation solved. Make request again!");
    }


    void EntityDialog::on_pushButtonOK_clicked()
    {
        ui.lineEditStatusBar->clear();
        m_clickedOKButton = true;
        on_pushButtonApply_clicked();
    }


    void EntityDialog::on_pushButtonApply_clicked()
    {
        if(!m_clickedOKButton)
        {
            m_clickedApplyButton = true;
        }

        ui.lineEditStatusBar->clear();

        // Collect the values from the dialog controls and store them in the vehicle 
        // entity and send a create or update request.

        Capabilities::Vehicles::VehiclePtr pVehicle = Capabilities::Vehicles::Vehicle::Create();

        if(ui.lineEditSpeed->isModified())
        {           
           if(ui.lineEditSpeed->text() == NULL)
           {
               pVehicle->Speed().SetNull();
           }
           else
           {
               pVehicle->Speed().SetVal((ui.lineEditSpeed->text()).toFloat());
           }
        }

        Safir::Geodesy::PositionPtr pPosition = Safir::Geodesy::Position::Create();
       
        if(ui.lineEditPositionLat->text() == NULL || ui.lineEditPositionLong->text() == NULL)
        {
            pVehicle->Position().SetNull();
        }
        else
        {
            pPosition->Latitude().SetVal((ui.lineEditPositionLat->text()).toFloat());
            pPosition->Longitude().SetVal((ui.lineEditPositionLong->text()).toFloat());
            pPosition->Altitude().SetVal(Safir::Geodesy::Position::DummyAltitude());
            pVehicle->Position().SetPtr(pPosition);
        }
        
        Capabilities::Vehicles::VehicleCategoryCode::Enumeration eVehicleCategory
            = Capabilities::Vehicles::VehicleCategoryCode::ToValue(QtWorkaround::QStringToStdWString(ui.comboBoxCategory->currentText()));
        pVehicle->VehicleCategory().SetVal(eVehicleCategory);
        
        if(m_isCreate)
        {
            if( ui.lineEditIdentification->text() == NULL)
            {
                ui.lineEditStatusBar->setText(QString("Identification must be given."));
                return;
            }
            else
            {
                pVehicle->Identification().SetVal(QtWorkaround::QStringToStdWString(ui.lineEditIdentification->text()));
            }

            // Send create request;
            try
            {
                m_secDobConnection.CreateRequest(pVehicle, Safir::Dob::Typesystem::HandlerId(), this);
            }
            catch(Safir::Dob::OverflowException)
            {
                ui.lineEditStatusBar->setText("Overflow when sending, please wait!");
            }
        }
        else
        {
            // Send update request
            try
            {
                m_secDobConnection.UpdateRequest(pVehicle, m_vehicleObjectId.GetInstanceId(), this);
            }
            catch(Safir::Dob::OverflowException)
            {
                ui.lineEditStatusBar->setText("Overflow when sending, please wait!");
            }
        }
    }
}
