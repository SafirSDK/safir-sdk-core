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

#include "categoryinfodialog.h"
#include "qtworkaround.h"
#include <Safir/Dob/SuccessResponse.h>
#include <Safir/Dob/OverflowException.h>
#include <Capabilities/Vehicles/Vehicle.h>
#include <Capabilities/Vehicles/GetVehicleCategoryService.h>
#include <Capabilities/Vehicles/GetVehicleCategoryResponse.h>
#include <Capabilities/Vehicles/SetVehicleCategoryService.h>

namespace VehicleMmiCppQt
{

    CategoryInfoDialog::CategoryInfoDialog(QWidget *parent)
        : QDialog(parent, Qt::WindowTitleHint|Qt::WindowSystemMenuHint) // Remove help button
    {
        ui.setupUi(this);
        m_secDobConnection.Attach();
    }


    CategoryInfoDialog::~CategoryInfoDialog()
    {
        // Dob will automatically be disconnected.
    }


    void CategoryInfoDialog::ClearAllControls()
    {
        ui.lineEditAdditionalRemark->clear();
        ui.lineEditSpeed->clear();
        ui.lineEditCategoryCode->clear();
        ui.checkBoxDrivingLicenseRequired->setChecked(false);
        ui.lineEditStatusBar->setText("OK");
    }


    void CategoryInfoDialog::Show()
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


    void CategoryInfoDialog::Open(Safir::Dob::Typesystem::EntityId entityId)
    {
        window()->setWindowTitle("Category Information");
        ClearAllControls();
        ui.lineEditSpeed->setFocus();

        // Get data (if any) from Dob and fill dialog

        Capabilities::Vehicles::VehiclePtr pVehicle = boost::static_pointer_cast<Capabilities::Vehicles::Vehicle>(m_secDobConnection.Read(entityId).GetEntity());
        Capabilities::Vehicles::VehicleCategoryInfoPtr pVehicleCatInfo = Capabilities::Vehicles::VehicleCategoryInfo::Create();

        if(!pVehicle->VehicleCategory().IsNull())
        {
            pVehicleCatInfo->VehicleCategory().SetVal(pVehicle->VehicleCategory().GetVal());
            m_pVehicleCatInfo = pVehicleCatInfo;
        }
        else
        {
            // Vehicle category has no value
            return;
        }

        if(!m_pVehicleCatInfo->VehicleCategory().IsNull())
        {
            ui.lineEditCategoryCode->setText(QtWorkaround::StdWStringToQString(Capabilities::Vehicles::VehicleCategoryCode::ToString(
                    m_pVehicleCatInfo->VehicleCategory().GetVal())));

            // Create service request to read category information from database
            Capabilities::Vehicles::GetVehicleCategoryServicePtr req = Capabilities::Vehicles::GetVehicleCategoryService::Create();
            req->VehicleCategory().SetVal(m_pVehicleCatInfo->VehicleCategory().GetVal());

            try
            {
                // Send service request
                m_secDobConnection.ServiceRequest(req, Safir::Dob::Typesystem::HandlerId(), this);
            }
            catch(Safir::Dob::OverflowException)
            {
                ui.lineEditStatusBar->setText("Overflow when sending, please wait!");
            }

            Show();
        }
    }


    void CategoryInfoDialog::OnResponse(const Safir::Dob::ResponseProxy responseProxy)
    {
        Safir::Dob::SuccessResponsePtr success = boost::dynamic_pointer_cast<Safir::Dob::SuccessResponse>(responseProxy.GetResponse());

        if(responseProxy.GetRequestTypeId() == Capabilities::Vehicles::GetVehicleCategoryService::ClassTypeId)
        {
            if(success)
            {
                Capabilities::Vehicles::GetVehicleCategoryResponsePtr response = Capabilities::Vehicles::GetVehicleCategoryResponse::Create();

                response = boost::static_pointer_cast<Capabilities::Vehicles::GetVehicleCategoryResponse>(responseProxy.GetResponse());

                if(!response->VehicleCategoryInfo().IsNull())
                {
                    if(!response->VehicleCategoryInfo()->Remark().IsNull())
                    {
                        ui.lineEditAdditionalRemark->setText(QtWorkaround::StdWStringToQString(response->VehicleCategoryInfo()->Remark().GetVal()));
                    }

                    if(!response->VehicleCategoryInfo()->MaxSpeed().IsNull())
                    {
                        float speed = response->VehicleCategoryInfo()->MaxSpeed().GetVal();
                        QString qstrSpeed("");
                        qstrSpeed.setNum(speed);
                        ui.lineEditSpeed->setText(qstrSpeed);
                    }

                    if(!response->VehicleCategoryInfo()->IsDrivingLicenceRequired().IsNull())
                    {
                        ui.checkBoxDrivingLicenseRequired->setChecked(response->VehicleCategoryInfo()->IsDrivingLicenceRequired().GetVal());
                    }
                }
            }
            else
            {
                // The chosen category had no information stored persistently
            }
        }

        else // If the response is a SetVehicleCategoryResponse
        {
            if(success)
            {
                ui.lineEditStatusBar->setText("OK");
            }
            else
            {
                ui.lineEditStatusBar->setText("Error");
            }
        }
    }


    void CategoryInfoDialog::OnNotRequestOverflow()
    {
        // No automatic resending is made.
       ui.lineEditStatusBar->setText("Overflow situation solved. Make request again!");
    }


    void CategoryInfoDialog::on_pushButtonApply_clicked()
    {
        ui.lineEditStatusBar->clear();

        // Collect the values from the dialog controls and store them in database
        // by sending a service request.

        //Capabilities::Vehicles::VehicleCategoryInfoPtr pVehicleCategoryInfo =
        //    Capabilities::Vehicles::VehicleCategoryInfo::Create();

        /*const*/Capabilities::Vehicles::SetVehicleCategoryServicePtr req = Capabilities::Vehicles::SetVehicleCategoryService::Create();

        req->VehicleCategoryInfo().SetPtr(boost::static_pointer_cast<Capabilities::Vehicles::VehicleCategoryInfo>(m_pVehicleCatInfo));
        /*bool isModified = false;
        if(ui.lineEditSpeed->isModified())
        {
            isModified = true;*/

            if(ui.lineEditSpeed->text() == NULL)
            {
                req->VehicleCategoryInfo()->MaxSpeed().SetNull();
            }
            else
            {
                req->VehicleCategoryInfo()->MaxSpeed().SetNull();
                req->VehicleCategoryInfo()->MaxSpeed().SetVal((ui.lineEditSpeed->text()).toFloat());
            }
        //}

        /*if(ui.lineEditAdditionalRemark->isModified())
        {
            isModified = true;*/

            if(ui.lineEditAdditionalRemark->text() == NULL)
            {
                req->VehicleCategoryInfo()->Remark().SetNull();
            }
            else
            {
                req->VehicleCategoryInfo()->Remark().SetVal(QtWorkaround::QStringToStdWString(ui.lineEditAdditionalRemark->text()));
            }
        //}

        //if(ui.checkBoxDrivingLicenseRequired->ismodified())
            req->VehicleCategoryInfo()->IsDrivingLicenceRequired().SetVal(ui.checkBoxDrivingLicenseRequired->isChecked());

            req->VehicleCategoryInfo()->VehicleCategory().SetVal(Capabilities::Vehicles::VehicleCategoryCode::ToValue(QtWorkaround::QStringToStdWString(ui.lineEditCategoryCode->text())));


            // Send the request.
            try
            {
                // Send service request
                m_secDobConnection.ServiceRequest(req, Safir::Dob::Typesystem::HandlerId(), this);
            }
            catch(Safir::Dob::OverflowException)
            {
                ui.lineEditStatusBar->setText("Overflow when sending, please wait!");
            }
        //}
    }
}
