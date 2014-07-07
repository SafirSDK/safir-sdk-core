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

#include "entitymw.h"
#include <Capabilities/Vehicles/DeleteVehicleCategoryService.h>

#include <Safir/Dob/NotFoundException.h>
#include <Safir/Dob/OverflowException.h>
#include <Safir/Dob/SuccessResponse.h>

namespace VehicleMmiCppQt
{

    EntityMw::EntityMw(QWidget *parent, Qt::WindowFlags flags)
        : QMainWindow(parent, flags)
    {
        // Set up HMI
        ui.setupUi(this);

        // Create class that handles the QTableWidget containing vehicles
        m_pEntityTableHandler = new EntityTableHandler(ui.tableWidget,Capabilities::Vehicles::Vehicle::ClassTypeId);

        if(m_pEntityTableHandler)
        {
            m_pEntityTableHandler->Init();
        }

        m_secDobConnection.Attach();

        // Create dialogs
        m_pEntityDlg = new EntityDialog(this);
        m_pServiceDlg = new ServiceDialog(this);
        m_pMessageDlg = new MessageDialog(this);
        m_pCategoryInfoDlg = new CategoryInfoDialog(this);
    }


    EntityMw::~EntityMw()
    {
        // Dob will automatically be disconnected.

        if(m_pEntityTableHandler)
        {
            delete m_pEntityTableHandler;
            m_pEntityTableHandler = NULL;
        }
    }


    void EntityMw::OnResponse(const Safir::Dob::ResponseProxy responseProxy)
    {
        Safir::Dob::SuccessResponsePtr success = boost::dynamic_pointer_cast<Safir::Dob::SuccessResponse>(responseProxy.GetResponse());
        
        if(responseProxy.GetRequestTypeId() == Capabilities::Vehicles::DeleteVehicleCategoryService::ClassTypeId)
        {
            if(success)
            {
                statusBar()->showMessage("Category info for the chosen object has succesfully been deleted", 5000);
            }
            else
            {
                statusBar()->showMessage("Category info for the chosen object could not be deleted", 5000);          
            }
        }
        else
        {
            if(success)
            {
                statusBar()->showMessage("OK");
            }
            else
            {
                statusBar()->showMessage("Error");          
            }
        }
    }


    void EntityMw::OnNotRequestOverflow()
    {
        statusBar()->showMessage("Overflow situation solved. Make request again!"); 
    }


    void EntityMw::on_actionCreate_triggered()
    {
        m_pEntityDlg->CreateVehicle();
    }


    void EntityMw::on_actionUpdate_triggered()
    {
        if (m_pEntityTableHandler->GetNrOfSelectedRows()> 0)
        {
            Safir::Dob::Typesystem::EntityId entityId = m_pEntityTableHandler->GetSelectedEntityId();
            m_pEntityDlg->UpdateVehicle(entityId);
            statusBar()->showMessage("OK");
        }
        else
        {
            statusBar()->showMessage("No vehicle selected"); 
        }
    }


    void EntityMw::on_actionDelete_triggered()
    {
        Safir::Dob::Typesystem::EntityId entityId = m_pEntityTableHandler->GetSelectedEntityId();
        if(entityId.GetTypeId() != 0)
        {
            try
            {
                m_secDobConnection.DeleteRequest(entityId, this);
                statusBar()->showMessage("OK"); 
            }
            catch(Safir::Dob::OverflowException)
            {
                statusBar()->showMessage("Overflow when sending, please wait!"); 
            }
        }
        else
        {
            statusBar()->showMessage("No vehicle selected"); 
        }
    }


    void EntityMw::on_actionSpeed_triggered()
    {
        if (m_pEntityTableHandler->GetNrOfSelectedRows()> 0)
        {
            Safir::Dob::Typesystem::EntityId entityId = m_pEntityTableHandler->GetSelectedEntityId();
            m_pServiceDlg->Open(entityId);
            statusBar()->showMessage("OK"); 
        }
        else
        {
            statusBar()->showMessage("No vehicle selected"); 
        }
    }


    void EntityMw::on_actionCategory_triggered()
    {
        if(m_pEntityTableHandler->GetNrOfSelectedRows()> 0)
        {
            Safir::Dob::Typesystem::EntityId entityId = m_pEntityTableHandler->GetSelectedEntityId();
            m_pCategoryInfoDlg->Open(entityId);
            statusBar()->showMessage("OK");
        }
        else
        {
            statusBar()->showMessage("No vehicle selected");
        }
    }

    void EntityMw::on_actionDeleteCategoryInfo_triggered()
    {
        Safir::Dob::Typesystem::EntityId entityId = m_pEntityTableHandler->GetSelectedEntityId();
        
        if(entityId.GetTypeId() != 0)
        {
            Capabilities::Vehicles::VehiclePtr pVehicle = boost::static_pointer_cast<Capabilities::Vehicles::Vehicle>(m_secDobConnection.Read(entityId).GetEntity());
            if(pVehicle)
            {
                if(!pVehicle->VehicleCategory().IsNull())
                {
                    Capabilities::Vehicles::DeleteVehicleCategoryServicePtr req = Capabilities::Vehicles::DeleteVehicleCategoryService::Create();
                    req->VehicleCategory().SetVal(pVehicle->VehicleCategory().GetVal());
                    
                    try
                    {
                        m_secDobConnection.ServiceRequest(req, Safir::Dob::Typesystem::HandlerId(), this);
                        statusBar()->showMessage("OK"); 
                    }
                    catch(Safir::Dob::OverflowException)
                    {
                        statusBar()->showMessage("Overflow when sending, please wait!"); 
                    }
                }
            }
        }
        else
        {
            statusBar()->showMessage("No vehicle selected"); 
        }
    }
} // namespace VehicleMmiCppQt
