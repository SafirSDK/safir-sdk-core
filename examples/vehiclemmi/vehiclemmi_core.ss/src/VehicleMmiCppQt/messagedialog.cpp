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

#include "messagedialog.h"
#include "qtworkaround.h"
#include <Capabilities/Vehicles/VehicleMsg.h>

namespace VehicleMmiCppQt
{

    MessageDialog::MessageDialog(QWidget *parent)
        : QDialog(parent, Qt::WindowTitleHint|Qt::WindowSystemMenuHint) //Remove help button
    {
        ui.setupUi(this);

        m_secDobConnection.Attach();
  
        /*const Safir::Dob::Typesystem::EntityId 
            entityId(Capabilities::Vehicles::VehicleMsg::ClassTypeId,
            Safir::Dob::Typesystem::WHOLE_CLASS)*/;
        m_secDobConnection.SubscribeMessage(Capabilities::Vehicles::VehicleMsg::ClassTypeId, Safir::Dob::Typesystem::ChannelId(), this);
    }


    MessageDialog::~MessageDialog()
    {
        // Dob will automatically be disconnected.
    }


    void MessageDialog::Show()
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


    void MessageDialog::OnMessage(const Safir::Dob::MessageProxy messageProxy)
    {
        Capabilities::Vehicles::VehicleMsgPtr msg = 
            boost::static_pointer_cast<Capabilities::Vehicles::VehicleMsg>(messageProxy.GetMessage());

        if (!msg->MessageText().IsNull())
        {
            QString qstrMsg = QtWorkaround::StdWStringToQString(msg->MessageText().GetVal());
            ui.labelMessage->setText(qstrMsg);
        }
        else
        {
            ui.labelMessage->setText( "No text in received message");
        }
        Show();
    }
}
