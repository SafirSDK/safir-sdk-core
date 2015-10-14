/******************************************************************************
*
* Copyright Saab AB, 2008-2013 (http://safirsdkcore.com)
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

#include "MessageSender.h"
#include <Capabilities/Vehicles/VehicleMsg.h>
#include <Safir/Dob/OverflowException.h>

namespace VehicleAppCpp
{
    MessageSender::MessageSender()
    {
    }

    MessageSender& MessageSender::Instance()
    {
        static MessageSender inst;
        return inst;
    }

    void MessageSender::Init() 
    {
        m_connection.Attach();  
    }

    void MessageSender::OnNotMessageOverflow()
    {
        // Retry to send the message.
        SendMaxNofVehicleMsg();
    }

    void MessageSender::SendMaxNofVehicleMsg()
    {
        // This is just a test to see how you create messages.
        // This is not part of the design pattern.
        
        Capabilities::Vehicles::VehicleMsgPtr msg =
            Capabilities::Vehicles::VehicleMsg::Create();

        msg -> MessageText().SetVal(L"Number of vehicles reached defined limit.");

        try
        {
            m_connection.Send(msg, Safir::Dob::Typesystem::ChannelId(), this);
        }
        catch (Safir::Dob::OverflowException)
        {
            // Do nothing OnNotMessageOverflow() will be called when 
            // overflow situation solved.
        }
    }
};
