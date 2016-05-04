/******************************************************************************
*
* Copyright Saab AB, 2016 (http://safirsdkcore.com)
*
* Created by: Joel Ottosson / joel.ottosson@consoden.se
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
#pragma once

#include <Safir/Websocket/Send.h>

namespace ws = Safir::Websocket;

namespace CommandValidator
{
    inline void ValidateOpen(const ws::SendPtr& cmd)
    {
        if (cmd->ConnectionName().IsNull() && cmd->ConnectionName().GetVal().empty())
            throw std::invalid_argument("ConnectionName is mandatory in command 'Open'");
    }

    inline void ValidateSubscribeMessage(const ws::SendPtr& cmd)
    {
        if (cmd->Type().IsNull())
            throw std::invalid_argument("ConnectionName is mandatory in command 'Open'");

        if (!Safir::Dob::Typesystem::Operations::IsOfType(cmd->Type().GetVal(), Safir::Dob::Message::ClassTypeId))
            throw std::invalid_argument("Type must refer to a subtype of Safir.Dob.Message");
    }

    inline void ValidateSendMessage(const ws::SendPtr& cmd)
    {
        if (cmd->Message().IsNull())
            throw std::invalid_argument("Message is mandatory in command 'SendMessage'");
    }

    inline void ValidateUnsubscribeMessage(const ws::SendPtr& cmd)
    {
        if (cmd->Type().IsNull())
            throw std::invalid_argument("Type is mandatory in command 'UnsubscribeMessage'");
    }

}
