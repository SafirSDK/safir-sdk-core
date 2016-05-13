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
#include <string>

namespace Methods
{
    //dob
    static const std::string Open = "open";
    static const std::string Close = "close";
    static const std::string IsOpen = "isOpen";
    static const std::string SubscribeMessage = "subscribeMessage";
    static const std::string UnsubscribeMessage = "unsubscribeMessage";
    static const std::string SendMessage = "sendMessage";
    static const std::string SubscribeEntity = "subscribeEntity";
    static const std::string UnsubscribeEntity = "unsubscribeEntity";
    static const std::string RegisterEntityHandler = "registerEntityHandler";
    static const std::string UnregisterHandler = "unregisterHandler";

    //typesystem
    static const std::string GetTypeHierarchy = "getTypeHierarchy";

}
