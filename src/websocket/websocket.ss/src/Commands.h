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

namespace Commands
{
    static const std::wstring Open = L"Open";
    static const std::wstring Close = L"Close";
    static const std::wstring SubscribeMessage = L"SubscribeMessage";
    static const std::wstring SendMessage = L"SendMessge";
    static const std::wstring UnsubscribeMessage = L"UnsubscribeMessage";
    static const std::wstring SubscribeEntity = L"SubscribeEntity";


    static const std::wstring GetTypeHierarchy = L"GetTypeHierarchy";

    static const std::wstring GeneralError = L"General Error";
}
