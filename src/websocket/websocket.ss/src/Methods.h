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
    static const std::string Ping = "ping";

    //******************************************************************
    // Methods used in requests from clients
    //******************************************************************
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
    static const std::string RegisterServiceHandler = "registerServiceHandler";
    static const std::string UnregisterHandler = "unregisterHandler";
    static const std::string SubscribeRegistration = "subscribeRegistration";
    static const std::string UnsubscribeRegistration = "unsubscribeRegistration";
    static const std::string CreateRequest = "createRequest";
    static const std::string UpdateRequest = "updateRequest";
    static const std::string DeleteRequest = "deleteRequest";
    static const std::string ServiceRequest = "serviceRequest";
    static const std::string SetEntityChanges = "setEntityChanges";
    static const std::string SetEntity = "setEntity";
    static const std::string DeleteEntity = "deleteEntity";
    static const std::string DeleteAllInstances = "deleteAllInstances";
    static const std::string ReadEntity = "readEntity";
    static const std::string IsCreated = "isCreated";
    static const std::string GetNumberOfInstances = "getNumberOfInstances";
    static const std::string GetAllInstanceIds = "getAllInstanceIds";
    static const std::string GetInstanceIdPolicy = "getInstanceIdPolicy";

    //typesystem
    static const std::string GetTypeHierarchy = "getTypeHierarchy";

    //******************************************************************
    // Methods used in notifications to clients
    //******************************************************************
    static const std::string OnCreateRequest = "onCreateRequest";
    static const std::string OnUpdateRequest = "onUpdateRequest";
    static const std::string OnDeleteRequest = "onDeleteRequest";
    static const std::string OnInjectedNewEntity = "onInjectedNewEntity";
    static const std::string OnInjectedUpdatedEntity = "onInjectedUpdatedEntity";
    static const std::string OnInjectedDeletedEntity = "onInjectedDeletedEntity";
    static const std::string OnInitialInjectionsDone = "onInitialInjectionsDone";
    static const std::string OnRevokedRegistration = "onRevokedRegistration";
    static const std::string OnCompletedRegistration = "onCompletedRegistration";
    static const std::string OnServiceRequest = "onServiceRequest";
    static const std::string OnNotRequestOverflow = "onNotRequestOverflow";
    static const std::string OnNotMessageOverflow = "onNotMessageOverflow";
    static const std::string OnRegistered = "onRegistered";
    static const std::string OnUnregistered = "onUnregistered";
    static const std::string OnMessage = "onMessage";
    static const std::string OnNewEntity = "onNewEntity";
    static const std::string OnUpdatedEntity = "onUpdatedEntity";
    static const std::string OnDeletedEntity = "onDeletedEntity";
}
