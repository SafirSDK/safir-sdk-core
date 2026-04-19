/******************************************************************************
*
* Copyright Saab AB, 2026 (http://safirsdkcore.com)
*
* Created by: Joel Ottosson
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
#include <iostream>
#include "../../src/RestRouting.h"

#ifdef _MSC_VER
#pragma warning(disable: 4127)
#endif

#ifndef CHECK
#define CHECK(expr) {if (!(expr)) { std::cout<<"Test failed! Line: "<<__LINE__<<", expr: "<< #expr <<std::endl; exit(1);}}
#endif

namespace
{
    namespace bhttp = boost::beast::http;

    // Helper: parse URL and route in one call
    inline RestRoute Route(bhttp::verb verb, const std::string& url)
    {
        std::vector<std::string> seg;
        std::unordered_map<std::string, std::string> query;
        ParseUrlTarget(url, seg, query);
        return RouteRestRequest(verb, seg, query);
    }
}

inline void RestRoutingTest()
{
    //-------------------------------------------
    // ParseBoolParam
    //-------------------------------------------

    {
        std::unordered_map<std::string, std::string> q;
        // key absent — returns default value
        CHECK(ParseBoolParam(q, "flag", true)  == true);
        CHECK(ParseBoolParam(q, "flag", false) == false);
    }
    {
        std::unordered_map<std::string, std::string> q = {{"flag", "true"}};
        CHECK(ParseBoolParam(q, "flag", false) == true);
    }
    {
        std::unordered_map<std::string, std::string> q = {{"flag", "1"}};
        CHECK(ParseBoolParam(q, "flag", false) == true);
    }
    {
        std::unordered_map<std::string, std::string> q = {{"flag", "false"}};
        CHECK(ParseBoolParam(q, "flag", true) == false);
    }
    {
        std::unordered_map<std::string, std::string> q = {{"flag", "0"}};
        CHECK(ParseBoolParam(q, "flag", true) == false);
    }
    {
        // Unrecognised value is treated as false
        std::unordered_map<std::string, std::string> q = {{"flag", "yes"}};
        CHECK(ParseBoolParam(q, "flag", true) == false);
    }
    {
        // Case-sensitive: "True" is not "true"
        std::unordered_map<std::string, std::string> q = {{"flag", "True"}};
        CHECK(ParseBoolParam(q, "flag", false) == false);
    }

    //-------------------------------------------
    // ParseUrlTarget
    //-------------------------------------------

    {
        // Path only, no query
        std::vector<std::string> seg;
        std::unordered_map<std::string, std::string> query;
        ParseUrlTarget("/connections/myConn/isOpen", seg, query);
        CHECK(seg.size() == 3);
        CHECK(seg[0] == "connections");
        CHECK(seg[1] == "myConn");
        CHECK(seg[2] == "isOpen");
        CHECK(query.empty());
    }
    {
        // Path with single query param
        std::vector<std::string> seg;
        std::unordered_map<std::string, std::string> query;
        ParseUrlTarget("/connections/c/entities/T/123?includeSubclasses=true", seg, query);
        CHECK(seg.size() == 5);
        CHECK(seg[4] == "123");
        CHECK(query.size() == 1);
        CHECK(query["includeSubclasses"] == "true");
    }
    {
        // Path with multiple query params
        std::vector<std::string> seg;
        std::unordered_map<std::string, std::string> query;
        ParseUrlTarget("/typeHierarchy?a=1&b=2&c=3", seg, query);
        CHECK(seg.size() == 1);
        CHECK(seg[0] == "typeHierarchy");
        CHECK(query.size() == 3);
        CHECK(query["a"] == "1");
        CHECK(query["b"] == "2");
        CHECK(query["c"] == "3");
    }
    {
        // Query param with no value (flag-style)
        std::vector<std::string> seg;
        std::unordered_map<std::string, std::string> query;
        ParseUrlTarget("/typeHierarchy?verbose", seg, query);
        CHECK(query.size() == 1);
        CHECK(query["verbose"] == "");
    }
    {
        // Empty string → no segments, no query
        std::vector<std::string> seg;
        std::unordered_map<std::string, std::string> query;
        ParseUrlTarget("", seg, query);
        CHECK(seg.empty());
        CHECK(query.empty());
    }
    {
        // Root "/" → no segments
        std::vector<std::string> seg;
        std::unordered_map<std::string, std::string> query;
        ParseUrlTarget("/", seg, query);
        CHECK(seg.empty());
    }

    //-------------------------------------------
    // RouteRestRequest — happy paths
    //-------------------------------------------

    {
        // GET /typeHierarchy
        auto r = Route(bhttp::verb::get, "/typeHierarchy");
        CHECK(r.method == Methods::GetTypeHierarchy);
        CHECK(r.connectionId.empty());
    }
    {
        // GET /connections/c/ping
        auto r = Route(bhttp::verb::get, "/connections/myConn/ping");
        CHECK(r.method == Methods::Ping);
        CHECK(r.connectionId == "myConn");
    }
    {
        // GET /connections/c/isOpen
        auto r = Route(bhttp::verb::get, "/connections/myConn/isOpen");
        CHECK(r.method == Methods::IsOpen);
        CHECK(r.connectionId == "myConn");
    }
    {
        // GET /connections/c/entities/T/123  → readEntity
        auto r = Route(bhttp::verb::get, "/connections/c/entities/Safir.Dob.Entity/42");
        CHECK(r.method == Methods::ReadEntity);
        CHECK(r.typeIdStr == "Safir.Dob.Entity");
        CHECK(r.instanceIdStr == "42");
    }
    {
        // PUT /connections/c/entities/T/123  → setEntity
        auto r = Route(bhttp::verb::put, "/connections/c/entities/Safir.Dob.Entity/42");
        CHECK(r.method == Methods::SetEntity);
        CHECK(r.instanceIdStr == "42");
    }
    {
        // PATCH /connections/c/entities/T/123  → setEntityChanges
        auto r = Route(bhttp::verb::patch, "/connections/c/entities/Safir.Dob.Entity/42");
        CHECK(r.method == Methods::SetEntityChanges);
    }
    {
        // DELETE /connections/c/entities/T/123  → deleteEntity
        auto r = Route(bhttp::verb::delete_, "/connections/c/entities/Safir.Dob.Entity/42");
        CHECK(r.method == Methods::DeleteEntity);
        CHECK(r.instanceIdStr == "42");
    }
    {
        // DELETE /connections/c/entities/T  → deleteAllInstances
        auto r = Route(bhttp::verb::delete_, "/connections/c/entities/Safir.Dob.Entity");
        CHECK(r.method == Methods::DeleteAllInstances);
        CHECK(r.instanceIdStr.empty());
    }
    {
        // GET /connections/c/entities/T/instances  → getAllInstanceIds
        auto r = Route(bhttp::verb::get, "/connections/c/entities/Safir.Dob.Entity/instances");
        CHECK(r.method == Methods::GetAllInstanceIds);
    }
    {
        // GET /connections/c/entities/T/instanceIdPolicy
        auto r = Route(bhttp::verb::get, "/connections/c/entities/Safir.Dob.Entity/instanceIdPolicy");
        CHECK(r.method == Methods::GetInstanceIdPolicy);
    }
    {
        // GET /connections/c/entities/T/count
        auto r = Route(bhttp::verb::get, "/connections/c/entities/Safir.Dob.Entity/count");
        CHECK(r.method == Methods::GetNumberOfInstances);
    }
    {
        // GET /connections/c/entities/T/42/isCreated  (instanceId in path)
        auto r = Route(bhttp::verb::get, "/connections/c/entities/Safir.Dob.Entity/42/isCreated");
        CHECK(r.method == Methods::IsCreated);
        CHECK(r.typeIdStr == "Safir.Dob.Entity");
        CHECK(r.instanceIdStr == "42");
    }
    {
        // PUT /connections/c/handlers/entities/T  → registerEntityHandler
        auto r = Route(bhttp::verb::put, "/connections/c/handlers/entities/Safir.Dob.Entity");
        CHECK(r.method == Methods::RegisterEntityHandler);
        CHECK(r.typeIdStr == "Safir.Dob.Entity");
    }
    {
        // PUT /connections/c/handlers/services/T  → registerServiceHandler
        auto r = Route(bhttp::verb::put, "/connections/c/handlers/services/Safir.Dob.Service");
        CHECK(r.method == Methods::RegisterServiceHandler);
    }
    {
        // DELETE /connections/c/handlers/T  → unregisterHandler
        auto r = Route(bhttp::verb::delete_, "/connections/c/handlers/Safir.Dob.Entity");
        CHECK(r.method == Methods::UnregisterHandler);
    }
    {
        // PUT /connections/c/subscriptions/entities/T  → subscribeEntity
        auto r = Route(bhttp::verb::put, "/connections/c/subscriptions/entities/Safir.Dob.Entity");
        CHECK(r.method == Methods::SubscribeEntity);
    }
    {
        // DELETE /connections/c/subscriptions/entities/T  → unsubscribeEntity
        auto r = Route(bhttp::verb::delete_, "/connections/c/subscriptions/entities/Safir.Dob.Entity");
        CHECK(r.method == Methods::UnsubscribeEntity);
    }
    {
        // PUT /connections/c/subscriptions/messages/T  → subscribeMessage
        auto r = Route(bhttp::verb::put, "/connections/c/subscriptions/messages/Safir.Dob.Message");
        CHECK(r.method == Methods::SubscribeMessage);
    }
    {
        // DELETE /connections/c/subscriptions/messages/T  → unsubscribeMessage
        auto r = Route(bhttp::verb::delete_, "/connections/c/subscriptions/messages/Safir.Dob.Message");
        CHECK(r.method == Methods::UnsubscribeMessage);
    }
    {
        // PUT /connections/c/subscriptions/registrations/T  → subscribeRegistration
        auto r = Route(bhttp::verb::put, "/connections/c/subscriptions/registrations/Safir.Dob.Entity");
        CHECK(r.method == Methods::SubscribeRegistration);
    }
    {
        // DELETE /connections/c/subscriptions/registrations/T  → unsubscribeRegistration
        auto r = Route(bhttp::verb::delete_, "/connections/c/subscriptions/registrations/Safir.Dob.Entity");
        CHECK(r.method == Methods::UnsubscribeRegistration);
    }
    {
        // POST /connections/c/messages/T  → sendMessage
        auto r = Route(bhttp::verb::post, "/connections/c/messages/Safir.Dob.Message");
        CHECK(r.method == Methods::SendMessage);
    }
    {
        // POST /connections/c/requests/entities/T  → createRequest
        auto r = Route(bhttp::verb::post, "/connections/c/requests/entities/Safir.Dob.Entity");
        CHECK(r.method == Methods::CreateRequest);
    }
    {
        // POST /connections/c/requests/entities/T/42/update  → updateRequest
        auto r = Route(bhttp::verb::post, "/connections/c/requests/entities/Safir.Dob.Entity/42/update");
        CHECK(r.method == Methods::UpdateRequest);
        CHECK(r.instanceIdStr == "42");
    }
    {
        // POST /connections/c/requests/entities/T/42/delete  → deleteRequest
        auto r = Route(bhttp::verb::post, "/connections/c/requests/entities/Safir.Dob.Entity/42/delete");
        CHECK(r.method == Methods::DeleteRequest);
        CHECK(r.instanceIdStr == "42");
    }
    {
        // POST /connections/c/requests/services/T  → serviceRequest
        auto r = Route(bhttp::verb::post, "/connections/c/requests/services/Safir.Dob.Service");
        CHECK(r.method == Methods::ServiceRequest);
    }

    //-------------------------------------------
    // RouteRestRequest — wrong verb → kWrongVerb
    //-------------------------------------------

    {
        // POST /typeHierarchy is not allowed
        auto r = Route(bhttp::verb::post, "/typeHierarchy");
        CHECK(r.method == kWrongVerb);
    }
    {
        // POST /connections/c/ping is not allowed
        auto r = Route(bhttp::verb::post, "/connections/c/ping");
        CHECK(r.method == kWrongVerb);
    }
    {
        // POST /connections/c/isOpen is not allowed
        auto r = Route(bhttp::verb::post, "/connections/c/isOpen");
        CHECK(r.method == kWrongVerb);
    }
    {
        // PUT /connections/c/entities/T  — wrong verb for deleteAllInstances
        auto r = Route(bhttp::verb::put, "/connections/c/entities/Safir.Dob.Entity");
        CHECK(r.method == kWrongVerb);
    }
    {
        // POST /connections/c/entities/T/instances — wrong verb
        auto r = Route(bhttp::verb::post, "/connections/c/entities/Safir.Dob.Entity/instances");
        CHECK(r.method == kWrongVerb);
    }
    {
        // GET /connections/c/handlers/entities/T — wrong verb (should be PUT)
        auto r = Route(bhttp::verb::get, "/connections/c/handlers/entities/Safir.Dob.Entity");
        CHECK(r.method == kWrongVerb);
    }
    {
        // GET /connections/c/subscriptions/entities/T — wrong verb (should be PUT or DELETE)
        auto r = Route(bhttp::verb::get, "/connections/c/subscriptions/entities/Safir.Dob.Entity");
        CHECK(r.method == kWrongVerb);
    }
    {
        // GET /connections/c/messages/T — wrong verb (should be POST)
        auto r = Route(bhttp::verb::get, "/connections/c/messages/Safir.Dob.Message");
        CHECK(r.method == kWrongVerb);
    }
    {
        // GET /connections/c/requests/entities/T — wrong verb (should be POST)
        auto r = Route(bhttp::verb::get, "/connections/c/requests/entities/Safir.Dob.Entity");
        CHECK(r.method == kWrongVerb);
    }

    //-------------------------------------------
    // RouteRestRequest — unknown paths → empty method
    //-------------------------------------------

    {
        // Completely unknown path
        auto r = Route(bhttp::verb::get, "/unknown/path");
        CHECK(r.method.empty());
    }
    {
        // /connections with only one segment after (missing third segment)
        auto r = Route(bhttp::verb::get, "/connections/myConn");
        CHECK(r.method.empty());
    }
    {
        // /connections/{id}/unknownSection
        auto r = Route(bhttp::verb::get, "/connections/c/unknownSection");
        CHECK(r.method.empty());
    }
    {
        // Path starting with something other than "connections" or "typeHierarchy"
        auto r = Route(bhttp::verb::get, "/foobar/c/entities/T");
        CHECK(r.method.empty());
    }
    {
        // requests/entities with unrecognised action
        auto r = Route(bhttp::verb::post, "/connections/c/requests/entities/T/42/invalidAction");
        CHECK(r.method.empty());
    }
    {
        // handlers/unknownKind
        auto r = Route(bhttp::verb::put, "/connections/c/handlers/unknown/T");
        CHECK(r.method.empty());
    }
    {
        // subscriptions/unknownKind
        auto r = Route(bhttp::verb::put, "/connections/c/subscriptions/unknown/T");
        CHECK(r.method.empty());
    }

    //-------------------------------------------
    // RouteRestRequest — /log (connectionless)
    //-------------------------------------------

    {
        // POST /log  → sendSystemLog (no connectionId)
        auto r = Route(bhttp::verb::post, "/log");
        CHECK(r.method == Methods::SendSystemLog);
        CHECK(r.connectionId.empty());
    }
    {
        // GET /log  → wrong verb
        auto r = Route(bhttp::verb::get, "/log");
        CHECK(r.method == kWrongVerb);
    }
    {
        // PUT /log  → wrong verb
        auto r = Route(bhttp::verb::put, "/log");
        CHECK(r.method == kWrongVerb);
    }
}
