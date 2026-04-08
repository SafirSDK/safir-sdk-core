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
#pragma once

#include <string>
#include <vector>
#include <unordered_map>
#include <sstream>
#include <boost/beast/http/verb.hpp>
#include "Methods.h"

// ---------------------------------------------------------------------------
// ParseBoolParam
// ---------------------------------------------------------------------------

inline bool ParseBoolParam(const std::unordered_map<std::string, std::string>& query,
                            const std::string& key,
                            bool defaultVal)
{
    auto it = query.find(key);
    if (it == query.end()) return defaultVal;
    return it->second == "true" || it->second == "1";
}

// ---------------------------------------------------------------------------
// URL parser
// ---------------------------------------------------------------------------

inline void ParseUrlTarget(const std::string& target,
                            std::vector<std::string>& segments,
                            std::unordered_map<std::string, std::string>& query)
{
    segments.clear();
    query.clear();

    // Split path and query string
    std::string path = target;
    const auto qpos = target.find('?');
    if (qpos != std::string::npos)
    {
        path = target.substr(0, qpos);
        const std::string qs = target.substr(qpos + 1);
        std::istringstream qss(qs);
        std::string token;
        while (std::getline(qss, token, '&'))
        {
            const auto eq = token.find('=');
            if (eq != std::string::npos)
                query[token.substr(0, eq)] = token.substr(eq + 1);
            else
                query[token] = "";
        }
    }

    // Split path on '/'
    std::istringstream pss(path);
    std::string seg;
    while (std::getline(pss, seg, '/'))
    {
        if (!seg.empty())
            segments.push_back(seg);
    }
}

// ---------------------------------------------------------------------------
// Route struct and router
// ---------------------------------------------------------------------------

// Sentinel used when the path matches but the verb is wrong
inline const std::string kWrongVerb = "__wrong_verb__";

struct RestRoute
{
    std::string method;       // Methods:: constant, empty = not found, kWrongVerb = 405
    std::string connectionId;
    std::string typeIdStr;
    std::string instanceIdStr;
};

inline RestRoute RouteRestRequest(boost::beast::http::verb verb,
                                   const std::vector<std::string>& seg,
                                   const std::unordered_map<std::string, std::string>& /*query*/)
{
    const bool isGet    = (verb == boost::beast::http::verb::get);
    const bool isPut    = (verb == boost::beast::http::verb::put);
    const bool isPatch  = (verb == boost::beast::http::verb::patch);
    const bool isDelete = (verb == boost::beast::http::verb::delete_);
    const bool isPost   = (verb == boost::beast::http::verb::post);

    // GET /typeHierarchy
    if (seg.size() == 1 && seg[0] == "typeHierarchy")
    {
        if (!isGet) return {kWrongVerb, {}, {}, {}};
        return {Methods::GetTypeHierarchy, {}, {}, {}};
    }

    // GET /connections  (list all connection names, no connection id)
    if (seg.size() == 1 && seg[0] == "connections")
    {
        if (!isGet) return {kWrongVerb, {}, {}, {}};
        return {Methods::GetConnections, {}, {}, {}};
    }

    // All remaining routes start with /connections/{id}/...
    if (seg.size() < 3 || seg[0] != "connections")
        return {};

    const std::string& connId = seg[1];

    // /connections/{id}/ping
    if (seg.size() == 3 && seg[2] == "ping")
    {
        if (!isGet) return {kWrongVerb, {}, {}, {}};
        return {Methods::Ping, connId, {}, {}};
    }

    // /connections/{id}/isOpen
    if (seg.size() == 3 && seg[2] == "isOpen")
    {
        if (!isGet) return {kWrongVerb, {}, {}, {}};
        return {Methods::IsOpen, connId, {}, {}};
    }

    // /connections/{id}/entities/...
    if (seg.size() >= 4 && seg[2] == "entities")
    {
        const std::string& typeId = seg[3];

        if (seg.size() == 4)
        {
            // DELETE /connections/{id}/entities/{typeId}  → deleteAllInstances
            if (!isDelete) return {kWrongVerb, {}, {}, {}};
            return {Methods::DeleteAllInstances, connId, typeId, {}};
        }

        if (seg.size() == 5)
        {
            const std::string& fifth = seg[4];

            if (fifth == "instances")
            {
                if (!isGet) return {kWrongVerb, {}, {}, {}};
                return {Methods::GetAllInstanceIds, connId, typeId, {}};
            }
            if (fifth == "instanceIdPolicy")
            {
                if (!isGet) return {kWrongVerb, {}, {}, {}};
                return {Methods::GetInstanceIdPolicy, connId, typeId, {}};
            }
            if (fifth == "count")
            {
                if (!isGet) return {kWrongVerb, {}, {}, {}};
                return {Methods::GetNumberOfInstances, connId, typeId, {}};
            }
            if (fifth == "isCreated")
            {
                // /connections/{id}/entities/{typeId}/isCreated — instanceId from query param
                if (!isGet) return {kWrongVerb, {}, {}, {}};
                return {Methods::IsCreated, connId, typeId, {}};
            }

            // fifth is instanceId
            const std::string& instanceId = fifth;
            if (isGet)    return {Methods::ReadEntity,       connId, typeId, instanceId};
            if (isPut)    return {Methods::SetEntity,        connId, typeId, instanceId};
            if (isPatch)  return {Methods::SetEntityChanges, connId, typeId, instanceId};
            if (isDelete) return {Methods::DeleteEntity,     connId, typeId, instanceId};
            return {kWrongVerb, {}, {}, {}};
        }

        // /connections/{id}/entities/{typeId}/{instanceId}/isCreated  (GET)
        if (seg.size() == 6 && seg[5] == "isCreated")
        {
            if (!isGet) return {kWrongVerb, {}, {}, {}};
            return {Methods::IsCreated, connId, typeId, seg[4]};
        }

        return {};
    }

    // /connections/{id}/handlers/...
    if (seg.size() >= 4 && seg[2] == "handlers")
    {
        if (seg.size() == 4)
        {
            // DELETE /connections/{id}/handlers/{typeId}  → unregisterHandler
            if (!isDelete) return {kWrongVerb, {}, {}, {}};
            return {Methods::UnregisterHandler, connId, seg[3], {}};
        }

        if (seg.size() == 5)
        {
            const std::string& kind   = seg[3]; // "entities" or "services"
            const std::string& typeId = seg[4];

            if (kind == "entities")
            {
                if (!isPut) return {kWrongVerb, {}, {}, {}};
                return {Methods::RegisterEntityHandler, connId, typeId, {}};
            }
            if (kind == "services")
            {
                if (!isPut) return {kWrongVerb, {}, {}, {}};
                return {Methods::RegisterServiceHandler, connId, typeId, {}};
            }
        }

        return {};
    }

    // /connections/{id}/subscriptions/...
    if (seg.size() == 5 && seg[2] == "subscriptions")
    {
        const std::string& kind   = seg[3];
        const std::string& typeId = seg[4];

        if (kind == "entities")
        {
            if (isPut)    return {Methods::SubscribeEntity,   connId, typeId, {}};
            if (isDelete) return {Methods::UnsubscribeEntity, connId, typeId, {}};
            return {kWrongVerb, {}, {}, {}};
        }
        if (kind == "messages")
        {
            if (isPut)    return {Methods::SubscribeMessage,   connId, typeId, {}};
            if (isDelete) return {Methods::UnsubscribeMessage, connId, typeId, {}};
            return {kWrongVerb, {}, {}, {}};
        }
        if (kind == "registrations")
        {
            if (isPut)    return {Methods::SubscribeRegistration,   connId, typeId, {}};
            if (isDelete) return {Methods::UnsubscribeRegistration, connId, typeId, {}};
            return {kWrongVerb, {}, {}, {}};
        }

        return {};
    }

    // /connections/{id}/messages/{typeId}  → sendMessage (POST)
    if (seg.size() == 4 && seg[2] == "messages")
    {
        if (!isPost) return {kWrongVerb, {}, {}, {}};
        return {Methods::SendMessage, connId, seg[3], {}};
    }

    // /connections/{id}/requests/...
    if (seg.size() >= 5 && seg[2] == "requests")
    {
        const std::string& kind   = seg[3];
        const std::string& typeId = seg[4];

        if (kind == "entities")
        {
            if (seg.size() == 5)
            {
                // POST /connections/{id}/requests/entities/{typeId}  → createRequest
                if (!isPost) return {kWrongVerb, {}, {}, {}};
                return {Methods::CreateRequest, connId, typeId, {}};
            }
            if (seg.size() == 7)
            {
                const std::string& instanceId = seg[5];
                const std::string& action     = seg[6];
                if (!isPost) return {kWrongVerb, {}, {}, {}};
                if (action == "update") return {Methods::UpdateRequest, connId, typeId, instanceId};
                if (action == "delete") return {Methods::DeleteRequest, connId, typeId, instanceId};
            }
        }
        if (kind == "services" && seg.size() == 5)
        {
            if (!isPost) return {kWrongVerb, {}, {}, {}};
            return {Methods::ServiceRequest, connId, typeId, {}};
        }

        return {};
    }

    return {};
}
