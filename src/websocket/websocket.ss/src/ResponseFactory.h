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

#include <boost/optional.hpp>
#include <Safir/Dob/Typesystem/Utilities.h>
#include <Safir/Dob/Typesystem/Serialization.h>
#include <Safir/Websocket/Receive.h>

namespace ts = Safir::Dob::Typesystem;
namespace ws = Safir::Websocket;

class ResponseFactory
{
public:
    static std::string Error(const std::wstring& reason, const std::wstring& msg, boost::optional<int> requestId = boost::optional<int>())
    {
        ws::ReceivePtr r=ws::Receive::CreateWithReason(reason);
        r->Error()=msg;
        if (requestId)
        {
            r->Id()=requestId.get();
        }

        return ToString(r);
    }

    static std::string Success(const std::wstring& reason, boost::optional<int> requestId = boost::optional<int>())
    {
        ws::ReceivePtr r=ws::Receive::CreateWithReason(reason);
        if (requestId)
        {
            r->Id()=requestId.get();
        }

        return ToString(r);
    }

private:
    static std::string ToString(const ws::ReceivePtr& r)
    {
        return ts::Utilities::ToUtf8(ts::Serialization::ToJson(r));
    }

};
