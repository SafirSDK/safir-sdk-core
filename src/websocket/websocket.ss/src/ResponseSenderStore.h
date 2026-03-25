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

#include <vector>
#include <Safir/Dob/ResponseSender.h>
#include <Safir/Dob/Typesystem/HandlerId.h>

namespace sd = Safir::Dob;
namespace ts = Safir::Dob::Typesystem;

class ResponseSenderStore
{
public:

    ResponseSenderStore(size_t requestInQueueSize)
        :m_queueSize(requestInQueueSize)
        ,m_id(0)
    {
    }

    std::uint64_t Add(const sd::ResponseSenderPtr& responseSender, ts::TypeId typeId, const ts::HandlerId& handlerId)
    {
        //if we have to many response senders, it must mean that the remote client has not responded.
        //in that case dose has already sent a timeout-response so we can safely remove the ResponseSender
        //to make room for new once.
        if (m_store.size()>=m_queueSize)
        {
            auto remove=static_cast<size_t>(1+m_store.size()-m_queueSize);
            m_store.erase(m_store.begin(), m_store.begin()+remove);
        }

        m_store.push_back({++m_id, responseSender, typeId, handlerId});

        return m_id;
    }

    sd::ResponseSenderPtr Get(std::uint64_t reqId)
    {
        for (auto it=m_store.begin(); it!=m_store.end(); ++it)
        {
            if (it->id==reqId)
            {
                auto responseSender=it->sender;
                m_store.erase(it);
                return responseSender;
            }
        }

        //responseSender not found, return nullPtr
        return nullptr;
    }

    // Discard all pending response senders for the given handler before unregistering it.
    // Calling Send() on a response sender after its handler has been unregistered crashes
    // the DOB, so we must discard any that the client hasn't responded to yet.
    void DiscardForHandler(ts::TypeId typeId, const ts::HandlerId& handlerId)
    {
        for (auto it = m_store.begin(); it != m_store.end(); )
        {
            if (it->typeId == typeId && it->handlerId == handlerId)
            {
                if (!it->sender->IsDone())
                    it->sender->Discard();
                it = m_store.erase(it);
            }
            else
            {
                ++it;
            }
        }
    }

    // Discard all pending response senders before closing the connection.
    void DiscardAll()
    {
        for (auto& entry : m_store)
        {
            if (!entry.sender->IsDone())
                entry.sender->Discard();
        }
        m_store.clear();
    }

    size_t Count() const {return m_store.size();}

private:
    struct Entry
    {
        std::uint64_t id;
        sd::ResponseSenderPtr sender;
        ts::TypeId typeId;
        ts::HandlerId handlerId;
    };

    size_t m_queueSize;
    std::uint64_t m_id;
    std::vector<Entry> m_store;
};
