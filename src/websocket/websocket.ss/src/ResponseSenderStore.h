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

#include <vector>
#include <Safir/Dob/ResponseSender.h>

namespace sd = Safir::Dob;

class ResponseSenderStore
{
public:

    ResponseSenderStore(size_t requestInQueueSize)
        :m_queueSize(requestInQueueSize)
        ,m_id(0)
    {
    }

    boost::uint64_t Add(const sd::ResponseSenderPtr& responseSender)
    {
        //if we have to many response senders, it must mean that the remote client has not responded.
        //in that case dose has already sent a timeout-response so we can safely remove the ResponseSender
        //to make room for new once.
        if (m_store.size()>=m_queueSize)
        {
            auto remove=static_cast<size_t>(1+m_store.size()-m_queueSize);
            m_store.erase(m_store.begin(), m_store.begin()+remove);
        }

        m_store.emplace_back(std::make_pair(++m_id, responseSender));

        return m_id;
    }

    sd::ResponseSenderPtr Get(boost::uint64_t reqId)
    {
        for (auto it=m_store.begin(); it!=m_store.end(); ++it)
        {
            if (it->first==reqId)
            {
                auto responseSender=it->second;
                m_store.erase(it);
                return responseSender;
            }
        }

        //responseSender not found, return nullPtr
        std::shared_ptr<sd::ResponseSender> nullSender;
        return nullSender;
    }

    size_t Count() const {return m_store.size();}

private:
    size_t m_queueSize;
    boost::uint64_t m_id;
    std::vector< std::pair<boost::uint64_t, sd::ResponseSenderPtr> >  m_store;
};
