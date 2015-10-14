/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safirsdkcore.com)
*
* Created by: Stefan Lindström / stsyli
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
#ifndef __FOREACH_DATA_H
#define __FOREACH_DATA_H

#include <Safir/Application/Tracer.h>
#include <Safir/Utilities/ForEach/ResponseType.h>
#include <Safir/Dob/Consumer.h>
#include <Safir/Dob/Connection.h>

#include <list>
#include <map>


namespace Safir
{
    namespace Utilities
    {
        namespace ForEach
        {
            class RequestSpecificData;
            typedef boost::shared_ptr<RequestSpecificData> RequestSpecificDataPtr;

            struct TransactionTableEntry 
            {
                Safir::Dob::Typesystem::EntityId entityId;
                Safir::Dob::RequestId requestId;
                Safir::Dob::ResponsePtr responsePtr;
            };

            enum RequestType
            {
                Delete,
                Update
            };

            class RequestSpecificData 
            {
            public:
                RequestSpecificData()
                {
                    m_numberOfErrorResponses = 0;
                    m_numberOfObjects = 0;
                    m_numberOfResponses = 0;
                    m_numberOfSuccessResponses = 0;
                    m_currentTransaction = 0;
                }

                ~RequestSpecificData() 
                {
                    // empty
                }


                const Safir::Dob::Typesystem::Int32 & NumberOfErrorResponses() const { return m_numberOfErrorResponses; }
                Safir::Dob::Typesystem::Int32 & NumberOfErrorResponses() { return m_numberOfErrorResponses; }

                const Safir::Dob::Typesystem::Int32 & NumberOfObjects() const { return m_numberOfObjects; }
                Safir::Dob::Typesystem::Int32 & NumberOfObjects() { return m_numberOfObjects; }

                const Safir::Dob::Typesystem::Int32 & NumberOfResponses() const { return m_numberOfResponses; }
                Safir::Dob::Typesystem::Int32 & NumberOfResponses() { return m_numberOfResponses; }

                const Safir::Dob::Typesystem::Int32 & NumberOfSuccessResponses() const { return m_numberOfSuccessResponses; }
                Safir::Dob::Typesystem::Int32 & NumberOfSuccessResponses() { return m_numberOfSuccessResponses; }

                const Safir::Utilities::ForEach::ResponseType::Enumeration & ResponseType() const { return m_responseType; }
                Safir::Utilities::ForEach::ResponseType::Enumeration & ResponseType() { return m_responseType; }

                const std::vector<TransactionTableEntry> & Transactions() const { return m_transactions; }
                std::vector<TransactionTableEntry> & Transactions() { return m_transactions; }

                const TransactionTableEntry * CurrentTransaction() const { return m_currentTransaction; }
                TransactionTableEntry * CurrentTransaction() { return m_currentTransaction; }

                void SetCurrentTransaction(TransactionTableEntry *entry) { m_currentTransaction = entry; }
                TransactionTableEntry *GetCurrentTransaction(void) { return m_currentTransaction; }


                const Safir::Dob::EntityPtr & TemplateEntityRequest() const { return m_templateEntityRequest; }
                Safir::Dob::EntityPtr & TemplateEntityRequest() { return m_templateEntityRequest; }


                const RequestType & GetRequestType() const { return m_requestType; }
                RequestType & GetRequestType() { return m_requestType; }

            private:
                Safir::Dob::Typesystem::Int32 m_numberOfSuccessResponses;
                Safir::Dob::Typesystem::Int32 m_numberOfErrorResponses;
                Safir::Dob::Typesystem::Int32 m_numberOfResponses;
                Safir::Dob::Typesystem::Int32 m_numberOfObjects;

                Safir::Utilities::ForEach::ResponseType::Enumeration m_responseType;
                std::vector<TransactionTableEntry> m_transactions;
                //TransactionTableEntry * lood = &m_transactions[0];
                TransactionTableEntry *m_currentTransaction;

                /* templateObject */
                Safir::Dob::EntityPtr m_templateEntityRequest;
                enum RequestType m_requestType;

            };
        }
    }
}
#endif
