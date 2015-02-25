/******************************************************************************
*
* Copyright Saab AB, 2008-2013 (http://safir.sourceforge.net)
*
* Created by: Anders Widén / stawi
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

#ifndef _dose_internal_connection_consumer_pair_h
#define _dose_internal_connection_consumer_pair_h

#include <Safir/Dob/Internal/InternalExportDefs.h>
#include <Safir/Dob/Internal/ConsumerId.h>
#include <Safir/Dob/Internal/InternalFwd.h>
#include <Safir/Dob/Internal/InternalDefs.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    struct DOSE_INTERNAL_API ConnectionConsumerPair:
        public SharedMemoryObject
    {
        ConnectionConsumerPair()
            : connection(NULL), consumer(NULL,0) {}

        ConnectionConsumerPair(ConnectionPtr _connection, ConsumerId _consumer)
            : connection(_connection), consumer(_consumer) {}

        ConnectionPtr connection;
        ConsumerId    consumer;

        bool operator<(const ConnectionConsumerPair& other) const;

        bool operator==(const ConnectionConsumerPair& other) const;

        ConnectionConsumerPair& operator=(const ConnectionConsumerPair& other);
    };

}
}
}
#endif

