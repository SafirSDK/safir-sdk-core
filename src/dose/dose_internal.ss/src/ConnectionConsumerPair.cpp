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
#include <Safir/Dob/Internal/ConnectionConsumerPair.h>
#include <Safir/Dob/Internal/Connection.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    bool ConnectionConsumerPair::operator<(const ConnectionConsumerPair& other) const
    {
       ENSURE(connection != NULL && other.connection != NULL, << "ConnectionConsumerPair::operator< called with a NULL connection pointer!");

       if (connection->Id() == other.connection->Id())
       {
           return consumer < other.consumer;
       }
       else
       {
           return connection->Id() < other.connection->Id();
       }
    }

    bool ConnectionConsumerPair::operator==(const ConnectionConsumerPair& other) const
    {
        ENSURE(connection != NULL && other.connection != NULL, << "ConnectionConsumerPair::operator== called with a NULL connection pointer!");

        return connection->Id() == other.connection->Id() && consumer == other.consumer;
    }

    ConnectionConsumerPair& ConnectionConsumerPair::operator=(const ConnectionConsumerPair& other)
    {
        connection = other.connection;
        consumer = other.consumer;
        return *this;
    }
}
}
}

