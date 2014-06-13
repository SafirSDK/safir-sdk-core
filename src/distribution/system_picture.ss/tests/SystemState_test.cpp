/******************************************************************************
*
* Copyright Saab AB, 2014 (http://safir.sourceforge.net)
*
* Created by: Lars Hagström / lars.hagstrom@consoden.se
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
#include <boost/test/minimal.hpp>
#include <Safir/Dob/Internal/SystemState.h>
#include "../src/MessageWrapperCreators.h"
#include "SystemStateMessage.pb.h"
#include <Safir/Utilities/Internal/MakeUnique.h>
#include <boost/lexical_cast.hpp>

using namespace Safir::Dob::Internal::SP;


std::unique_ptr<SystemStateMessage> GetProtobuf(bool empty)
{
    auto msg = Safir::make_unique<SystemStateMessage>();
    if (empty)
    {
        return std::move(msg);
    }
    
    msg->set_elected_id(100);

    for (int i = 0; i < 5; ++i)
    {
        const auto iAsStr = boost::lexical_cast<std::string>(i);
        auto node = msg->add_node_info();

        node->set_name(iAsStr);
        node->set_id(i);
        node->set_node_type_id(i + 100);
        node->set_control_address(iAsStr + ":fobar!");
        node->set_data_address(iAsStr + ":flopp");
        node->set_is_dead(i%2==0);

    }
    return std::move(msg);
    
}

int test_main(int, char**)
{
    {
        const auto r = SystemStateCreator::Create(GetProtobuf(true));
        BOOST_CHECK(r.Size() == 0);
    }

    {
        const auto r = SystemStateCreator::Create(GetProtobuf(false));
        BOOST_CHECK(r.ElectedId() == 100);

        BOOST_CHECK(r.Size() == 5);

        for (int i = 0; i < 5; ++i)
        {
            const auto iAsStr = boost::lexical_cast<std::string>(i);

            BOOST_CHECK(r.Name(i) == iAsStr);

            BOOST_CHECK(r.Id(i) == i);
            BOOST_CHECK(r.NodeTypeId(i) == i + 100);
            BOOST_CHECK(r.ControlAddress(i) == iAsStr + ":fobar!");
            BOOST_CHECK(r.DataAddress(i) == iAsStr + ":flopp");
            BOOST_CHECK(r.IsDead(i) == (i%2==0));
        }
    }

    return 0;
}


