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
#include "../src/RawHandler.h"

#define BOOST_TEST_MODULE RawHandlerTest
#include <boost/test/unit_test.hpp>

class Communication
{
public:

   //Callbacks functions used in Communications public interface.
    typedef std::function<void(const std::string& name, int64_t nodeId, int64_t nodeTypeId, const std::string& controlAddress, const std::string& dataAddress)> NewNode;
    typedef std::function<void(int64_t fromNodeId)> GotReceiveFrom;
    typedef std::function<void(int64_t toNodeId)> RetransmitTo;

    void SetNewNodeCallback(const NewNode& /*callback*/)
    {

    }

    void SetGotReceiveFromCallback(const GotReceiveFrom& /*callback*/)
    {

    }

    void SetRetransmitToCallback(const RetransmitTo& /*callback*/)
    {

    }

    void IncludeNode(int64_t /*nodeId*/)
    {

    }

    void ExcludeNode(int64_t /*nodeId*/)
    {

    }

};

BOOST_AUTO_TEST_CASE( nothing )
{
    using namespace Safir::Dob::Internal::SP;

    Communication comm;
    boost::asio::io_service ioService;

    RawHandlerBasic<::Communication> rh(ioService,comm,"plopp",10,100,"asdfasdf","qwerty",{});
}


