/******************************************************************************
*
* Copyright Saab AB, 2014 (http://safirsdkcore.com)
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
#include "../../src/RawChanges.h"

#define BOOST_TEST_MODULE RawChangesTest
#include <boost/test/unit_test.hpp>
using namespace Safir::Dob::Internal::SP;

BOOST_AUTO_TEST_CASE( no_flags )
{
    RawChanges flags(0);
    BOOST_CHECK(!flags.NewRemoteStatistics());
    BOOST_CHECK(!flags.NewDataChannelStatistics());
    BOOST_CHECK(!flags.NodesChanged());
    BOOST_CHECK(!flags.MetadataChanged());
    BOOST_CHECK(!flags.BadElectionIdDetected());
}


BOOST_AUTO_TEST_CASE( new_remote )
{
    RawChanges flags(RawChanges::NEW_REMOTE_STATISTICS);
    BOOST_CHECK(flags.NewRemoteStatistics());
    BOOST_CHECK(!flags.NewDataChannelStatistics());
    BOOST_CHECK(!flags.NodesChanged());
    BOOST_CHECK(!flags.MetadataChanged());
    BOOST_CHECK(!flags.BadElectionIdDetected());
}


BOOST_AUTO_TEST_CASE( new_data )
{
    RawChanges flags(RawChanges::NEW_DATA_CHANNEL_STATISTICS);
    BOOST_CHECK(!flags.NewRemoteStatistics());
    BOOST_CHECK(flags.NewDataChannelStatistics());
    BOOST_CHECK(!flags.NodesChanged());
    BOOST_CHECK(!flags.MetadataChanged());
    BOOST_CHECK(!flags.BadElectionIdDetected());
}

BOOST_AUTO_TEST_CASE( nodes_changed )
{
    RawChanges flags(RawChanges::NODES_CHANGED);
    BOOST_CHECK(!flags.NewRemoteStatistics());
    BOOST_CHECK(!flags.NewDataChannelStatistics());
    BOOST_CHECK(flags.NodesChanged());
    BOOST_CHECK(!flags.MetadataChanged());
}


BOOST_AUTO_TEST_CASE( election_id_changed )
{
    RawChanges flags(RawChanges::METADATA_CHANGED);
    BOOST_CHECK(!flags.NewRemoteStatistics());
    BOOST_CHECK(!flags.NewDataChannelStatistics());
    BOOST_CHECK(!flags.NodesChanged());
    BOOST_CHECK(flags.MetadataChanged());
    BOOST_CHECK(!flags.BadElectionIdDetected());
}

BOOST_AUTO_TEST_CASE( bad_election_id_detected )
{
    RawChanges flags(RawChanges::BAD_ELECTION_ID_DETECTED);
    BOOST_CHECK(!flags.NewRemoteStatistics());
    BOOST_CHECK(!flags.NewDataChannelStatistics());
    BOOST_CHECK(!flags.NodesChanged());
    BOOST_CHECK(!flags.MetadataChanged());
    BOOST_CHECK(flags.BadElectionIdDetected());
}


BOOST_AUTO_TEST_CASE( two_set )
{
    RawChanges flags(RawChanges::METADATA_CHANGED | RawChanges::NEW_REMOTE_STATISTICS);
    BOOST_CHECK(flags.NewRemoteStatistics());
    BOOST_CHECK(!flags.NewDataChannelStatistics());
    BOOST_CHECK(!flags.NodesChanged());
    BOOST_CHECK(flags.MetadataChanged());
    BOOST_CHECK(!flags.BadElectionIdDetected());
}

BOOST_AUTO_TEST_CASE( three_set )
{
    RawChanges flags(RawChanges::NEW_REMOTE_STATISTICS |
                     RawChanges::NODES_CHANGED |
                     RawChanges::METADATA_CHANGED);
    BOOST_CHECK(flags.NewRemoteStatistics());
    BOOST_CHECK(!flags.NewDataChannelStatistics());
    BOOST_CHECK(flags.NodesChanged());
    BOOST_CHECK(flags.MetadataChanged());
    BOOST_CHECK(!flags.BadElectionIdDetected());
}

BOOST_AUTO_TEST_CASE( four_set )
{
    RawChanges flags(RawChanges::NEW_REMOTE_STATISTICS |
                     RawChanges::NEW_DATA_CHANNEL_STATISTICS |
                     RawChanges::NODES_CHANGED |
                     RawChanges::METADATA_CHANGED);
    BOOST_CHECK(flags.NewRemoteStatistics());
    BOOST_CHECK(flags.NewDataChannelStatistics());
    BOOST_CHECK(flags.NodesChanged());
    BOOST_CHECK(flags.MetadataChanged());
    BOOST_CHECK(!flags.BadElectionIdDetected());
}

BOOST_AUTO_TEST_CASE( five_set )
{
    RawChanges flags(RawChanges::NEW_REMOTE_STATISTICS |
                     RawChanges::NEW_DATA_CHANNEL_STATISTICS |
                     RawChanges::NODES_CHANGED |
                     RawChanges::METADATA_CHANGED |
                     RawChanges::BAD_ELECTION_ID_DETECTED);
    BOOST_CHECK(flags.NewRemoteStatistics());
    BOOST_CHECK(flags.NewDataChannelStatistics());
    BOOST_CHECK(flags.NodesChanged());
    BOOST_CHECK(flags.MetadataChanged());
    BOOST_CHECK(flags.BadElectionIdDetected());
}
