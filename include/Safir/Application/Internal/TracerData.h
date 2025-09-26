/******************************************************************************
*
* Copyright Saab AB, 2025 (http://safirsdkcore.com)
*
* Created by: Lars Hagström / lars@foldspace.nu
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

#include <cstdint>
#include <type_traits>

/*
 * TracerDataHeader represents a single tracer message transported between
 * processes.  The header is kept in plain POD members to simplify
 * zero-copy serialisation:
 *
 *   - incarnationId     : unique incarnation identifier of the system the sender belongs to
 *   - senderId          : unique identifier for the sender. Probably just a random number
 *   - sequenceNumber    : monotonically increasing sequence number for
 *                         detecting lost or out-of-order datagrams
 *   - payloadLength     : number of bytes in the user payload that follows the two name strings
 *   - timestampUsec     : wall-clock time when the datagram was produced,
 *                         expressed as micro-seconds since the Unix epoch
 *                         (00:00:00 UTC on 1 January 1970)
 *   - programNameLength : length in bytes (UTF-8) of the program name string, truncated if needed to fit in this field
 *   - nodeNameLength    : length in bytes (UTF-8) of the node name string, truncated if needed to fit in this field
 *
 * Only the fixed-size header is defined here to guarantee the type is a
 * Plain Old Data (POD) object suitable for transmission with e.g.
 * boost::asio::buffer().  The payload bytes must be appended directly
 * after the header when sending over the wire.
 */
#pragma pack(push, 4)
struct TracerDataHeader
{
    std::int64_t  incarnationId;   // unique incarnation identifier of the system the sender belongs to
    std::int64_t  senderId;        // unique identifier for the sender. Probably just a random number
    std::uint32_t sequenceNumber;  // monotonically increasing per-datagram sequence
    std::uint32_t payloadLength;     // size of payload that follows this header
    std::uint64_t timestampUsec;     // micro-seconds since Unix epoch (UTC)
    std::uint8_t  programNameLength; // bytes of UTF-8 program name string, truncated if needed to fit in this field
    std::uint8_t  nodeNameLength;    // bytes of UTF-8 node name string, truncated if needed to fit in this field
    std::uint16_t padding;
};
#pragma pack(pop)

static_assert(std::is_standard_layout<TracerDataHeader>::value &&
              std::is_trivially_copyable<TracerDataHeader>::value,
              "TracerDataHeader must remain POD for UDP transport.");

static_assert(sizeof(TracerDataHeader) == 36,
              "Unexpected TracerDataHeader size; check packing/alignment.");

/*
 * Maximum number of bytes that fit into a single tracer UDP datagram.
 * Must match the value used by the sender implementation.
 */
constexpr std::size_t TracerMaxPacketSize = 1472; //bytes

/*
 * The fixed header plus the two variable-length name fields (each
 * truncated to 255 bytes) must always be smaller than the maximum packet
 * size so that there is room for at least one payload byte.
 */
static_assert(sizeof(TracerDataHeader) + 255 + 255 <= TracerMaxPacketSize,
              "TracerDataHeader plus maximum name fields must not exceed maximum packet size.");
