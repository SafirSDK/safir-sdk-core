/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
* 
* Created by: Joel Ottosson / stjoot
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

#ifndef _dots_base64_conversions_h
#define _dots_base64_conversions_h

#include "dots_internal_defs.h"

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    /**
     * Provides conversions between binary data format and base64 format.
     * Implementation of the PEM base64 conversion (specified in RFC 1421).
     * Uses the 64-character alphabet: ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/
     * See: http://en.wikipedia.org/wiki/Base64
     */
    class Base64Conversions
    {
    public:
        /**
         * Calculate the number of bytes that the base64 representation of
         * number of binary bytes will be.
         *
         * @param binarySourceSize The number of bytes in the binary data.
         * @return The size of the base64 representation in bytes.
         */
        static Int32 CalculateBase64Size(const Int32 binarySourceSize);

        /**
         * Convert binary data to base64 representation.
         */
        static bool ToBase64(char* base64Dest,
                             Int32 destCapacity,
                             const char * const binarySource,
                             Int32 sourceSize,
                             Int32 & resultSize);

        /**
         * Calculate the number of bytes that the binary representation of
         * number of base64 characters will be.
         *
         * @param base64SourceSize The number of bytes in the base64 data.
         * @return The size of the binary representation in bytes.
         */
        static Int32 CalculateBinarySize(const Int32 base64SourceSize);

        /**
         * Convert base64 data to binary.
         */
        static bool ToBinary(char * binaryDest,
                             Int32 destCapacity,
                             const char * const base64Source,
                             Int32 sourceSize,
                             Int32 & resultSize);

    private:
        Base64Conversions(); //Declare but do not define. Creation of this class should be impossible

        static void Bit24ToBase64(char* b64, const char* const bin);
        static void Base64ToBit24(char* bin, const char* const b64);
    };
}
}
}
}

#endif
