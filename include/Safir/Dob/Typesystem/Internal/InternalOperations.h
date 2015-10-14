/******************************************************************************
*
* Copyright Saab AB, 2004-2015 (http://safirsdkcore.com)
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

#ifndef _dots_dob_internal_h
#define _dots_dob_internal_h

#include <Safir/Dob/Typesystem/Defs.h>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    /**
     * Generate a 64 bit hash from a string.
     */
    DOTS_CPP_API Int64 Generate64BitHash(const std::wstring & str);

    /**
     * Generate a random 64 bit number.
     */
    DOTS_CPP_API Int64 GenerateRandom64Bit();

    /**
     * Get the full path to the dou file that the type id represents
     *
     * Note that this function looks at the disk every time it is called. No caching
     * is performed at all. Not meant to be used in "real" code, but useful for debugging
     * tools, such as sate or dobexplorer.
     * 
     * @param typeId Type id
     * @returns The full path to the dou file
     */
    DOTS_CPP_API std::wstring GetDouFilePath(const Dob::Typesystem::TypeId typeId);
        

    //These constants used to be calculated from the strings, but that caused some
    //strange elaboration errors, so they were changed to hard-coded constants.
    //The tests in dots_cpp.ss/tests/constants_test.cpp check that they 
    //correspond to the correct strings.

    const Int64 DEFAULT_CHANNEL_ID = 3313918482685577033LL; //Internal::Generate64BitHash(L"DEFAULT_CHANNEL");
    const Int64 DEFAULT_HANDLER_ID = -6778878277529052275LL; //Internal::Generate64BitHash(L"DEFAULT_HANDLER");

    const Int64 ALL_CHANNELS_ID = -3399801522715850860LL; //Internal::Generate64BitHash(L"ALL_CHANNELS");
    const Int64 ALL_HANDLERS_ID = 192125831057403942LL; //Internal::Generate64BitHash(L"ALL_HANDLERS");
}
}
}
}

#endif
