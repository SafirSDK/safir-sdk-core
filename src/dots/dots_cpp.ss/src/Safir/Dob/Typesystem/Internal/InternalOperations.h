/******************************************************************************
*
* Copyright Saab AB, 2004-2008 (http://www.safirsdk.com)
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
     * Create an exact copy of a blob.
     *
     * Be aware that you _MUST_ call BlobOperations::Delete on the blob you received when you are done with it.
     * otherwise memory will leak!
     *
     * @param blob [in] - the blob to copy.
     * @return The copied blob.
     */
    DOTS_API char * CreateCopy(char const * const blob);

    /**
     * Delete a blob.
     *
     * Blobs created with CreateCopy must be deleted with this method method
     *
     * @param blob [in,out] - the blob to delete.
     */
    DOTS_API void Delete(char * & blob);

    /**
     * Set all changed flags in the blob.
     *
     * @param blob [in] - The blob to modify.
     * @param changed [in] - The value to set the change flags to.
     */
    DOTS_API void SetChanged(char * const blob, const bool changed);

    /**
     * Set the change flag on a member in blob. 
     *
     * This operation is not recursive (hence the "Here" bit).
     *
     * @param blob [in] - The blob to modify.
     * @param [in] member - The member to be set.
     * @param [in] index - Array index in member to set. Shall be 0 if the member is not an array.
     * @param changed [in] - The value to set the change flag to.
     */
    DOTS_API void SetChangedHere(char* const blob,
                                 const Dob::Typesystem::MemberIndex member,
                                 const Dob::Typesystem::ArrayIndex index,
                                 const bool changed);

    /**
     * Compare two blobs and set the change flags.
     *
     * Change flags are set in "mine" on all members that have
     * changed between "base" and "mine".
     *
     * @param base [in] - Original to compare.
     * @param mine [in,out] - Compare to this and set change flags.
     */
    DOTS_API void Diff(char const * const base,
                       char * const mine);

    /**
     * Generate a 64 bit hash from a string.
     */
    DOTS_API Int64 Generate64BitHash(const std::wstring & str);

    /**
     * Generate a random 64 bit number.
     */
    DOTS_API Int64 GenerateRandom64Bit();

    //These constants used to be calculated from the strings, but that caused some
    //strange elaboration errors, so they were changed to hard-coded constants.

    const Int64 DEFAULT_CHANNEL_ID = 3313918482685577033LL; //Internal::Generate64BitHash(L"DEFAULT_CHANNEL");
    const Int64 DEFAULT_HANDLER_ID = -6778878277529052275LL; //Internal::Generate64BitHash(L"DEFAULT_HANDLER");

    const Int64 ALL_CHANNELS_ID = -3399801522715850860LL; //Internal::Generate64BitHash(L"ALL_CHANNELS");
    const Int64 ALL_HANDLERS_ID = 192125831057403942LL; //Internal::Generate64BitHash(L"ALL_HANDLERS");
}
}
}
}

#endif
