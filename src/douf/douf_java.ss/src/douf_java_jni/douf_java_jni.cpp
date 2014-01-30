/******************************************************************************
*
* Copyright Saab AB, 2013 (http://safir.sourceforge.net)
*
* Created by: Lars Hagstrom / lars.hagstrom@consoden.se
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
#ifdef __GNUC__
#pragma GCC visibility push (default)
#endif

#include "com_saabgroup_safir_time_Library.h"

#ifdef __GNUC_
#pragma GCC visibility pop
#endif

#include <Safir/Time/Internal/Interface.h>

/*
 * Class:     com_saabgroup_safir_time_Library
 * Method:    getUtcTime
 * Signature: ()D
 */
jdouble JNICALL Java_com_saabgroup_safir_time_Library_getUtcTime (JNIEnv *, jclass)
{
    bool success;
    DotsC_Float64 utcTime;
    DoufTimeC_GetUtcTime(utcTime,success);
    if (success)
    {
        return utcTime;
    }
    else
    {
        return -1;
    }
}

/*
 * Class:     com_saabgroup_safir_time_Library
 * Method:    getLocalTimeOffset
 * Signature: ()J
 */
jint JNICALL Java_com_saabgroup_safir_time_Library_getLocalTimeOffset (JNIEnv *, jclass)
{
    bool success;
    DotsC_Int32 offset;
    DoufTimeC_GetLocalTimeOffset(offset,success);
    if (success)
    {
        return offset;
    }
    else
    {
        return -1;
    }
}
