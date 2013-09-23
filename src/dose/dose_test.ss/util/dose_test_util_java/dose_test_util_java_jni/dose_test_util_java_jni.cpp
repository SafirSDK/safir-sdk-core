/******************************************************************************
*
* Copyright Saab AB, 2011-2013 (http://safir.sourceforge.net)
*
* Created by: Mikael Wennerberg / stmiwn
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

#include <Safir/Dob/Internal/DoseTest/dose_test_util.h>
#include <assert.h>
#include "com_saabgroup_safir_dob_test_util_DoseTestUtil.h"

//use this to set the first element of an array
//will assert on arraylength == 1
void SetJArray(JNIEnv * env, jbooleanArray array, const bool toValue)
{
    jboolean isCopy;
    jboolean * arrayElems = env->GetBooleanArrayElements(array, &isCopy);
    assert(env->GetArrayLength(array) == 1);
    arrayElems[0] = toValue;
    if (isCopy == JNI_TRUE)
    {
        env->ReleaseBooleanArrayElements(array, arrayElems, 0);
    }
}

/*
 * Class:     com_saabgroup_safir_dob_test_util_DoseTestUtil
 * Method:    InhibitOutgoingTraffic
 * Signature: (Z[Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_test_util_DoseTestUtil_InhibitOutgoingTraffic
  (JNIEnv * env, jclass, jboolean _val, jbooleanArray _success)
{
    bool success;
    InhibitOutgoingTraffic(_val == JNI_TRUE, success);
    SetJArray(env,_success, success);
}

/*
 * Class:     com_saabgroup_safir_dob_test_util_DoseTestUtil
 * Method:    IsOutgoingTrafficInhibited
 * Signature: ([Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_test_util_DoseTestUtil_InhibitOutgoingTrafficStatus
  (JNIEnv * env, jclass, jbooleanArray _val)
{
    bool val;
    InhibitOutgoingTrafficStatus(val);
    SetJArray(env,_val, val);
}




