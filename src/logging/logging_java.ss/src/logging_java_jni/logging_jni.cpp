/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
*
* Created by: Lars Hagström / stlrha
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

#include "com_saabgroup_safir_Logging.h"

#ifdef __GNUC__
#pragma GCC visibility pop
#endif

#include <Safir/Logging/Internal/Interface.h>

class StringConverter
{
public:
    StringConverter(JNIEnv * _env, jstring jstr):
        m_env(_env),
        m_jstr(jstr),
        m_isCopy(false)
    {
        m_str = m_env->GetStringUTFChars(m_jstr,&m_isCopy);
    }

    ~StringConverter()
    {
        m_env->ReleaseStringUTFChars(m_jstr, m_str);
    }

    operator const char*() const
    {
        return m_str;
    }
private:
    JNIEnv * m_env;
    jstring m_jstr;
    const char* m_str;
    jboolean m_isCopy;
};

void JNICALL Java_com_saabgroup_safir_Logging_sendSystemLogInternal
(JNIEnv * _env, jclass, jint _severity, jstring _message)
{
    LoggingC_SendSystemLog(_severity,
                           StringConverter(_env, _message));
}
