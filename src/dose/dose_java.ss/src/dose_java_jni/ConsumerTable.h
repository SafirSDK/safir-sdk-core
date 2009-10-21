/******************************************************************************
*
* Copyright Saab AB, 2009 (http://www.safirsdk.com)
*
* Created by: Lars Hagstrom / stlrha
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
#ifndef __DOSE_JAVA_CONSUMER_TABLE_H__
#define __DOSE_JAVA_CONSUMER_TABLE_H__

#include <boost/noncopyable.hpp>
#include <boost/cstdint.hpp>
#include <jni.h>
#include <vector>

#ifdef _MSC_VER
  #pragma warning(push)
  #pragma warning(disable: 4267)
#endif

#include <ace/Thread_Mutex.h>
#include <ace/Thread.h>

#ifdef _MSC_VER
  #pragma warning(pop)
#endif


class ConsumerTable :
    private boost::noncopyable
{
public:
    static ConsumerTable & Instance();

    jobject AddReference(JNIEnv * env, const jobject consumer);

    //returns NULL if not found
    jobject GetReference(JNIEnv * env, const jobject consumer);

    void DropReference(JNIEnv * env, const jobject doseConsumer, int noReferences = 1);
private:
    ConsumerTable();
    ~ConsumerTable();

    ACE_Thread_Mutex m_lock;

    struct Entry
    {
        Entry(jobject _doseConsumer):
            doseConsumer(_doseConsumer),
            references(1){}

        jobject doseConsumer;
        boost::uint32_t references;
    };


    typedef std::vector<Entry> Table;

    Table m_table;

    //Singleton stuff
    static ConsumerTable * volatile m_instance;

    static ACE_Thread_Mutex m_instantiationLock;

};


#endif

