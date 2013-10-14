/******************************************************************************
*
* Copyright Saab AB, 2009-2013 (http://safir.sourceforge.net)
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
#include <boost/thread/once.hpp>
#include <boost/thread/mutex.hpp>

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

    boost::mutex m_lock;

    struct Entry
    {
        Entry(jobject _doseConsumer):
            doseConsumer(_doseConsumer),
            references(1){}

        jobject doseConsumer;
        boost::int32_t references;
    };


    typedef std::vector<Entry> Table;

    Table m_table;

    /**
     * This class is here to ensure that only the Instance method can get at the 
     * instance, so as to be sure that boost call_once is used correctly.
     * Also makes it easier to grep for singletons in the code, if all 
     * singletons use the same construction and helper-name.
     */
    struct SingletonHelper
    {
    private:
        friend ConsumerTable& ConsumerTable::Instance();
        
        static ConsumerTable& Instance();
        static boost::once_flag m_onceFlag;
    };

};


#endif

