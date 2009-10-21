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
#include "ConsumerTable.h"
#include <ace/Guard_T.h>
#include <iostream>

ConsumerTable * volatile ConsumerTable::m_instance = NULL;

ACE_Thread_Mutex ConsumerTable::m_instantiationLock;

ConsumerTable & ConsumerTable::Instance()
{
    if (m_instance == NULL)
    {
        ACE_Guard<ACE_Thread_Mutex> lck(m_instantiationLock);

        if (m_instance == NULL)
        {
            m_instance = new ConsumerTable();
        }
    }
    return *m_instance;
}

ConsumerTable::ConsumerTable()
{

}

ConsumerTable::~ConsumerTable()
{

}


jobject ConsumerTable::AddReference(JNIEnv * env, const jobject consumer)
{
    ACE_Guard<ACE_Thread_Mutex> lck(m_lock);
    for (Table::iterator it = m_table.begin();
         it != m_table.end(); ++it)
    {
        if (env->IsSameObject(it->doseConsumer, consumer))
        {
            ++(it->references);
            //std::wcout << "Increasing refcount of consumer 0x" << std::hex << (void*)it->doseConsumer << " to " << std::dec << it->references << std::endl;
            return it->doseConsumer;
        }
    }
    jobject doseConsumer = env->NewGlobalRef(consumer);
    //std::wcout << "Adding consumer 0x" << std::hex << (void*)doseConsumer << std::endl;
    m_table.push_back(Entry(doseConsumer));
    //std::wcout << "ConsumerTable is now " << std::dec << m_table.size() << " entries long" << std::endl;
    return doseConsumer;
}


jobject ConsumerTable::GetReference(JNIEnv * env, const jobject consumer)
{
    ACE_Guard<ACE_Thread_Mutex> lck(m_lock);
    for (Table::iterator it = m_table.begin();
         it != m_table.end(); ++it)
    {
        if (env->IsSameObject(it->doseConsumer, consumer))
        {
            //std::wcout << "Getting consumer 0x" << std::hex << (void*)it->doseConsumer << std::endl;
            return it->doseConsumer;
        }
    }

    return NULL;
}


void ConsumerTable::DropReference(JNIEnv * env, const jobject doseConsumer, int noReferences)
{
    //we optimistically assume that the consumer that we're dropping is towards the end
    //of the table, since the most frequent consumers to be dropped would be "temporary" ones,
    //i.e. MessageSenders and Requestors, and they would have been put on "fairly recently"
    //when we're removing them.

    ACE_Guard<ACE_Thread_Mutex> lck(m_lock);
    for (Table::reverse_iterator it = m_table.rbegin();
         it != m_table.rend(); ++it)
    {
        if (env->IsSameObject(it->doseConsumer, doseConsumer))
        {
            it->references -= noReferences;
            //std::wcout << "Dropping consumer 0x" << std::hex << (void*)doseConsumer << " to " << std::dec << it->references << std::endl;
            if (it->references < 0)
            {
                std::wcerr << "References got to below 0!" << std::endl;
                std::wcerr << "Consumer 0x" << std::hex << (void*)doseConsumer << " dropped to " << std::dec <<  it->references << std::endl;
                exit(10);
            }
            if (it->references == 0)
            {
                m_table.erase((++it).base()); //see Effective STL item 28.
                //std::wcout << "ConsumerTable is now " << std::dec << m_table.size() << " entries long" << std::endl;
            }
            return;
        }
    }
    std::wcerr << "Failed to find consumer to drop" << std::endl;
    exit(10);
}
