/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safirsdkcore.com)
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


#include "StatisticsCollection.h"
#include <assert.h>
#include <iostream>




static const double fraction_multiplicator = pow(10.0,-boost::posix_time::time_duration::num_fractional_digits());
double GetUtcTime()
{
    using namespace boost::posix_time;

    const ptime now = microsec_clock::universal_time();
    const ptime epoch(boost::gregorian::date(1970,1,1));
    const time_duration diff = now - epoch;
    const double foo =  diff.total_seconds() + diff.fractional_seconds() * fraction_multiplicator;
    //    std::wcout << "UTC-time in seconds = " << foo << std::endl;
    return foo;
}


HzCollector::HzCollector():
    m_startTime(GetUtcTime()),
    m_count(0)
{
}

HzCollector::~HzCollector()
{

}

void HzCollector::Reset()
{
    m_startTime = GetUtcTime();
    m_count = 0;
}


PercentageCollector::PercentageCollector(const std::wstring & name, bool addSelf):
    m_name(name),
    m_count(0),
    m_addSelf(addSelf)
{
}

PercentageCollector::~PercentageCollector()
{

}

void PercentageCollector::Reset()
{
    m_count = 0;
}


StatisticsCollection & StatisticsCollection::Instance()
{
    static StatisticsCollection obj;
    return obj;
}


StatisticsCollection::StatisticsCollection()
{
    m_thread = boost::thread(boost::bind(&StatisticsCollection::PrintThread,this));
}

StatisticsCollection::~StatisticsCollection()
{
    m_thread.interrupt();
    m_thread.join();
}

void StatisticsCollection::PrintThread()
{
    try
    {
        for(;;)
        {
            boost::this_thread::sleep_for(boost::chrono::seconds(10));
            PrintStatistics();
            Reset();
        }
    }
    catch (boost::thread_interrupted&)
    {

    }
}

HzCollector *
StatisticsCollection::AddHzCollector(const std::wstring & name)
{
    assert (m_hzCollectorTable.find(name) == m_hzCollectorTable.end());
    return m_hzCollectorTable.insert(std::make_pair(name,new HzCollector())).first->second;
}

PercentageCollector *
StatisticsCollection::AddPercentageCollector(const std::wstring & name,
                                             const HzCollector * const collector,
                                             bool addSelf)
{
    return m_percentageCollectors.insert(std::make_pair(collector,new PercentageCollector(name,addSelf))).first->second;
}


LatencyCollector *
StatisticsCollection::AddLatencyCollector(const::std::wstring & name)
{
    assert (m_latencyCollectorTable.find(name) == m_latencyCollectorTable.end());
    return m_latencyCollectorTable.insert(std::make_pair(name,new LatencyCollector())).first->second;
}

void StatisticsCollection::PrintStatistics() const
{
    std::wcout << "-----------------------------------------------------------" << std::endl;
    Safir::Dob::Typesystem::Si64::Second now = GetUtcTime();
    for (HzCollectorTable::const_iterator it = m_hzCollectorTable.begin();
         it != m_hzCollectorTable.end(); ++it)
    {
        HzCollector * collector = it->second;
        if (collector->m_count != 0)
        {
            std::wcout << it->first << ":\t" << collector->m_count/(now - collector->m_startTime) << " Hz. ";
            PercentageCollectorTable::const_iterator findPercentage = m_percentageCollectors.find(collector);
            if (findPercentage != m_percentageCollectors.end())
            {
                PercentageCollector * percentage = findPercentage->second;
                const double percent = 100.0 * (percentage->m_addSelf?
                                        (double)percentage->m_count/(collector->m_count + percentage->m_count):
                                        (double)percentage->m_count/(collector->m_count));

                std::wcout << percentage->m_name << " " << percent << "% (" << percentage->m_count/(now - collector->m_startTime) << " Hz)";
            }
            std::wcout << std::endl;
        }
    }
    
    for (LatencyCollectorTable::const_iterator it = m_latencyCollectorTable.begin();
         it != m_latencyCollectorTable.end(); ++it)
    {
        LatencyCollector * collector = it->second;
        if (collector->m_count != 0)
        {
            std::wcout << it->first << ":\t average = " << collector->m_totalDelay/collector->m_count << ", max = " << collector->m_maxDelay << ", min = " << collector->m_minDelay << std::endl;
        }
    }
}

void StatisticsCollection::Reset()
{
    for (HzCollectorTable::iterator it = m_hzCollectorTable.begin();
         it != m_hzCollectorTable.end(); ++it)
    {
        it->second->Reset();
    }

    for (PercentageCollectorTable::iterator it = m_percentageCollectors.begin();
        it != m_percentageCollectors.end(); ++it)
    {
        it->second->Reset();
    }

    for (LatencyCollectorTable::iterator it = m_latencyCollectorTable.begin();
         it != m_latencyCollectorTable.end(); ++it)
    {
        it->second->Reset();
    }
}


