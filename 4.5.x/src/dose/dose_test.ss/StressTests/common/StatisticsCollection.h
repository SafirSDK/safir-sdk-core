/******************************************************************************
*
* Copyright Saab AB, 2006-2013 (http://safir.sourceforge.net)
*
* Created by: Hannah Myerscough / sthamy
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

#ifndef __FREQ_CALC_H__
#define __FREQ_CALC_H__

#include <map>
#include <Safir/Dob/Typesystem/Defs.h>
#include <boost/noncopyable.hpp>

#ifdef _MSC_VER
  #pragma warning(push)
  #pragma warning(disable: 4267)
  #pragma warning(disable: 4244)
#endif

#include <boost/date_time/posix_time/posix_time_types.hpp>
#include <boost/thread.hpp>

#ifdef _MSC_VER
  #pragma warning(pop)
#endif

double GetUtcTime();

//TODO: Make all these thread safe!

class StatisticsCollection;


class LatencyCollector:
    private boost::noncopyable
{
public:
    void Begin()
    {
        m_beginTime = GetUtcTime();
    }
    void End()
    {
        Safir::Dob::Typesystem::Si64::Second elapsed = GetUtcTime() - m_beginTime;
        m_totalDelay += elapsed;
        m_maxDelay = std::max(m_maxDelay,elapsed);
        m_minDelay = std::min(m_minDelay,elapsed);
        ++m_count;
    }
private:
    friend class StatisticsCollection;
    LatencyCollector():
        m_maxDelay(0),
        m_minDelay(99999999999999LL),
        m_totalDelay(0)
    {}
    ~LatencyCollector(){}

    void Reset()
    {
        m_maxDelay = 0;
        m_minDelay = 99999999999999LL;
        m_totalDelay = 0;
        m_count = 0;
    }

    Safir::Dob::Typesystem::Si64::Second m_beginTime;
    Safir::Dob::Typesystem::Si64::Second m_maxDelay;
    Safir::Dob::Typesystem::Si64::Second m_minDelay;
    Safir::Dob::Typesystem::Si64::Second m_totalDelay;

    long m_count;
};

class HzCollector:
    private boost::noncopyable
{
public:
    void Tick() {++m_count;}
    void Tick(const long numPoints) {m_count += numPoints;}
private:
    friend class StatisticsCollection;
    HzCollector();
    ~HzCollector();

    void Reset();

    Safir::Dob::Typesystem::Si64::Second m_startTime;
    long m_count;
};


class PercentageCollector:
    private boost::noncopyable
{
public:
    void Tick() {++m_count;}
    void Tick(const long numPoints) {m_count += numPoints;}
private:
    friend class StatisticsCollection;
    PercentageCollector(const std::wstring & name, bool addSelf);
    ~PercentageCollector();

    void Reset();

    const std::wstring m_name;
    long m_count;
    bool m_addSelf;
};

class StatisticsCollection:
    private boost::noncopyable
{
public:
    static StatisticsCollection & Instance();
    HzCollector * AddHzCollector(const std::wstring & name);
    PercentageCollector * AddPercentageCollector(const std::wstring & name,
                                                 const HzCollector * const collector,
                                                 bool addSelf = true);

    LatencyCollector * AddLatencyCollector(const::std::wstring & name);

    void PrintStatistics() const;
    void Reset();
private:
    void PrintThread();

    StatisticsCollection();
    ~StatisticsCollection();
    typedef std::map<std::wstring, HzCollector *> HzCollectorTable;
    HzCollectorTable m_hzCollectorTable;

    typedef std::map<const HzCollector *, PercentageCollector *> PercentageCollectorTable;
    PercentageCollectorTable m_percentageCollectors;

    typedef std::map<std::wstring, LatencyCollector *> LatencyCollectorTable;
    LatencyCollectorTable m_latencyCollectorTable;

    boost::thread m_thread;
};

#endif
