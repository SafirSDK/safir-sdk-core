/******************************************************************************
*
* Copyright Saab AB, 2025 (http://safirsdkcore.com)
*
* Created by: Lars Hagström / lars@foldspace.nu
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
#pragma once

#include <Safir/Application/TracerStatus.h>
#include <Safir/Dob/Consumer.h>
#include <Safir/Dob/LowMemoryException.h>
#include <Safir/Dob/NotFoundException.h>
#include <Safir/Dob/ThisNodeParameters.h>
#include <Safir/Logging/Log.h>
#include <Safir/Utilities/AsioDispatcher.h>
#include <Safir/Utilities/ProcessInfo.h>
#include <boost/algorithm/string/predicate.hpp>
#include <boost/asio.hpp>
#include <atomic>
#include <iomanip>
#include <iostream>
#include <memory>
#include <set>
#include <thread>



/**
 * PrefixId is an opaque handle to a prefix, exposed to language bindings.
 *
 * INTERNAL IMPLEMENTATION DETAILS (do not rely on these in external code):
 *
 * PrefixId is actually a pointer to PrefixState disguised as an int64.
 * This design enables:
 * - C API compatibility (C can't handle C++ objects, only integers)
 * - Language bindings (Java/C#/.NET) can hold PrefixId as a long/Int64
 * - Direct memory access via GetPrefixStatePointer() for performance
 *
 * CRITICAL REQUIREMENTS:
 *
 * 1. MUST use std::list<PrefixState> for storage
 *    - std::list guarantees pointer stability (elements never move in memory)
 *    - This makes PrefixId (a pointer) valid for the lifetime of the container
 *    - DO NOT change to std::vector or other containers that relocate elements!
 *
 * 2. NEVER remove elements from m_prefixes
 *    - Once a PrefixId is issued, that pointer must remain valid forever
 *    - Language bindings may hold PrefixIds for the entire process lifetime
 *    - Removing elements would create dangling pointers
 *
 * 3. GetPrefixStatePointer() returns volatile bool*
 *    - This pointer is given to language bindings for direct read/write access
 *    - volatile prevents compiler optimization across the pointer access
 *    - WARNING: volatile is NOT a threading primitive!
 *      * Does not provide memory barriers or cache coherency guarantees
 *      * Relies on bool read/write being atomic on target platforms
 *      * Synchronization happens via DOB entity updates and frequent memory traffic
 *    - This is a pragmatic compromise for multi-language C API compatibility
 *
 * 4. Thread safety model:
 *    - m_prefixes guarded by m_prefixSearchLock for Add/iteration
 *    - Once PrefixId obtained, m_isEnabled can be accessed without lock (via volatile pointer)
 *    - Correctness depends on:
 *      a) bool read/write atomicity (guaranteed on modern CPUs)
 *      b) Synchronization from DOB entity updates (authoritative source of truth)
 *      c) Frequent memory traffic creating implicit visibility
 *    - Worst case: a few trace calls see stale enabled/disabled state briefly
 */
typedef Safir::Dob::Typesystem::Int64 PrefixId;

class Prefixes:
    public Safir::Dob::EntitySubscriber,
    public Safir::Dob::Requestor,
    public Safir::Dob::RegistrationSubscriber
{
public:
    Prefixes(Safir::Dob::Connection& connection,
             boost::asio::io_context& ioContext)
        : m_connection(connection)
        , m_ioContext(ioContext)
        , m_stop(false)
    {
    }

    void StartEntityHandling(const std::wstring& programName)
    {
        m_programName = programName;

        m_connection.SubscribeEntity(GetMyEntityId(), true, true, this);
        m_connection.SubscribeRegistration(Safir::Application::TracerStatus::ClassTypeId,
                                           Safir::Dob::Typesystem::HandlerId(),
                                           true,
                                           true,
                                           this);

    }

    void StopEntityHandling()
    {
        m_stop = true;
        if (m_timer != nullptr)
        {
            m_timer->cancel();
        }
    }

    // Lock-free reads via atomics
    bool LogToStdout() const {return m_logToStdout.load(std::memory_order_acquire);}
    bool LogToSafirLogging() const {return m_logToSafirLogging.load(std::memory_order_acquire);}
    bool LogToTracer() const {return m_logToTracer.load(std::memory_order_acquire);}

    // Check if any prefix is enabled - uses lock but only called on syslog paths
    bool AnyPrefixEnabled() const
    {
        std::unique_lock<std::mutex> lck(m_prefixSearchLock);
        return std::any_of(m_prefixes.begin(), m_prefixes.end(),
                           [](const PrefixState& p) { return p.m_isEnabled; });
    }


    PrefixId Add(const std::wstring & prefix)
    {
        bool wasNew = false;
        PrefixId result;
        {
            std::unique_lock<std::mutex> lck(m_prefixSearchLock);
            wasNew = (std::find(m_prefixes.begin(), m_prefixes.end(), prefix) == m_prefixes.end());
            result = AddInternal(prefix);
        }
        if (wasNew)
        {
            UpdateEntity();
        }
        return result;
    }

    volatile bool * GetStatePointer(const PrefixId prefixId) { return &ToPrefix(prefixId).m_isEnabled; }

    bool IsEnabled(const PrefixId prefixId) const { return ToPrefix(prefixId).m_isEnabled; }
    void Enable(const PrefixId prefixId, const bool enabled)
    {
        ToPrefix(prefixId).m_isEnabled = enabled;
        UpdateEntity();
    }

    PrefixId GetPrefixId(const std::wstring& prefix) const
    {
        std::unique_lock<std::mutex> lck(m_prefixSearchLock);
        auto findIt = std::find(m_prefixes.begin(), m_prefixes.end(), prefix);
        if (findIt != m_prefixes.end())
        {
            return ToPrefixId(*findIt);
        }
        else
        {
            return 0;
        }
    }

    std::set<PrefixId> GetAllPrefixIds() const
    {
        std::unique_lock<std::mutex> lck(m_prefixSearchLock);
        return GetAllPrefixIdsInternal();
    }

    const std::wstring& GetPrefix(const PrefixId prefixId) const { return ToPrefix(prefixId).m_prefix; }
    const std::wstring& GetPrefixAscii(const PrefixId prefixId) const { return ToPrefix(prefixId).m_prefixAscii; }
    const std::wstring GetHelpText() const
    {
        std::wostringstream out;
        std::unique_lock<std::mutex> lck(m_prefixSearchLock);
        for (const auto& prefix: m_prefixes)
        {
            out << "<app>:   " << std::setw (m_longestPrefix) << prefix.m_prefix << " on/off - Turn logging of this prefix on or off. Currently "<<
                (prefix.m_isEnabled?"on":"off") <<std::endl;
        }
        return out.str();
    }

    int LongestPrefixLength() const {return m_longestPrefix;}

    Safir::Dob::Typesystem::EntityId GetMyEntityId() const
    {
        return {Safir::Application::TracerStatus::ClassTypeId,
                Safir::Dob::Typesystem::InstanceId(Safir::Dob::ThisNodeParameters::Name() + L";" + m_programName)};
    }

    void HandleEntity(const Safir::Dob::EntityProxy& entityProxy)
    {
        const auto entity = std::static_pointer_cast<Safir::Application::TracerStatus>(entityProxy.GetEntity());

        m_logToStdout.store(entity->LogToStdout().GetValOrDefault(m_logToStdout.load(std::memory_order_relaxed)),
                            std::memory_order_release);
        m_logToSafirLogging.store(entity->LogToSafirLogging().GetValOrDefault(m_logToSafirLogging.load(std::memory_order_relaxed)),
                                  std::memory_order_release);
        m_logToTracer.store(entity->LogToTracer().GetValOrDefault(m_logToTracer.load(std::memory_order_relaxed)),
                            std::memory_order_release);

        std::unique_lock<std::mutex> lck(m_prefixSearchLock);
        auto missingPrefixIds = GetAllPrefixIdsInternal();
        for (const auto& prefix: entity->Prefixes())
        {
            const auto id = AddInternal(prefix.first);
            missingPrefixIds.erase(id);
            ToPrefix(id).m_isEnabled = prefix.second.GetVal();
        }
        lck.unlock();

        if (!missingPrefixIds.empty() ||
            entity->ProgramName().IsNull() ||
            entity->NodeName().IsNull() ||
            entity->LogToStdout().IsNull() ||
            entity->LogToSafirLogging().IsNull() ||
            entity->LogToTracer().IsNull())
        {
            UpdateEntity();
        }
    }

    void UpdateEntity()
    {
        if (m_stop)
        {
            return;
        }

        if (m_timer == nullptr)
        {
            m_timer = std::make_unique<boost::asio::steady_timer>(m_ioContext);
        }

        m_timer->cancel();
        m_timer->expires_after(std::chrono::milliseconds(40));
        m_timer->async_wait([this](const boost::system::error_code& error)
        {
            if (error || m_stop)
            {
                return;
            }

            try
            {
                Safir::Application::TracerStatusPtr entity;
                bool created = false;
                try
                {
                    entity = std::static_pointer_cast<Safir::Application::TracerStatus>(m_connection.Read(GetMyEntityId()).GetEntity());
                    created = true;
                }
                catch (const Safir::Dob::NotFoundException&)
                {
                    entity = Safir::Application::TracerStatus::Create();
                }

                if (entity->ProgramName().IsNull() || entity->ProgramName() != m_programName)
                {
                    entity->ProgramName() = m_programName;
                }

                if (entity->NodeName().IsNull() || entity->NodeName() != Safir::Dob::ThisNodeParameters::Name())
                {
                    entity->NodeName() = Safir::Dob::ThisNodeParameters::Name();
                }

                std::unique_lock<std::mutex> lck(m_prefixSearchLock);
                for (const auto& prefix: m_prefixes)
                {
                    auto findIt = entity->Prefixes().find(prefix.m_prefix);
                    if (findIt == entity->Prefixes().end())
                    {
                        entity->Prefixes().Insert(prefix.m_prefix, prefix.m_isEnabled);
                    }
                    else if (findIt->second.IsNull() || findIt->second.GetVal() != prefix.m_isEnabled)
                    {
                        findIt->second.SetVal(prefix.m_isEnabled);
                    }
                }

                lck.unlock();

                if (entity->LogToStdout().IsNull())
                {
                    entity->LogToStdout() = m_logToStdout.load(std::memory_order_relaxed);
                }

                if (entity->LogToSafirLogging().IsNull())
                {
                    entity->LogToSafirLogging() = m_logToSafirLogging.load(std::memory_order_relaxed);
                }

                if (entity->LogToTracer().IsNull())
                {
                    entity->LogToTracer() = m_logToTracer.load(std::memory_order_relaxed);
                }

                if (!created)
                {
                    m_connection.CreateRequest(entity,GetMyEntityId().GetInstanceId(), Safir::Dob::Typesystem::HandlerId(), this);
                }
                else if (entity->IsChanged())
                {
                    m_connection.UpdateRequest(entity,GetMyEntityId().GetInstanceId(), this);
                }
            }
            catch (const Safir::Dob::LowMemoryException&)
            {
                return;
            }
        });
    }

    void OnNewEntity(const Safir::Dob::EntityProxy entityProxy) override { HandleEntity(entityProxy); }
    void OnUpdatedEntity(const Safir::Dob::EntityProxy entityProxy) override { HandleEntity(entityProxy); }

    void OnDeletedEntity(const Safir::Dob::EntityProxy /*entityProxy*/,
                         const bool /*deprecated*/) override
    {
        UpdateEntity();
    }

    void OnRegistered(const Safir::Dob::Typesystem::TypeId      /*typeId*/,
                      const Safir::Dob::Typesystem::HandlerId&  /*handlerId*/) override
    {
        UpdateEntity();
    }

    void OnUnregistered(const Safir::Dob::Typesystem::TypeId      /*typeId*/,
                        const Safir::Dob::Typesystem::HandlerId&  /*handlerId*/) override
    {
    }

    void OnResponse(const Safir::Dob::ResponseProxy responseProxy) override
    {
        if (!responseProxy.IsSuccess())
        {
            UpdateEntity();
        }
    }

    void OnNotRequestOverflow() override
    {
        UpdateEntity();
    }

private:
    struct PrefixState
    {
        //PrefixState():m_prefix(),m_isEnabled(false) {}
        PrefixState(const std::wstring & prefix, const bool enabled)
            : m_prefix(prefix)
            , m_prefixAscii(prefix)
            , m_isEnabled(enabled)
        {
            //replace non-ascii chars
            for(std::wstring::iterator it = m_prefixAscii.begin();
                it != m_prefixAscii.end(); ++it)
            {
                if ((*it & ~0x7F) != 0)
                {
                    *it = '@';
                }
            }
        }
        bool operator==(const std::wstring & str) const {return m_prefix == str;}

        const std::wstring m_prefix;
        std::wstring m_prefixAscii;
        bool m_isEnabled;
    };
    // Ensure PrefixId (int64) is large enough to hold a pointer on all platforms
    BOOST_STATIC_ASSERT(sizeof(::PrefixId) >= sizeof(PrefixState*));

    // Convert PrefixId back to PrefixState reference (unwrap the disguised pointer)
    static PrefixState & ToPrefix(const PrefixId prefixId) {return *reinterpret_cast<PrefixState*>(prefixId);}

    // Convert PrefixState address to PrefixId (disguise pointer as int64)
    static PrefixId ToPrefixId(const PrefixState& prefix) {return reinterpret_cast<PrefixId>(&prefix);}

    // Does not take any locks!
    PrefixId AddInternal(const std::wstring & prefix)
    {
        auto findIt = std::find(m_prefixes.begin(), m_prefixes.end(), prefix);
        if (findIt != m_prefixes.end())
        {
            return ToPrefixId(*findIt);
        }
        else
        {
            m_prefixes.push_back(PrefixState(prefix,false));
            m_longestPrefix = std::max(m_longestPrefix, static_cast<int>(prefix.length()));
            return ToPrefixId(m_prefixes.back());
        }
    }

    std::set<PrefixId> GetAllPrefixIdsInternal() const
    {
        std::set<PrefixId> prefixIds;
        for (const auto& prefix: m_prefixes)
        {
            prefixIds.insert(ToPrefixId(prefix));
        }
        return prefixIds;
    }

    Safir::Dob::Connection& m_connection;
    boost::asio::io_context& m_ioContext;
    std::wstring m_programName;

    // Contains all prefixes. Pointers into this structure are returned as PrefixId handles to
    // language bindings. MUST be std::list for pointer stability. NEVER remove anything from
    // this list - PrefixIds must remain valid for the process lifetime!
    std::list<PrefixState> m_prefixes;
    int m_longestPrefix = 0;
    mutable std::mutex m_prefixSearchLock; //lock for anyone that loops through the prefixes or adds elements to it.

    std::unique_ptr<boost::asio::steady_timer> m_timer;
    std::atomic<bool> m_stop;

    std::atomic<bool> m_logToStdout{true};
    std::atomic<bool> m_logToSafirLogging{true};
    std::atomic<bool> m_logToTracer{true};
};
