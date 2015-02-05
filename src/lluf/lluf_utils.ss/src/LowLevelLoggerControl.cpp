/******************************************************************************
*
* Copyright Saab AB, 2012-2013 (http://safir.sourceforge.net)
* Copyright Consoden AB, 2015 (http://www.consoden.se)
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
#include <Safir/Utilities/Internal/LowLevelLoggerControl.h>
#include <Safir/Utilities/Internal/ConfigReader.h>
#include <Safir/Utilities/StartupSynchronizer.h>
#include <boost/filesystem/convenience.hpp>
#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/fstream.hpp>
#include <boost/interprocess/mapped_region.hpp>
#include <boost/interprocess/shared_memory_object.hpp>
#include <iostream>

namespace //anonymous namespace for internal functions
{
    const std::string shmName("SAFIR_LLL_SHARED_MEMORY" +
                                           Safir::Utilities::Internal::Expansion::GetSafirInstanceSuffix());
    const char * SHARED_MEMORY_NAME = shmName.c_str();
}


namespace Safir
{
namespace Utilities
{
namespace Internal
{
    class LowLevelLoggerControl::Impl
        : public Synchronized
    {
    public:
        Impl(const bool openOnly, const bool readWrite)
            : m_configReader(new Safir::Utilities::Internal::ConfigReader())
            , m_logDirectory(m_configReader->Logging().get<std::string>("LowLevelLog_General.log_directory"))
            , m_writePeriod(m_configReader->Logging().get<std::uint32_t>("LowLevelLog_Services.write_period"))
            , m_bufferSize(m_configReader->Logging().get<std::uint64_t>("LowLevelLog_Services.write_buffer_size")*1024*1024)
            , m_startupSynchronizer("SAFIR_LLL_INITIALIZATION")
            , m_readWrite(readWrite)
            , m_openOnly(openOnly)
        {
            m_startupSynchronizer.Start(this);
        }

        bool FileLoggingEnabled() const
        {
            return !m_logDirectory.empty();
        }

        const boost::filesystem::path LogDirectory() const
        {
            if (!FileLoggingEnabled())
            {
                throw std::logic_error("File logging is disabled");
            }
            return m_logDirectory;
        }

        //StartupSynchronizer stuff
        virtual void Create()
        {
            if (m_openOnly)
            {
                return;
            }

            boost::interprocess::shared_memory_object::remove(SHARED_MEMORY_NAME);
            boost::interprocess::shared_memory_object shm(boost::interprocess::create_only,
                                                          SHARED_MEMORY_NAME,
                                                          boost::interprocess::read_write);
            shm.truncate(sizeof(ShmData));
            boost::interprocess::mapped_region shmRegion(shm,
                                                         boost::interprocess::read_write);
            ShmData* data = static_cast<ShmData*>(shmRegion.get_address());

            data->logLevel = m_configReader->Logging().get<int>("LowLevelLog_General.log_level");
            data->timestamps = m_configReader->Logging().get<bool>("LowLevelLog_General.show_timestamps");
            data->toStdout = m_configReader->Logging().get<bool>("LowLevelLog_Services.to_standard_output");
            data->ignoreFlush = m_configReader->Logging().get<bool>("LowLevelLog_General.ignore_flush");
        }

        virtual void Use()
        {
            m_configReader.reset();

            boost::interprocess::mode_t mode = boost::interprocess::read_only;
            if (m_readWrite)
            {
                mode = boost::interprocess::read_write;
            }

            boost::interprocess::shared_memory_object shm(boost::interprocess::open_only,
                                                          SHARED_MEMORY_NAME,
                                                          mode);
            m_shm.swap(shm);
            boost::interprocess::mapped_region shmRegion(m_shm, mode);
            m_shmRegion.swap(shmRegion);

            m_shmData = static_cast<ShmData*>(m_shmRegion.get_address());
        }

        virtual void Destroy()
        {
            boost::interprocess::shared_memory_object::remove(SHARED_MEMORY_NAME);
        }

        void CheckReadWrite() const
        {
            if (!m_readWrite)
            {
                throw std::logic_error("This instance of LowLevelLoggerControl is not read-write");
            }
        }

        boost::chrono::milliseconds WritePeriod() const
        {
            return m_writePeriod;
        }

        std::uint64_t BufferSize() const
        {
            return m_bufferSize;
        }

        struct ShmData
        {
            int logLevel;
            bool timestamps;
            bool toStdout;
            bool ignoreFlush;
        };

        ShmData* m_shmData {nullptr};
    private:
        std::unique_ptr<Safir::Utilities::Internal::ConfigReader> m_configReader;
        const boost::filesystem::path m_logDirectory;
        const boost::chrono::milliseconds m_writePeriod;
        const std::uint64_t m_bufferSize;
        boost::interprocess::shared_memory_object m_shm;
        boost::interprocess::mapped_region m_shmRegion;
        StartupSynchronizer m_startupSynchronizer;
        const bool m_readWrite;
        const bool m_openOnly;
    };

    LowLevelLoggerControl::LowLevelLoggerControl(const bool openOnly, const bool readWrite)
    {
        m_impl.reset(new Impl(openOnly, readWrite));
    }

    bool LowLevelLoggerControl::FileLoggingEnabled() const
    {
        return m_impl->FileLoggingEnabled();
    }

    const boost::filesystem::path LowLevelLoggerControl::LogDirectory() const
    {
        return m_impl->LogDirectory();
    }

    const int* LowLevelLoggerControl::GetLogLevelPointer() const
    {
        if (m_impl->m_shmData == nullptr)
        {
            return nullptr;
        }
        else
        {
            return &m_impl->m_shmData->logLevel;
        }
    }

    int LowLevelLoggerControl::LogLevel() const
    {
        if (m_impl->m_shmData == nullptr)
        {
            return 0;
        }
        else
        {
            return m_impl->m_shmData->logLevel;
        }
    }

    void LowLevelLoggerControl::LogLevel(const int level)
    {
        m_impl->CheckReadWrite();
        m_impl->m_shmData->logLevel = level;
    }


    bool LowLevelLoggerControl::UseTimestamps() const
    {
        if (m_impl->m_shmData == nullptr)
        {
            throw std::logic_error("Shared memory not loaded");
        }

        return m_impl->m_shmData->timestamps;
    }

    void LowLevelLoggerControl::UseTimestamps(const bool enabled)
    {
        m_impl->CheckReadWrite();
        m_impl->m_shmData->timestamps = enabled;
    }


    bool LowLevelLoggerControl::LogToStdout()  const
    {
        if (m_impl->m_shmData == nullptr)
        {
            throw std::logic_error("Shared memory not loaded");
        }

        return m_impl->m_shmData->toStdout;
    }

    void LowLevelLoggerControl::LogToStdout(const bool enabled)
    {
        m_impl->CheckReadWrite();
        m_impl->m_shmData->toStdout = enabled;
    }


    bool LowLevelLoggerControl::IgnoreFlush()  const
    {
        if (m_impl->m_shmData == nullptr)
        {
            throw std::logic_error("Shared memory not loaded");
        }

        return m_impl->m_shmData->ignoreFlush;
    }

    void LowLevelLoggerControl::IgnoreFlush(const bool enabled)
    {
        m_impl->CheckReadWrite();
        m_impl->m_shmData->ignoreFlush = enabled;
    }

    boost::chrono::milliseconds LowLevelLoggerControl::WritePeriod() const
    {
        return m_impl->WritePeriod();
    }

    std::uint64_t LowLevelLoggerControl::BufferSize() const
    {
        return m_impl->BufferSize();
    }

}
}
}
