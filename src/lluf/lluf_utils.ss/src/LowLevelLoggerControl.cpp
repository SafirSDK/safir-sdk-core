/******************************************************************************
*
* Copyright Saab AB, 2012-2013 (http://safir.sourceforge.net)
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
    const char * SHARED_MEMORY_NAME = "LLUF_LLL_SHARED_MEMORY";
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
            : m_shmData(NULL)
            , m_configReader(new Safir::Utilities::Internal::ConfigReader())
            , m_disabled(m_configReader->Logging().get<bool>("LowLevelLog.disabled"))
            , m_startupSynchronizer("LLUF_LLL_INITIALIZATION")
            , m_readWrite(readWrite)
            , m_openOnly(openOnly)
        {
            if (m_disabled)
            {
                m_configReader.reset();
                return;
            }

            m_startupSynchronizer.Start(this);
        }
                         
        bool Disabled() const
        {
            return m_disabled;
        }

        void CheckDisabled() const
        {
            if (m_disabled)
            {
                throw std::logic_error("LowLevelLogger is disabled! It is illegal to call this method!");
            }
        }
                         
        const boost::filesystem::path LogDirectory() const
        {
            CheckDisabled();
            return m_configReader->Logging().get<std::string>("LowLevelLog.log_directory");
        }

        //StartupSynchronizer stuff
        virtual void Create()
        {
            CheckDisabled();

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

            data->logLevel = m_configReader->Logging().get<int>("LowLevelLog.log_level");
            data->timestamps = m_configReader->Logging().get<bool>("LowLevelLog.show_timestamps");
            data->toStdout = m_configReader->Logging().get<bool>("LowLevelLog.to_standard_output");
            data->toFile = m_configReader->Logging().get<bool>("LowLevelLog.to_file");
            data->ignoreFlush = m_configReader->Logging().get<bool>("LowLevelLog.ignore_flush");
        }

        virtual void Use()
        {
            CheckDisabled();

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
            CheckDisabled();

            boost::interprocess::shared_memory_object::remove(SHARED_MEMORY_NAME);
        }

        void CheckReadWrite() const 
        {
            CheckDisabled();

            if (!m_readWrite)
            {
                throw std::logic_error("This instance of LowLevelLoggerControl is not read-write");
            }
        }

        struct ShmData
        {
            int logLevel;
            bool timestamps;
            bool toStdout;
            bool toFile;
            bool ignoreFlush;
        };

        ShmData* m_shmData;
    private:
        boost::shared_ptr<Safir::Utilities::Internal::ConfigReader> m_configReader;
        const bool m_disabled;
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

    bool LowLevelLoggerControl::Disabled() const
    {
        return m_impl->Disabled();
    }

    const boost::filesystem::path LowLevelLoggerControl::LogDirectory() const
    {
        return m_impl->LogDirectory();
    }

    const int* LowLevelLoggerControl::GetLogLevelPointer() const
    {
        m_impl->CheckDisabled();
        return &m_impl->m_shmData->logLevel;
    }

    int LowLevelLoggerControl::LogLevel() const
    {
        m_impl->CheckDisabled();
        return m_impl->m_shmData->logLevel;
    }

    void LowLevelLoggerControl::LogLevel(const int level)
    {
        m_impl->CheckReadWrite();
        m_impl->m_shmData->logLevel = level;
    }

        
    bool LowLevelLoggerControl::UseTimestamps() const
    {
        m_impl->CheckDisabled();
        return m_impl->m_shmData->timestamps;
    }
    
    void LowLevelLoggerControl::UseTimestamps(const bool enabled)
    {
        m_impl->CheckReadWrite();
        m_impl->m_shmData->timestamps = enabled;
    }

        
    bool LowLevelLoggerControl::LogToStdout()  const
    {
        m_impl->CheckDisabled();
        return m_impl->m_shmData->toStdout;
    }
    
    void LowLevelLoggerControl::LogToStdout(const bool enabled)
    {
        m_impl->CheckReadWrite();
        m_impl->m_shmData->toStdout = enabled;
    }


    bool LowLevelLoggerControl::LogToFile()  const
    {
        m_impl->CheckDisabled();
        return m_impl->m_shmData->toFile;
    }
    
    void LowLevelLoggerControl::LogToFile(const bool enabled)
    {
        m_impl->CheckReadWrite();
        m_impl->m_shmData->toFile = enabled;
    }


    bool LowLevelLoggerControl::IgnoreFlush()  const
    {
        m_impl->CheckDisabled();
        return m_impl->m_shmData->ignoreFlush;
    }
    
    void LowLevelLoggerControl::IgnoreFlush(const bool enabled)
    {
        m_impl->CheckReadWrite();
        m_impl->m_shmData->ignoreFlush = enabled;
    }
}
}
}


