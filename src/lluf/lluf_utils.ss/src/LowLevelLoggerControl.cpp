/******************************************************************************
*
* Copyright Saab AB, 2012 (http://www.safirsdk.com)
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

//Boost property tree was introduced in 1.41
//TODO: when we drop support for 1.41 we can remove this other implementation
//Dont forget to remove the stuff in CMakeLists.txt
#if ((BOOST_VERSION / 100000) >= 1 && (BOOST_VERSION / 100 % 1000) > 40)
#define HAVE_BOOST_PROPERTY_TREE
#include <boost/property_tree/ini_parser.hpp>
#include <boost/property_tree/ptree.hpp>
#else
#include <boost/program_options.hpp>
#endif

namespace //anonymous namespace for internal functions
{
    const char * SHARED_MEMORY_NAME = "LLUF_LLL_SHARED_MEMORY";

    const boost::filesystem::path GetLogDirectory()
    {
        Safir::Utilities::Internal::ConfigReader reader;
        return reader.Logging().get<std::string>("low_level_log_directory");
    }

    const boost::filesystem::path GetLogSettingsPath()
    {
        return GetLogDirectory()/"logging_on";
    }

    //check for file %SAFIR_RUNTIME%\log\Dob-LowLevelLog\logging_on
    bool LogSettingsFileExists()
    {
        try
        {
            return boost::filesystem::exists(GetLogSettingsPath());
        }
        catch(const std::logic_error&)
        {
            return false;
        }
    }
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
            , m_startupSynchronizer("LLUF_LLL_INITIALIZATION")
            , m_readWrite(readWrite)
            , m_openOnly(openOnly)
        {
            m_startupSynchronizer.Start(this);
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

            data->logLevel = 0;
            data->timestamps = true;
            data->toStdout = true;
            data->toFile = true;
            data->ignoreFlush = false;

            if (LogSettingsFileExists())
            {
                try
                {
#ifdef HAVE_BOOST_PROPERTY_TREE
                    using boost::property_tree::ptree;
                    
                    ptree root;
                    read_ini(GetLogSettingsPath().string(), root );                    
                    data->logLevel = root.get("logLevel", 9);
                    data->timestamps = root.get("timestamps", true);
                    data->toStdout = root.get("toStdout", true);
                    data->toFile = root.get("toFile", true);
                    data->ignoreFlush = root.get("ignoreFlush", false);
#else
                    using namespace boost::program_options;
                    options_description options;
                    options.add_options()
                        ("logLevel",value<int>(&data->logLevel)->default_value(9),"")
                        ("ignoreFlush","")
                        ("timestamps","")
                        ("toStdout","")
                        ("toFile","");
                    variables_map vm;
                    boost::filesystem::ifstream ini(GetLogSettingsPath());
                    store(parse_config_file<char>(ini,options),vm);
                    notify(vm);

                    data->timestamps = vm["timestamps"].as<std::string>() == "true";
                    data->toStdout = vm["toStdout"].as<std::string>() == "true";
                    data->toFile = vm["toFile"].as<std::string>() == "true";
                    data->ignoreFlush = vm["ignoreFlush"].as<std::string>() == "true";

#endif
                }
                catch (const std::exception&)
                {
                    data->logLevel = 9;
                }
            }
        }

        virtual void Use()
        {
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

    const boost::filesystem::path LowLevelLoggerControl::GetLogDirectory()
    {
        return ::GetLogDirectory();
    }

    const int* LowLevelLoggerControl::GetLogLevelPointer() const
    {
        return &m_impl->m_shmData->logLevel;
    }

    int LowLevelLoggerControl::LogLevel() const
    {
        return m_impl->m_shmData->logLevel;
    }

    void LowLevelLoggerControl::LogLevel(const int level)
    {
        m_impl->CheckReadWrite();
        m_impl->m_shmData->logLevel = level;
    }

        
    bool LowLevelLoggerControl::UseTimestamps() const
    {
        return m_impl->m_shmData->timestamps;
    }
    
    void LowLevelLoggerControl::UseTimestamps(const bool enabled)
    {
        m_impl->CheckReadWrite();
        m_impl->m_shmData->timestamps = enabled;
    }

        
    bool LowLevelLoggerControl::LogToStdout()  const
    {
        return m_impl->m_shmData->toStdout;
    }
    
    void LowLevelLoggerControl::LogToStdout(const bool enabled)
    {
        m_impl->CheckReadWrite();
        m_impl->m_shmData->toStdout = enabled;
    }


    bool LowLevelLoggerControl::LogToFile()  const
    {
        return m_impl->m_shmData->toFile;
    }
    
    void LowLevelLoggerControl::LogToFile(const bool enabled)
    {
        m_impl->CheckReadWrite();
        m_impl->m_shmData->toFile = enabled;
    }


    bool LowLevelLoggerControl::IgnoreFlush()  const
    {
        return m_impl->m_shmData->ignoreFlush;
    }
    
    void LowLevelLoggerControl::IgnoreFlush(const bool enabled)
    {
        m_impl->CheckReadWrite();
        m_impl->m_shmData->ignoreFlush = enabled;
    }
    
    void LowLevelLoggerControl::WriteIniFile(const int level,
                                             const bool useTimestamps,
                                             const bool toStdout,
                                             const bool toFile,
                                             const bool ignoreFlush)
    {
        if (!LogSettingsFileExists())
        {
            try
            {
                boost::filesystem::create_directory(GetLogDirectory());
            }
            catch (const boost::filesystem::filesystem_error&)
            {
                throw std::logic_error("Failed to create log directory, does the $SAFIR_RUNTIME/log directory exist?");
            }
        }

#ifdef HAVE_BOOST_PROPERTY_TREE
        using boost::property_tree::ptree;

        ptree root;
        root.put("logLevel", level);
        root.put("timestamps", useTimestamps);
        root.put("toStdout", toStdout);
        root.put("toFile", toFile);
        root.put("ignoreFlush", ignoreFlush);

        write_ini(GetLogSettingsPath().string(), root );
#else
        boost::filesystem::ofstream ini(GetLogSettingsPath());
        ini << std::boolalpha
            << "logLevel=" << level << "\n"
            << "timestamps=" << useTimestamps << "\n"
            << "toStdout=" << toStdout << "\n"
            << "toFile=" << toFile << "\n"
            << "ignoreFlush=" << ignoreFlush << std::endl;
#endif
    }

    void LowLevelLoggerControl::RemoveIniFile()
    {
        if (LogSettingsFileExists())
        {
            boost::filesystem::remove(GetLogSettingsPath());
        }
    }

}
}
}


