/******************************************************************************
*
* Copyright Saab AB, 2014-2015 (http://safirsdkcore.com)
*
* Created by: Samuel Waxin / samuel.waxin@consoden.se
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

#include <Safir/Utilities/Internal/SystemLog.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/path.hpp>
#include <fstream>
#include <iostream>


namespace Safir
{
namespace Dob
{
namespace Internal
{
namespace Control
{
    /**
    * @brief The IncarnationBlacklistHandler class holds handles the incarnation blacklist file
    * and can be used to check if a given incarnation is blacklisted.
    */
    class IncarnationBlacklistHandler
    {
    public:

        explicit IncarnationBlacklistHandler(const std::string& fileName)
            : m_path(fileName)
        {
            CheckConfigurationFile(m_path); //throws if errors detected
        }

        //returns true if the incarnationId is not blacklisted
        bool ValidateIncarnationId(const int64_t incarnationId) const
        {
            std::ifstream file(m_path.string().c_str(), std::iostream::in);

            if (!file.is_open())
            {
                return true;
            }

            while (!file.eof())
            {
                int64_t readValue;

                file >> readValue;

                if (readValue == incarnationId)
                {
                    lllog(3) << "Found incarnationId " << incarnationId
                             << " in blacklist " << m_path.string().c_str() << std::endl;
                    file.close();
                    return false;
                }
            }

            file.close();

            return true;
        }

        void AddIncarnationId(const int64_t incarnationId)
        {

            if(ValidateIncarnationId(incarnationId) == false) //only add if it's not already added
            {
                return;
            }

            std::ofstream file(m_path.string().c_str(), std::ofstream::out | std::ofstream::app);

            if (!file.is_open())
            {
                SEND_SYSTEM_LOG(Error,
                                << "Configuration error: Unable to write incarnation id to black list file '" <<
                                m_path
                                << "' configured via the Safir::Dob::NodeParameters::IncarnationBlacklistFilename "
                                << "parameter. Please check configuration and permissions.");
                return;
            }

            file << incarnationId << std::endl;
            file.close();
        }

    private:

        void TryOpenCreateFile(const boost::filesystem::path& path)
        {
            std::ofstream file(path.string().c_str(), std::ofstream::out | std::ofstream::app);

            if (!file.is_open())
            {
                throw std::runtime_error("Configuration error: Unable to write to incarnation black list file '"
                                         + m_path.string()
                                         + "' configured via the Safir::Dob::NodeParameters::IncarnationBlacklistFilename "
                                         + " parameter. Please check configuration and permissions.");
            }

            file.close();

            //make the file world read-writeable
            using namespace boost::filesystem;
            permissions(path,
                        owner_read  | owner_write  |
                        group_read  | group_write  |
                        others_read | others_write );
        }

        void CheckConfigurationFile(const boost::filesystem::path& path)
        {
            if (boost::filesystem::exists(path))
            {
                //path exists
                if (!boost::filesystem::is_regular_file(path))
                {
                    //path is directory not file
                    throw std::runtime_error("Configuration error: Safir::Dob::NodeParameters::"
                                             "IncarnationBlacklistFilename parameter should point to a "
                                             "file, not a directory!");
                }

                //file exists, try open it
                TryOpenCreateFile(path); //throws if we cant open with write access
            }
            else
            {
                TryOpenCreateFile(path); //throws if unable to create
            }
        }

        const boost::filesystem::path m_path;
    };


}
}
}
}
