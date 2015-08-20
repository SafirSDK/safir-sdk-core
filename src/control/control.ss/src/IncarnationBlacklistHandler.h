/******************************************************************************
*
* Copyright Saab AB, 2014-2015 (http://safir.sourceforge.net)
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

#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/path.hpp>
#include <fstream>


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

        IncarnationBlacklistHandler(std::string fileName)
            : m_path(fileName)
        {
            CheckConfigurationFile(m_path); //throws if errors detected
        }

    private:

        void CreateConfigurationFile(boost::filesystem::path path)
        {
            std::ofstream file(path.string().c_str(),std::ios::out);

            file.close();
        }

        void CheckConfigurationFile(boost::filesystem::path path)
        {
            if (boost::filesystem::exists(path))
            {
                //path exists
                if ( boost::filesystem::is_regular_file(path) == false)
                {
                    throw std::logic_error("Configuration error: IncarnationBlacklistFilename parameter should point to a file, not a directory!");
                }
            }
            else
            {
                CreateConfigurationFile(path); //throws if unable to create
            }
        }

        boost::filesystem::path m_path;
    };


}
}
}
}
