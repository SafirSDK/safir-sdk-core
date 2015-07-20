/******************************************************************************
*
* Copyright Saab AB, 2006-2015 (http://safir.sourceforge.net)
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

#ifndef __COMM_OPT_H__
#define __COMM_OPT_H__
#include <iostream>
#include <boost/noncopyable.hpp>

#include "../common/CommonIncludes.h"

class CommandLine:
    private boost::noncopyable
{
public:
    static CommandLine & Instance()
    {
        static CommandLine obj;
        return obj;
    }

    bool Parse(int argc, char* argv[])
    {
        namespace po = boost::program_options;
        // Declare the supported options.
        std::wcout << std::boolalpha;
        try
        {
            po::options_description general("Allowed options");
            po::options_description all ("Allowed options");
            all.add_options()
                ("help,h", "produce this help message")
                ("attempts,a", po::value<int>(&m_attempts)->default_value(5),"total number of connection attempts that should be made")
                ("timeout,t", po::value<int>(&m_timeout)->default_value(0),"milliseconds between connection attempts")
                ("instance,i", po::value<int>(&m_instance)->default_value(0),"connection instance to use")
                ("show menu,m", "shows the menu after the tests")
                ;

            po::store(po::parse_command_line(argc,argv,all),m_variablesMap);
            po::notify(m_variablesMap);

            if (m_variablesMap.count("help"))
            {
                std::wcout << all << std::endl;
                return false;
            }

            m_showMenu = false;
            if (m_variablesMap.count("show menu"))
            {
                m_showMenu = true;
            }

        }
        catch (const std::exception & e)
        {
            std::wcerr << "Parse of command line failed: " << std::endl
                << e.what() << std::endl;
            return false;
        }
        return true;
    }

    const boost::program_options::variables_map & Options() const {return m_variablesMap;}

    int Timeout() {return m_timeout;}
    int Attempts() {return m_attempts;}
    int ConnectionInstance() {return m_instance;}
    bool ShowMenu()  {return m_showMenu;}



private:
    CommandLine() {}
    ~CommandLine() {}

    boost::program_options::variables_map m_variablesMap;

    int m_timeout;
    int m_attempts;
    int m_instance;
    bool m_showMenu;

};
#endif
