/******************************************************************************
*
* Copyright Saab AB, 2007-2011 (http://www.safirsdk.com)
*
* Created by: Joel Ottosson / joot
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
#include <boost/program_options.hpp>
#include "EntityCruncher.h"


int main(int argc, char* argv[])
{ 
    //----------------------
    //Command line parsing
    //----------------------
    namespace po = boost::program_options;
    po::options_description desc;
    desc.add_options()
    ("help,h", "produce help message")
    ("error-time,e", po::value<int>(), "set time before start reporting error");

    po::variables_map vm;    
    po::store(po::parse_command_line(argc, argv, desc), vm);
    po::notify(vm);
    
    if (vm.count("help"))
    {
        std::cout<<desc<<std::endl;
        return 0;
    }
    
    if (vm.count("error-time"))
    {
        Supervisor::TimeThreshold = vm["error-time"].as<int>();
    }

    //----------------------
    //Run program
    //----------------------
    EntityCruncher ec;    
    ec.RunReactor();
    return 0;
}

