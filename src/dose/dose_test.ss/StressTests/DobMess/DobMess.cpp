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
#if defined _MSC_VER
  #pragma warning (push)
  #pragma warning (disable : 4251 4275 4512)
#endif

#include <boost/program_options.hpp>

#if defined _MSC_VER
  #pragma warning (pop)
#endif

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
    ("verbose,v", "verbose mode")
    ("error-time,e", po::value<int>(), "set time before start reporting error")
    ("num-ent,n", po::value<int>(), "set max number of entities at the same time. Default 500, -1=inf")
    ("no-create,c", "never create entities, only subscribe and sen request to others")
    ("no-delete,d", "never delete entities, when max num entites is reached no new entities will be created, instead the existing entities will be continously updated");

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

    int numEnt = vm.count("num-ent")>0 ? numEnt=vm["num-ent"].as<int>() : 500; //default 500 entities
    bool deleteEnabled = vm.count("no-delete")<=0;
    bool createEnabled = vm.count("no-create")<=0;
    bool verbose = vm.count("verbose")>0;

    //----------------------
    //Run program
    //----------------------
    EntityCruncher ec(numEnt, createEnabled, deleteEnabled, verbose);    
    ec.RunReactor();
    return 0;
}

