/******************************************************************************
*
* Copyright Saab AB, 2012 (http://safir.sourceforge.net)
*
* Created by: Lars Hagström / lars.hagstrom@consoden.se
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
#include <Safir/Dob/Internal/Communication.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/SystemLog.h>
#include <Safir/Dob/Internal/SystemPicture.h>
#include <Safir/Utilities/Internal/Id.h>
#include <iostream>
#include <map>
#include <boost/asio.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/thread.hpp>
#include "Config.h"

//disable warnings in boost
#if defined _MSC_VER
  #pragma warning (push)
  #pragma warning (disable : 4100)
#endif

#include <boost/program_options.hpp>

#if defined _MSC_VER
  #pragma warning (pop)
#endif


std::wostream& operator<<(std::wostream& out, const std::string& str)
{
    return out << str.c_str();
}

std::wostream& operator<<(std::wostream& out, const boost::program_options::options_description& opt)
{
    std::ostringstream ostr;
    ostr << opt;
    return out << ostr.str().c_str();
}


class ProgramOptions
{
public:
    ProgramOptions(int argc, char* argv[])
        : parseOk(false)
    {
        using namespace boost::program_options;
        options_description options("Options");
        options.add_options()
            ("help,h", "show help message")
            ("control-address,c", 
             value<std::string>(&controlAddress)->default_value("0.0.0.0:30000"), 
             "Address and port of the control channel")
            ("data-address,c", 
             value<std::string>(&dataAddress)->default_value("0.0.0.0:40000"), 
             "Address and port of the data channel")
            ("seed,s", 
             value<std::vector<std::string> >(&seeds), 
             "Seed address (can be specified multiple times). Pairs of address:port to control channel.")
            ("name,n", 
             value<std::string>(&name)->default_value("<not set>", ""), 
             "A nice name for the node, for presentation purposes only")
            ("node-type,t", 
             value<std::string>(&nodeType)->default_value("Server"), 
             "Node type of this node")
            ("force-id", 
             value<boost::int64_t>(&id)->default_value(LlufId_GenerateRandom64(), ""), 
             "Override the automatically generated node id. For debugging/testing purposes only.");
        
        variables_map vm;

        try
        {
            store(command_line_parser(argc, argv).
                  options(options).run(), vm);
            notify(vm);
        }
        catch (const std::exception& exc)
        {
            std::wcerr << "Error parsing command line: " << exc.what() << "\n" << std::endl;
            ShowHelp(options);
            return;
        }

        if (vm.count("help"))
        {
            ShowHelp(options);
            return;
        }

        for (const auto& nt: config.GetNodeTypes())
        {
            if (nt.name == nodeType)
            {
                nodeTypeId = nt.id;
                break;
            }
        }
        
        if (nodeTypeId == -1)
        {
            std::wcerr << "Invalid nodeType specified" << std::endl;
            return;
        }

        parseOk = true;
    }
    bool parseOk;
    
    const Safir::Dob::Internal::Control::Config config;

    std::string controlAddress;
    std::string dataAddress;
    std::vector<std::string> seeds;
    boost::int64_t id;
    std::string name;
    std::string nodeType;
    boost::int64_t nodeTypeId = -1;
private:
    static void ShowHelp(const boost::program_options::options_description& desc)
    {
        std::wcout << std::boolalpha
                   << "Usage: control [OPTIONS]\n"
                   << desc << "\n"
                   << "Examples:\n"
                   << "  Listen to loopback address and ports 33000 and 43000.\n"
                   << "    control --controlAddress='127.0.0.1:33000' --data-address='127.0.0.1:43000'\n"
                   << "  As above, but all addresses.\n"
                   << "    control --controlAddress='0.0.0.0:33000' --data-address='0.0.0.0:43000'\n"
                   << "  Listen to ipv6 loopback.\n"
                   << "    control --controlAddress='[::1]:33000' --data-address='[::1]:43000'\n"
                   << std::endl;
    }

};


int main(int argc, char * argv[])
{
    const ProgramOptions options(argc, argv);
    if (!options.parseOk)
    {
        return 1;
    }

    boost::shared_ptr<boost::asio::io_service> ioService(new boost::asio::io_service());

    //make some work to stop io_service from exiting.
    boost::shared_ptr<boost::asio::io_service::work> work(new boost::asio::io_service::work(*ioService));

    boost::shared_ptr<Safir::Dob::Internal::Com::Communication> communication;

    try
    {
        std::vector<Safir::Dob::Internal::Com::Communication::NodeType> nodeTypes;

        for (const auto& nt: options.config.GetNodeTypes())
        {
            nodeTypes.push_back({nt.id, nt.name, nt.multicastAddress, nt.heartbeatInterval, nt.retryTimeout});
        }
        
        communication.reset(new Safir::Dob::Internal::Com::Communication
                            (ioService,
                             options.name,
                             options.id,
                             options.nodeTypeId,
                             options.controlAddress,
                             options.dataAddress,
                             true, //this is the control channel
                             nodeTypes)); 

        communication->InjectSeeds(options.seeds);
    }
    catch (const std::invalid_argument& e)
    {
        std::wcerr << "Address does not appear to be valid. Got exception:\n" 
                   << e.what()
                   << std::endl;
        
        return 1;
    }


    std::map<boost::int64_t, Safir::Dob::Internal::SP::NodeType> nodeTypes;
    
    for (const auto& nt: options.config.GetNodeTypes())
    {
        nodeTypes.insert(std::make_pair(nt.id, 
                                        Safir::Dob::Internal::SP::NodeType(nt.id, nt.name, nt.isLight, nt.heartbeatInterval, nt.retryTimeout)));
    }


    Safir::Dob::Internal::SP::SystemPicture sp(Safir::Dob::Internal::SP::master_tag, 
                                               ioService, 
                                               communication,
                                               options.name,
                                               options.id,
                                               options.nodeTypeId,
                                               options.controlAddress,
                                               options.dataAddress,
                                               nodeTypes);


    communication->Start();



    boost::asio::signal_set signals(*ioService);
    
#if defined (_WIN32)
    signals.add(SIGABRT);
    signals.add(SIGBREAK);
    signals.add(SIGINT);
    signals.add(SIGTERM);
#else
    signals.add(SIGQUIT);
    signals.add(SIGINT);
    signals.add(SIGTERM);
#endif

    signals.async_wait([&sp,&work,communication,&signals](const boost::system::error_code& error,
                                   const int /*signal_number*/)
                       {
                           if (!!error) //double not to remove spurious vs2010 warning
                           {
                               SEND_SYSTEM_LOG(Error,
                                               << "Got a signals error: " << error);
                           }
                           sp.Stop();
                           communication->Stop();
                           work.reset();
                       }
                       );



    boost::thread_group threads;
    for (int i = 0; i < 9; ++i)
    {
        threads.create_thread([ioService]{ioService->run();});
    }

    ioService->run();

    threads.join_all();

    lllog(3) << "Exiting..." << std::endl;
    return 0;
}
