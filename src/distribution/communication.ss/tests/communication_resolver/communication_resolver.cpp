/******************************************************************************
*
* Copyright Saab AB, 2015 (http://safirsdkcore.com)
*
* Created by: Joel Ottosson / joel.ottosson@consoden.se
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
#include <iostream>
#include <boost/program_options.hpp>
#include "../../src/include/Safir/Dob/Internal/Resolver.h"

std::wostream& operator<<(std::wostream& out, const boost::program_options::options_description& opt)
{
    std::ostringstream ostr;
    ostr << opt;
    return out << ostr.str().c_str();
}

class Cmd
{
public:
    Cmd(int argc, char * argv[]) :
        help(false),
        verbose(false),
        local(false),
        expr()
    {
        boost::program_options::options_description desc("Command line options");
        desc.add_options()
                ("help,h", "Produce help message")
                ("verbose,v", "Verbose mode")
                ("local,l", "Resolve expression to an interface on the local machine")
                ("arguments",  boost::program_options::value<std::string>(), "Expression to resolve");

        boost::program_options::positional_options_description pd;
        pd.add("arguments", 1);

        boost::program_options::variables_map vm;
        boost::program_options::store(boost::program_options::command_line_parser(argc, argv).options(desc).positional(pd).run(), vm);
        boost::program_options::notify(vm);

        if (vm.count("help"))
        {
            help=true;
            std::wcout<<desc<<std::endl;
            return;
        }

        if (vm.count("local"))
        {
            local=true;
        }
        if (vm.count("verbose"))
        {
            verbose=true;
        }
        if (vm.count("arguments")==1)
        {
            expr=vm["arguments"].as<std::string>();
        }
        else
        {
            std::wcout<<"Must specify exactly one argument to resolve."<<std::endl;
            help=true;
        }
    }

    bool help;
    bool verbose;
    bool local;
    std::string expr;
};

std::string RemovePort(const std::string& s)
{
    auto i=s.find_last_of(":");
    if (i!=s.npos)
        return s.substr(0, i);
    return s;
}

int main(int argc, char * argv[])
{
    Cmd cmd(argc, argv);
    if (cmd.help) return 0; //only show help

    boost::function<void()> fun;
    if (cmd.local)
    {
        try
        {
            std::wcout<<"Resolved to local address: "<<
                RemovePort(Safir::Dob::Internal::Com::Resolver::ResolveLocalEndpoint(cmd.expr+":10000"))<<std::endl;
        }
        catch (const std::logic_error& e)
        {
            std::wcout<<e.what()<<std::endl;
        }
    }
    else
    {
        boost::asio::io_context ioContext;
        Safir::Dob::Internal::Com::Resolver resolver(ioContext, cmd.verbose);

        boost::asio::post(ioContext, [&]
        {
            try
            {
                std::wcout<<"Resolved "<<cmd.expr<<" to: "<<
                            RemovePort(resolver.ResolveRemoteEndpoint(cmd.expr+":10000", 4))<<std::endl;
            }
            catch (const std::logic_error& e)
            {
                std::wcout<<e.what()<<std::endl;
            }
        });
        ioContext.run();
    }

    return 0;
}

