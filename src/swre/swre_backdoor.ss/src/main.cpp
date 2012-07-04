/******************************************************************************
*
* Copyright Saab AB, 2006-2008 (http://www.safirsdk.com)
*
* Created by: Samuel Waxin / stsawa
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
#include <Safir/Utilities/AsioDispatcher.h>
#include <Safir/Dob/Connection.h>
#include <Safir/Dob/NotOpenException.h>

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning (disable: 4702)
#pragma warning (disable: 4512)
#endif

#include <boost/lexical_cast.hpp>
#include <boost/program_options.hpp>

#ifdef _MSC_VER
#pragma warning(pop)
#endif

#include <Safir/Application/BackdoorCommand.h>
#include <iostream>

std::string gProgramName;

const boost::posix_time::time_duration EXIT_TIMER_INTERVAL(boost::posix_time::milliseconds(100));

class StopHandler
    : public Safir::Dob::StopHandler
    , private boost::noncopyable
{
public:
    explicit StopHandler(boost::asio::io_service& ioService)
        : m_ioService(ioService) {}
    virtual void OnStopOrder() {m_ioService.stop();}
private:
    boost::asio::io_service& m_ioService;
    
};

class MessageSender:
    public Safir::Dob::MessageSender
{
    void OnNotMessageOverflow(){}
};

void dummy() {}


void PrintHelpAndExit(const boost::program_options::options_description & desc)
{
    std::ostringstream ostr;
    ostr << " " << desc;

    std::wcout
        << "Usage:" << std::endl
        << " " << gProgramName.c_str() << " <options> command\n" << std::endl
        << ostr.str().c_str();
    exit(1);
}

int main(int argc, char* argv[])
{
    gProgramName = argv[0];
    boost::asio::io_service ioService;
    Safir::Dob::Connection connection;
    Safir::Utilities::AsioDispatcher dispatcher(connection, ioService);
    StopHandler stopHandler(ioService);
    MessageSender messageSender;

    int inst = 0;
    for (;;)
    {
        try
        {
            connection.Open (L"Backdoor", boost::lexical_cast<std::wstring>(++inst), 0, &stopHandler, &dispatcher);
            break;
        }
        catch (const Safir::Dob::NotOpenException&)
        {
        }
    }

    //command message were about to send
    Safir::Application::BackdoorCommandPtr commandMsg = Safir::Application::BackdoorCommand::Create();

    // Declare the supported options.
    boost::program_options::options_description desc("Allowed Options");
    desc.add_options()
        ("help,h", "produce help message")
        ("ConnectionName,c", boost::program_options::value< std::string >(), "the connection name")
        ("NodeName,n", boost::program_options::value< std::string >(), "the node name");

    // Hidden options, will be allowed both on command line and
    // in config file, but will not be shown to the user.
    boost::program_options::options_description hidden("Hidden options");
    hidden.add_options()
        ("Command", boost::program_options::value< std::vector < std::string> >() , "the command");


    // command is a option without a switch as prefix
    boost::program_options::positional_options_description p;
    p.add("Command", -1);

    // paste the 2 command sets together
    boost::program_options::options_description cmdline_options;
    cmdline_options.add(desc).add(hidden);

    //if no args given, show help and exit
    if (argc == 1)
    {
        PrintHelpAndExit(desc);
    }

    // read the options from the 2 sets of  args into the vm map.
    boost::program_options::variables_map vm;
    try
    {
        boost::program_options::store(boost::program_options::command_line_parser(argc,argv).
                                      options(cmdline_options).positional(p).run(), vm);
    }
    catch (boost::program_options::error ex)
    {
        PrintHelpAndExit(desc);
    }

    boost::program_options::notify(vm);


    if (vm.count("Help"))
    {
        PrintHelpAndExit(desc);
    }

    if (vm.count("Command"))
    {
        std::wstring command;
        for(unsigned int i=0;i < vm["Command"].as< std::vector < std::string> >().size(); i++)
        {
            command.append(Safir::Dob::Typesystem::Utilities::ToWstring(vm["Command"].as< std::vector < std::string> >()[i]));

            if(i+1 != vm["Command"].as< std::vector < std::string> >().size())
            {
                command.append(L" ");
            }

        }

        //set the command in the message
        commandMsg->Command().SetVal(command);

    }
    else
    {
        PrintHelpAndExit(desc);
    }

    if (vm.count("ConnectionName"))
    {
        commandMsg->ConnectionName().SetVal(Safir::Dob::Typesystem::Utilities::ToWstring(vm["ConnectionName"].as< std::string >()) );
    }

    if (vm.count("NodeName"))
    {
        commandMsg->NodeName().SetVal(Safir::Dob::Typesystem::Utilities::ToWstring(vm["NodeName"].as< std::string >()) );
    }

    // send the message
    connection.Send(commandMsg, Safir::Dob::Typesystem::ChannelId(), &messageSender);

    //set a timer
    boost::asio::deadline_timer timer(ioService,EXIT_TIMER_INTERVAL);
    timer.async_wait(boost::bind(dummy));

    //ioService will only run until the timer has timed out, since that is all the work
    //that there is for it.
    ioService.run();

    return 0;
}
