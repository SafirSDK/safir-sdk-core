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
#include <Safir/Utilities/AceDispatcher.h>
#include <Safir/Dob/Connection.h>
#include <Safir/Dob/NotOpenException.h>
#include <boost/lexical_cast.hpp>
#include <Safir/Application/BackdoorCommand.h>
#include <boost/program_options.hpp>
#include <iostream>

std::string gProgramName;

const ACE_Time_Value EXIT_TIMER_INTERVAL(0,100000);

class StopHandler:
    public Safir::Dob::StopHandler,
    public ACE_Event_Handler
{
public:
    StopHandler():ACE_Event_Handler(ACE_Reactor::instance()){}
    void OnStopOrder(){reactor()->end_reactor_event_loop();}
    int handle_timeout(const ACE_Time_Value & /*currentTime*/, const void * /*act*/)
    {reactor()->end_reactor_event_loop(); return 0;}
};

class MessageSender:
    public Safir::Dob::MessageSender
{
    void OnNotMessageOverflow(){}
};


void PrintHelpAndExit(const boost::program_options::options_description & desc)
{
    std::ostringstream ostr;
    ostr << " " << desc;

    std::wcout
        << "Usage:" << std::endl
        << " " << gProgramName.c_str() << " <options> command\n" << std::endl
        << ostr.str().c_str();
    exit(-1);
}

int main(int argc, char* argv[])
{
    gProgramName = argv[0];
    Safir::Dob::Connection connection;
    Safir::Utilities::AceDispatcher dispatcher(connection);
    StopHandler stopHandler;
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

    //set a small timer so that our message is delivered. When the get the callback we exit this app
    ACE_Reactor::instance()->schedule_timer(&stopHandler,
                                            NULL,
                                            EXIT_TIMER_INTERVAL);
    ACE_Reactor::instance()->run_reactor_event_loop();
    return 0;

}
