/******************************************************************************
*
* Copyright Saab AB, 2013 (http://safir.sourceforge.net)
*
* Created by: Anders Widén/ anders.widen@consoden.se
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
#include "../../src/Safir/Utilities/Internal/IpcSubscriber.h"
#include <boost/algorithm/string.hpp>
#include <boost/filesystem.hpp>
#include <boost/thread.hpp>
#include <boost/make_shared.hpp>
#include <boost/program_options.hpp>
#include <Safir/Utilities/Internal/StringEncoding.h>
#include <stdlib.h>
#include <iostream>

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4267)
#endif

#include <boost/asio.hpp>

#ifdef _MSC_VER
#pragma warning (pop)
#endif

using Safir::Utilities::Internal::ToUtf16;


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
        : parseOk(false),
          cmdFromStdin(false),
          endpointName(),
          nbrOfMessages(std::numeric_limits<unsigned int>::max()),
          noMessageOutput(false),
          nbrOfLoops()
    {
        using namespace boost::program_options;

        options_description desc("Allowed options");
        desc.add_options()
            ("help,h", "show help message")
            ("cmd-from-stdin,c", value<bool>(&cmdFromStdin)->zero_tokens(), "Read commands from standard input")
            ("endpoint,e", value<std::string>(&endpointName)->default_value("safir_ipc"), "Endpoint name")
            ("number-of-messages,n", value<unsigned int>(&nbrOfMessages), "Number of messages to receive before disconnecting. Default is unlimited")
            ("no-message-output,o", value<bool>(&noMessageOutput)->zero_tokens(), "Don't output the actual message, just the received number of bytes")
            ("number-of-loops,l", value<unsigned int>(&nbrOfLoops)->default_value(1), "Number of connect-receive-disconnect loops")
            ;

        variables_map vm;

        try
        {
            store(command_line_parser(argc, argv).
                  options(desc).run(), vm);
            notify(vm);
        }
        catch (const std::exception& exc)
        {
            std::wcout << L"Error parsing command line: " << exc.what() << "\n" << std::endl;
            ShowHelp(desc);
            return;
        }

        if (vm.count("help"))
        {
            ShowHelp(desc);
            return;
        }

        parseOk = true;
    }
    bool            parseOk;
    bool            cmdFromStdin;
    std::string     endpointName;
    unsigned int    nbrOfMessages;
    bool            noMessageOutput;
    unsigned int    nbrOfLoops;

private:
    static void ShowHelp(const boost::program_options::options_description& desc)
    {
        std::wcout  << std::boolalpha
                    << L"Subscriber test program.\n"
                    << desc << L"\n"
                    << std::endl;
    }

};

struct SubscriberTestPolicy
{
    static void ConnectEvent()
    {
        std::wcout <<  "Trying to connect!" << std::endl;
    }

    static void DisconnectEvent()
    {
        std::wcout <<  "Disconnected from publisher!" << std::endl;
    }

    static void ConnectedToPublisherEvent()
    {
        std::wcout <<  "Connected to publisher!" << std::endl;
    }

    static void DisconnectedFromPublisherEvent()
    {
        std::wcout <<  "Publisher disconnected!" << std::endl;
    }
};

int main(int argc, char* argv[])
{
    std::ios_base::sync_with_stdio(false);

    ProgramOptions po(argc, argv);

    if (!po.parseOk)
    {
        return 1;
    }

    boost::asio::io_service ioService;

    boost::shared_ptr<boost::asio::io_service::work> work (new boost::asio::io_service::work(ioService));

    boost::thread_group threads;
    for (int i = 0; i < 1; ++i)
    {
        threads.create_thread([&ioService](){ioService.run();});
    }


    boost::condition_variable cond;
    boost::mutex mut;
    unsigned int nbrOfMsg = 0;

    auto subPtr = boost::make_shared<Safir::Utilities::Internal::IpcSubscriberImpl<SubscriberTestPolicy>>(
                    ioService,
                    po.endpointName,
                    [&nbrOfMsg, &cond, &mut, po](const char* msg, size_t size)
                    {
                        if (po.noMessageOutput)
                        {
                            std::wcout << L"Received msg with size " << size << std::endl;
                        }
                        else
                        {
                            std::wcout << L"Received msg: " << ToUtf16(std::string(msg, size)) << std::endl;
                        }

                        boost::lock_guard<boost::mutex> lock(mut);
                        ++nbrOfMsg;
                        cond.notify_one();
                    });

    if (po.cmdFromStdin)
    {
        for (;;)
        {
            std::wstring line;
            std::getline(std::wcin, line);

            std::vector<std::wstring> cmd;

            boost::split(cmd, line, boost::is_any_of(L"\t "));

            if (cmd[0] == L"CONNECT")
            {
                subPtr->Connect();
            }
            else if (cmd[0] == L"DISCONNECT")
            {
                subPtr->Disconnect();
            }
            else if (cmd[0] == L"EXIT")
            {
                subPtr->Disconnect();
                break;
            }
            else
            {
                std::wcerr << L"Unknown command: " << cmd[0]  << std::endl;
            }
        }

    }
    else
    {
        for (unsigned int i = 0; i < po.nbrOfLoops; ++i)
        {
            nbrOfMsg = 0;

            subPtr->Connect();

            boost::unique_lock<boost::mutex> lock(mut);
            while(nbrOfMsg < po.nbrOfMessages)
            {
                cond.wait(lock);
            }

            subPtr->Disconnect();

        }
    }

    work.reset();

    threads.join_all();

    return 0;
}


