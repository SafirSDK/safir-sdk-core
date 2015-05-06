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
#if defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
 #include <Safir/Utilities/Internal/IpcAcceptorWin32.h>
#elif defined(linux) || defined(__linux) || defined(__linux__)
 #include <Safir/Utilities/Internal/IpcAcceptorLinux.h>
#endif

#include <Safir/Utilities/Internal/IpcPublisher.h>
#include <Safir/Utilities/Internal/StringEncoding.h>
#include <boost/algorithm/string.hpp>
#include <boost/filesystem.hpp>
#include <boost/thread.hpp>
#include <boost/make_shared.hpp>
#include <boost/program_options.hpp>
#include <stdlib.h>
#include <string>
#include <iostream>

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4267)
#endif

#include <boost/asio.hpp>

#ifdef _MSC_VER
#pragma warning (pop)
#endif


std::wostream& operator<<(std::wostream& out, const boost::program_options::options_description& opt)
{
    std::ostringstream ostr;
    ostr << opt;
    return out << ostr.str().c_str();
}

std::unique_ptr<char[]> StrToPtr(const std::wstring& str)
{
    std::string s = Safir::Utilities::Internal::ToUtf8(str);

    std::unique_ptr<char[]> ptr(new char[s.length()]);

    memcpy(ptr.get(), s.c_str(), s.length());
    return ptr;
}

class ProgramOptions
{
public:
    ProgramOptions(int argc, char* argv[])
        : parseOk(false),
          cmdFromStdin(false),
          endpointName(),
          message(),
          largeMessageSize(0),
          delay(),
          nbrOfMessages(std::numeric_limits<unsigned int>::max()),
          nbrOfLoops()
    {
        using namespace boost::program_options;

        options_description desc("Allowed options");
        desc.add_options()
            ("help,h", "show help message")
            ("cmd-from-stdin, ", value<bool>(&cmdFromStdin)->zero_tokens(), "Read commands from standard input")
            ("endpoint,e", value<std::string>(&endpointName)->default_value("safir_ipc"), "Endpoint name")
            ("message,m", value<std::string>(&message)->default_value("Kalle"), "Message sent by Publisher")
            ("large-message-size,q", value<unsigned int>(&largeMessageSize)->default_value(0), "Send a message with the given size")
            ("message-delay,d", value<unsigned int>(&delay)->default_value(1000), "Delay in milliseconds between messages")
            ("number-of-messages,n", value<unsigned int>(&nbrOfMessages), "Number of messages to send before stopping. Default is unlimited")
            ("number-of-loops,l", value<unsigned int>(&nbrOfLoops)->default_value(1), "Number of start-send-stop loops")
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
            std::wcout << "Error parsing command line: " << exc.what() << "\n" << std::endl;
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
    std::string     message;
    unsigned int    largeMessageSize;
    unsigned int    delay;
    unsigned int    nbrOfMessages;
    unsigned int    nbrOfLoops;

private:
    static void ShowHelp(const boost::program_options::options_description& desc)
    {
        std::wcout << std::boolalpha
                   << L"Publisher test program.\n"
                   << desc << L"\n"
                   << std::endl;
    }

};

struct PublisherTestPolicy
{
    static void StartListeningEvent()
    {
        std::wcout <<  "Publisher is started!" << std::endl;
    }

    static void StopListeningEvent()
    {
        std::wcout <<  "Publisher is stopped!" << std::endl;
    }
};

#if defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
typedef Safir::Utilities::Internal::IpcPublisherImpl<PublisherTestPolicy, Safir::Utilities::Internal::Win32Acceptor> IpcPublisher;
#elif defined(linux) || defined(__linux) || defined(__linux__)
typedef Safir::Utilities::Internal::IpcPublisherImpl<PublisherTestPolicy, Safir::Utilities::Internal::LinuxAcceptor> IpcPublisher;
#endif


int main(int argc, char* argv[])
{
    ProgramOptions po(argc, argv);

    if (!po.parseOk)
    {
        return 1;
    }

    boost::asio::io_service ioService;
    boost::asio::io_service::strand strand(ioService);

    boost::shared_ptr<boost::asio::io_service::work> work (new boost::asio::io_service::work(ioService));

    auto pubPtr = boost::make_shared<IpcPublisher>(ioService,
                                                   po.endpointName,
                                                   strand.wrap([](){std::wcout <<  "A Subscriber connected!" << std::endl;}),
                                                   strand.wrap([](){std::wcout <<  "A Subscriber disconnected!" << std::endl;}));

    boost::thread_group threads;
    for (int i = 0; i < 9; ++i)
    {
        threads.create_thread([&ioService](){ioService.run();});
    }

    if (po.cmdFromStdin)
    {
        for (;;)
        {
            std::wstring line;
            std::getline(std::wcin, line);

            //std::wcout << "getline read: " << line << " Line length:" << line.length() << std::endl;

            std::vector<std::wstring> cmd;

            boost::split(cmd, line, boost::is_any_of(L"\t "));

            //std::wcout << "Received cmd: " << cmd[0] << std::endl;

            if (cmd[0] == L"START")
            {
                pubPtr->Start();
            }
            else if (cmd[0] == L"STOP")
            {
                pubPtr->Stop();
            }
            else if (cmd[0] == L"SEND")
            {
                //std::wcout << cmd[0] << "-" << cmd[1] << "-" << cmd[2] << "-" << cmd[3] << std::endl;

                auto n = stoul(cmd[2]);

                while (n > 0)
                {
                    pubPtr->Send(StrToPtr(cmd[1]), static_cast<uint32_t>(cmd[1].length()));
                    boost::this_thread::sleep_for(boost::chrono::milliseconds(stoul(cmd[3])));
                    --n;
                }
            }
            else if (cmd[0] == L"EXIT")
            {
                pubPtr->Stop();
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
        std::unique_ptr<char[]> msgPtr;
        uint32_t msgLength;

        for (unsigned int i = 0; i < po.nbrOfLoops; ++i)
        {
            pubPtr->Start();

            for (unsigned int i = 0; i < po.nbrOfMessages; ++i)
            {
                if (po.largeMessageSize > 0)
                {
                    // To minimize the overhead for very large messages we just
                    // create an uninitialized buffer in this case.
                    msgPtr = std::unique_ptr<char[]>(new char[po.largeMessageSize]);
                    msgLength = po.largeMessageSize;
                }
                else
                {
                    msgPtr = StrToPtr(Safir::Utilities::Internal::ToUtf16(po.message));
                    msgLength = static_cast<uint32_t>(po.message.length());
                }

                pubPtr->Send(std::move(msgPtr), msgLength);

                boost::this_thread::sleep_for(boost::chrono::milliseconds(po.delay));
            }

            pubPtr->Stop();
        }
    }

    work.reset();

    threads.join_all();

    return 0;
}


