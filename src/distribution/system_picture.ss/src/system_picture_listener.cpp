/******************************************************************************
*
* Copyright Saab AB, 2013 (http://safir.sourceforge.net)
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
#include <Safir/Dob/Internal/SystemPicture.h>
#include <Safir/Utilities/Internal/AsioPeriodicTimer.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/MakeUnique.h>
#include <memory>

//disable warnings in boost
#if defined _MSC_VER
  #pragma warning (push)
  #pragma warning (disable : 4100)
  #pragma warning (disable : 4267)
#endif

#include <boost/program_options.hpp>
#include <boost/asio.hpp>

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
            ("raw",
             "Subscribe to the Raw data instead of the SystemState data");

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

        raw = vm.count("raw") != 0;

        parseOk = true;
    }
    bool parseOk;

    bool raw;

private:
    static void ShowHelp(const boost::program_options::options_description& desc)
    {
        std::wcout << std::boolalpha
                   << "Usage: control [OPTIONS]\n"
                   << desc
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

    boost::asio::io_service ioService;

    auto wk = Safir::make_unique<boost::asio::io_service::work>(ioService);

    boost::asio::signal_set signals(ioService);

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

    Safir::Dob::Internal::SP::SystemPicture sp(Safir::Dob::Internal::SP::subscriber_tag, ioService);

    if (options.raw)
    {
        sp.StartRawSubscription([](const Safir::Dob::Internal::SP::RawStatistics& data)
                                {
                                    std::wcout << data << std::endl;
                                });
    }
    else
    {
        sp.StartStateSubscription([](const Safir::Dob::Internal::SP::SystemState& data)
                                  {
                                      std::wcout << data << std::endl;
                                  });
    }

    signals.async_wait([&](const boost::system::error_code& error,
                           const int /*signal_number*/)
                       {
                           if (error)
                           {
                               return;
                           }
                           sp.Stop();
                           wk.reset();
                       });

    ioService.run();
    return 0;
}
