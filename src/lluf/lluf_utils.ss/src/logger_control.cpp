/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
*
* Created by: Lars Hagström / stlrha
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
#include <Safir/Utilities/Internal/LowLevelLoggerControl.h>

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable:4100)
#endif

#include <boost/program_options.hpp>
#include <boost/filesystem/convenience.hpp>
#include <boost/filesystem/operations.hpp>

#ifdef _MSC_VER
#pragma warning (pop)
#endif

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
        , logLevel(false)
        , ignoreFlush(false)
        , noTimestamps(false)
        , noStdout(false)
    {
        using namespace boost::program_options;

        options_description general("General Options");
        general.add_options()
            ("help,h", "show help message");

        options_description overhead("Options that reduce logging overhead");
        overhead.add_options()
            ("ignore-flush,i", value<bool>(&ignoreFlush)->zero_tokens(), "Ignore flushes, and write only when buffer is full.")
            ("no-timestamps,t", value<bool>(&noTimestamps)->zero_tokens(), "Don't put timestamps on each line in the logs.")
            ("no-stdout,s", value<bool>(&noStdout)->zero_tokens(), "Don't log to stdout.");

        options_description hidden("Hidden options");
        hidden.add_options()
            ("log-level", value<int>(&logLevel));

        options_description all_options;
        all_options.add(general).add(overhead).add(hidden);

        options_description visible_options;
        visible_options.add(general).add(overhead);

        positional_options_description positional;
        positional.add("log-level", 1);

        variables_map vm;

        try
        {
            store(command_line_parser(argc, argv).
                  options(all_options).positional(positional).run(), vm);
            notify(vm);
        }
        catch (const std::exception& exc)
        {
            std::wcout << "Error parsing command line: " << exc.what() << "\n" << std::endl;
            ShowHelp(visible_options);
            return;
        }

        if (vm.count("help"))
        {
            ShowHelp(visible_options);
            return;
        }

        if (vm.count("log-level") == 0 || logLevel < 0 || logLevel >9)
        {
            std::wcout << "Logging level has to be between 0 and 9.\n" << std::endl;
            ShowHelp(visible_options);
            return;
        }

        parseOk = true;
    }
    bool parseOk;

    int logLevel;

    bool ignoreFlush;
    bool noTimestamps;
    bool noStdout;

private:
    static void ShowHelp(const boost::program_options::options_description& desc)
    {
        std::wcout << std::boolalpha
                   << "Usage: logger_control [OPTIONS] <level>\n"
                   << "Control logging level and options.\n\n"
                   << "Log files are found in the directory configured in typesystem.ini\n"
                   << "The log directory must exist for it to be possible to turn on logging.\n\n"
                   << "Logging is only turned on for the current session (i.e. settings are reset\n"
                   << "to defaults when no program using the logger is running), but logging can\n"
                   << "be enabled permanently by editing the logging.ini file.\n\n"
                   << "  <level> is a number between 0 and 9, where 0 is no logging and 9 is very\n"
                   << "          verbose logging.\n"
                   << desc << "\n"
                   << "Examples:\n"
                   << "  Set logging level to 5.\n"
                   << "    logger_control 5\n"
                   << "  Set logging level to 7 with no timestamps at the beginning of each line.\n"
                   << "    logger_control -t 7\n"
                   << std::endl;
    }

};


int main(int argc, char * argv[])
{
    const ProgramOptions options(argc,argv);

    if (!options.parseOk)
    {
        return 1;
    }

    try
    {
        Safir::Utilities::Internal::LowLevelLoggerControl control(true, true);
        /*TODO        if (control.Disabled())
        {
            std::wcout << "LowLevelLogger is currently disabled, please enable it in your logging.ini file." << std::endl;
            return 1;
            }*/
        control.LogLevel(options.logLevel);
        std::wcout << "Log level should now be " << options.logLevel << std::endl;
        control.UseTimestamps(!options.noTimestamps);
        control.LogToStdout(!options.noStdout);
        control.IgnoreFlush(options.ignoreFlush);
    }
    catch (const std::exception &)
    {
        std::wcout << "Failed to change logging options for current session. Is any app using the logger running?" << std::endl;
        return 1;
    }
    return 0;
}
