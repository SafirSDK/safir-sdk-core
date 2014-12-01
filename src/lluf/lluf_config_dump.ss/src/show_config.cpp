/******************************************************************************
*
* Copyright Saab AB, 2013 (http://safir.sourceforge.net)
*
* Created by: Anders Widén <anders.widen@consoden.se>
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

#include <Safir/Utilities/Internal/ConfigReader.h>
#include <boost/filesystem/path.hpp>

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable:4100)
#endif

#include <boost/program_options.hpp>

#ifdef _MSC_VER
#pragma warning (pop)
#endif

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
            ("typesystem", "Show contents of typesystem.ini")
            ("locations", "Show contents of locations.ini")
            ("logging", "Show contents of logging.ini")
            ("dou-install-dirs", "Show the dou install dirs of all modules defined in typesystem.ini")
            ("module-install-dir", value<std::string>(&moduledir),
             "Get the install dir from typesystem.ini for the specified module")
            ("module-dependencies", value<std::string>(&moduledep),
             "Get the all dependencies from typesystem.ini for the specified module");

        variables_map vm;

        try
        {
            store(command_line_parser(argc, argv).
                  options(options).run(), vm);
            notify(vm);
        }
        catch (const std::exception& exc)
        {
            std::cout << "Error parsing command line: " << exc.what() << "\n" << std::endl;
            ShowHelp(options);
            return;
        }

        if (vm.count("help"))
        {
            ShowHelp(options);
            return;
        }

        logging = vm.count("logging") != 0;
        typesystem = vm.count("typesystem") != 0;
        locations = vm.count("locations") != 0;
        dou_install_dirs = vm.count("dou-install-dirs") != 0;

        if (!logging &&
            !locations &&
            !typesystem &&
            !dou_install_dirs &&
            vm.count("module-install-dir") == 0 &&
            vm.count("module-dependencies") == 0)
        {
            ShowHelp(options);
            return;
        }

        parseOk = true;
    }
    bool logging;
    bool typesystem;
    bool locations;
    bool dou_install_dirs;
    std::string moduledir;
    std::string moduledep;

    bool parseOk;

private:
    static void ShowHelp(const boost::program_options::options_description& desc)
    {
        std::cout << std::boolalpha
                  << "Writes the Safir SDK ini file configuration to standard output.\n"
                  << desc << "\n"
                  << std::endl;
    }

};

static void PrintPTree (const boost::property_tree::ptree& ptree)
{
    for (boost::property_tree::ptree::const_iterator it = ptree.begin();
         it != ptree.end(); ++it)
    {
        const bool isSection = !it->second.empty();

        if (isSection)
        {
            std::cout << '[' << it->first << ']' << std::endl;
            PrintPTree(it->second);
        }
        else
        {
            std::cout << it->first << '=' << it->second.get_value<std::string>() << std::endl;
        }
    }
}


int main(int argc, char * argv[])
{
    const ProgramOptions options(argc,argv);

    if (!options.parseOk)
    {
        return 1;
    }

    try
    {
        Safir::Utilities::Internal::ConfigReader reader;

        if (options.locations)
        {
            std::cout << "; ==== locations.ini ====" << std::endl;
            PrintPTree(reader.Locations());
        }
        if (options.logging)
        {
            std::cout << "; ==== logging.ini ====" << std::endl;
            PrintPTree(reader.Logging());
        }
        if (options.typesystem)
        {
            std::cout << "; ==== typesystem.ini ====" << std::endl;
            PrintPTree(reader.Typesystem());
        }

        if (options.dou_install_dirs)
        {
            const std::vector<std::pair<std::string,std::string> > dirs =
                Safir::Utilities::Internal::ConfigHelper::GetDouDirectories(reader);
            for (std::vector<std::pair<std::string,std::string> >::const_iterator it = dirs.begin();
                 it != dirs.end(); ++it)
            {
                std::cout << it->first << "=" << it->second << std::endl;
            }
        }

        if (!options.moduledir.empty())
        {
            const std::vector<std::pair<std::string,std::string> > dirs =
                Safir::Utilities::Internal::ConfigHelper::GetDouDirectories(reader);
            for (std::vector<std::pair<std::string,std::string> >::const_iterator it = dirs.begin();
                 it != dirs.end(); ++it)
            {
                if (it->first == options.moduledir)
                {
                    std::cout << it->second << std::endl;
                }
            }
        }

        if (!options.moduledep.empty())
        {
            const std::set<std::string> deps =
                Safir::Utilities::Internal::ConfigHelper::GetDouDependencies(reader,options.moduledep);
            for (std::set<std::string>::const_iterator it = deps.begin();
                 it != deps.end(); ++it)
            {
                std::cout << *it << " ";
            }
            std::cout << std::endl;
        }

    }
    catch (const std::exception&e)
    {
        std::cout << "Can't read configuration. " << std::endl;
        std::cout << e.what() << std::endl;
        return 1;
    }
    return 0;
}
